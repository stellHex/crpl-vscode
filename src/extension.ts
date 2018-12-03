'use strict';
import * as vscode from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')
import {
  RichToken, ParseTree, ParseBranch, ParseMode,
  VarTracker, FuncTracker,
  scrapeSignatures, ParseChunk,
  surely } from './helpers'
import { getError, ParseContext } from './lint'

const documents = new Map<string, RichDoc>()
const diagnostics = vscode.languages.createDiagnosticCollection('crpl')

const crplSelector: vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }
const tokenPattern = /(?:<-|->|-\?|--|@|:|\$)[A-Za-z]\w*\b|-?\b\d+(?:\.\d*)?\b|(?:<-!|->!|-\?!|--\?)(?=\s|$)|\w+|\S/g
// note: this should be the same as language-configuration.json.wordPattern except with the addition of |\S
const symbolPatterns = new Map([
  ['read', /<-[A-Za-z]\w*\b/],
  ['write', /->[A-Za-z]\w*\b/],
  ['exists', /-\?[A-Za-z]\w*\b/],
  ['delete', /--[A-Za-z]\w*\b/],
  ['define', /\$[A-Za-z]\w*\b/],
  ['call', /@[A-Za-z]\w*\b/],
  ['func', /:\w[A-Za-z]\w*\b/],
  ['refread', /<-!/],
  ['refwrite', /->!/],
  ['refexists', /-\?!/],
  ['refdelete', /--\?/],
])
const prefixPatterns = [
  /^(<-|->|--|-\?|\$)(?=[A-Za-z])/,
  /^(:|@)(?=[A-Za-z])/
]
const docPattern = /=====\s*(.*)\s*=====\s*(.*\s*)*?.*Arguments.*\^\s*\|\s*(.*?)\s*\|\s*(.*?)\s*\|(.*?)\|(.*\s*)*?===\s*Description.*\s*((.*\s*)*)/

interface WordInTheHand {
  wordRange?: vscode.Range,
  word?: string,
  tokenMatch?: RichToken
}

class RichDoc {
  static valuePattern = /"|-?\d+(\.\d*)?/
  static keyofVarTracker = /^(read|write|exists|delete)$/
  static keyofFuncTracker = /^(func|call)$/
  static stackSpec = {
    up: ['(', 'define', ':', 'func', 'do', 'once', 'if', 'else', 'while', 'repeat', 'comment', '"'],
    down: new Map<string, string|RegExp>([
      [')', '('],
      ['define', 'start'], [':', 'define'], ['value', ':'],
      ['func', /^(func|start|bottom)$/],
      ['loop', 'do'],
      ['endonce', 'once'],
      ['else', 'if'],
      ['endif', /^(if|else)$/],
      ['repeat', 'while'],
      ['endwhile', 'repeat'],
      ['endcomment', 'comment'],
      ['"', '"']
    ]),
    unmatched: new Map<string, 0|2>([
      // number represents where the start of the block is
      // inside the ParseTree in relation to the error'd token
      // 0 = same token, eg ["once", [...]] -> "once" errors, its block starts at itself
      // 2 = different token, eg ["if", [...], "else", [...]] -> "else" errors, its block starts at "if"
      ['(', 0],
      ['do', 0],
      ['once', 0], 
      ['if', 0],
      ['else', 2],
      ['while', 0],
      ['repeat', 2],
      ['comment', 0],
      ['"', 0]
    ]),
  }
  static startToken(): RichToken { return {
    token: '',
    range: <vscode.Range>{
      start: <vscode.Position>{line: 0, character: 0},
      end: <vscode.Position>{line: 0, character: 0}
    },
    meta: { }
  }}

  public doc: vscode.TextDocument
  readonly tokens: { flat: RichToken[], lines: RichToken[][], tree: ParseTree }
  // tokens.flat and tokens.tree should be in warped order, tokens.lines can't be
  readonly funcs = new Map<string, FuncTracker>()
  readonly vars = new Map<string, VarTracker>()

  constructor(docref: vscode.TextDocument | vscode.Uri) {
    if ((<vscode.TextDocument>docref).getText) {
      this.doc = <vscode.TextDocument>docref
    } else {
      this.doc = <vscode.TextDocument>vscode.workspace.textDocuments.find(doc => doc.uri === docref)
    }
    this.tokens = { flat: [], lines: [], tree: new ParseTree([RichDoc.startToken()]) }
  }

  tokenize() { try {
    let tokens: RichToken[] = this.tokens.flat = []
    let lines: RichToken[][] = this.tokens.lines = []
    let tree: ParseTree = this.tokens.tree = new ParseTree([RichDoc.startToken()])
    this.vars.clear()
    this.funcs.clear()


    const spec = RichDoc.stackSpec
    const doc = this.doc

    const ctx: ParseContext = {
      linting: true,
      disabledLints: new Set(),
      branch: tree,
      parametric: true,
      mode: ParseMode.normal,
      line: -1,
      exeStack: [],
      warpStack: []
    }

    function addToken(rToken: RichToken, line: number) {
      tokens.push(rToken)
      ctx.branch.push(rToken)
      lines[line].push(rToken)
    }

    function checkUnnest(rToken: RichToken) {
      let id = rToken.id
      if (id && spec.down.get(id) && id !== '"') {
        let top = rToken.meta.blockPredecessor || ctx.exeStack.pop()
        let topid = top ? top.id : ctx.parametric ? 'start' : 'bottom'
        let matcher = spec.down.get(id)
        let matches = matcher instanceof RegExp ? matcher.test(<string>topid) : matcher === topid
        if (matches) {
          ctx.branch = ctx.branch.parent || ctx.branch
          rToken.meta.blockPredecessor = top
          if (top) { top.meta.blockSuccessor = rToken }
        } else {
          rToken.error = rToken.error || getError('bad-end-delim', id, rToken)
          if (top) { ctx.exeStack.push(top) }
        }
      }
    }
    function checkNest(rToken: RichToken) {
      let id = rToken.id
      if (id && spec.up.indexOf(id) > -1) {
        ctx.exeStack.push(rToken)
        let newBranch = new ParseTree()
        newBranch.start = rToken
        ctx.branch.push(newBranch)
        ctx.branch = newBranch
      }
    }

    function checkWarp(warpToken: RichToken) {
      if (warpToken.id === '(') {
        let wsMember: ParseChunk[] = []
        warp()
        function pop() {
          let result = ctx.branch.pop()
          if (Array.isArray(result)) {
            tokens.splice(tokens.lastIndexOf(<RichToken>(<ParseTree>result)[0]))
          } else {
            tokens.pop()
          }
          return result
        }
        function warp() {
          if (ctx.branch.length) {
            let prevToken = <RichToken>pop()
            if (prevToken.id === 'endcomment') { // ignore comments
              let body = <ParseTree>pop(), start = <RichToken>pop() 
              wsMember.push([start, body, prevToken])
              warp()
            } else if (prevToken.id === '"') { // treat whole strings as one thing
              let body = <ParseTree>pop(), start = <RichToken>pop()
              warpFin([start, body, prevToken])
            } else {
              if(Array.isArray(ctx.branch[ctx.branch.length-1])){
                ctx.branch = <ParseTree>ctx.branch[ctx.branch.length-1]
              }
              warpFin(prevToken)
            }
          } else {
            ctx.branch = ctx.branch.parent || ctx.branch
            warpFin(<RichToken>pop())
          }
        }
        function warpFin(prev: ParseChunk) {
          let realMember = wsMember.length ? wsMember : prev
          wsMember.push(prev)
          if (ctx.branch === tree && (
            ctx.branch[ctx.branch.length-2] && (<RichToken>ctx.branch[ctx.branch.length-2]).id === ':'
            || prev === tree[0])
          ) {
            warpToken.error = getError('warp-start')
            unwarp(realMember)
          } else {
            ctx.warpStack.push(realMember)
          }
        }
      }
    }
    function checkUnwarp(warpToken: RichToken) {
      if (warpToken.id === ')') {
        if (ctx.warpStack.length) {
          let toUnwarp = <ParseChunk|ParseChunk[]>ctx.warpStack.pop()
          unwarp(toUnwarp)
        } else {
          warpToken.error = getError('warp-start')
        }
      }
    }
    function unwarp(wsMember: ParseChunk|ParseChunk[]) {
      if (Array.isArray(wsMember)) {
        if (wsMember[1] instanceof ParseTree) {
          (<ParseBranch[]>wsMember).forEach(push)
        } else {
          while (wsMember.length) {
            unwarp(<ParseChunk|ParseChunk[]>wsMember.pop())
          }
        }
      } else {
        checkUnnest(wsMember)
        push(wsMember)
        checkNest(wsMember)
      }
      function push (b: ParseBranch) { tokens.push(...flatten(b)); ctx.branch.push(b) }
      function flatten(br: ParseBranch) {
        if (Array.isArray(br)) {
          let result: RichToken[] = []
          br.forEach(b => result.push(...flatten(b)))
          return result
        } else { return [br] }
      }
    }

    function checkIJK(ijkToken: RichToken) {
      let depth = ctx.exeStack.filter(rToken => rToken.id === 'do').length
      if (ijkToken.id === 'i') {
        if (depth === 0) {
          ijkToken.error = getError('ijk', 'i')
        }
      } else if (ijkToken.id === 'j') {
        if (depth < 2) {
          ijkToken.error = getError('ijk', 'j')
        }
      } else if (ijkToken.id === 'k') {
        if (depth === 0) {
          ijkToken.error = getError('ijk', 'k')
        } else if (depth === 1) {
          ijkToken.error = getError('ijk', 'ik')
        } else if (depth === 2) {
          ijkToken.error = getError('ijk', 'jk')
        }
      }
    }

    doc.getText().split('\n').forEach((text, line) => {
      lines.push([])
      // Here, replace is used as "for each match, do..."
      ctx.line = line
      text.replace(tokenPattern, (token, offset) => {
        let range = new vscode.Range(line, offset, line, offset + token.length)

        if (ctx.mode === ParseMode.normal) {
          let id: string|undefined
          let wiki: boolean = true
          let error: typeof rToken.error
          let rToken: RichToken = { token, error, range, meta: { wiki } }

          if (token === '#') { ctx.mode = ParseMode.comment; id = 'comment'; wiki = false } 
          else {
            if (ctx.parametric) {
              if (ctx.exeStack.length === 0) {
                if (token[0] === '$') {
                  id = 'define'
                  rToken.meta.var = token.toLowerCase().slice(1)
                  this.getVar(rToken.meta.var).define.push(rToken)
                } else { ctx.parametric = false }
              } else if (ctx.exeStack.length === 1 && ctx.exeStack[0].id === 'define') {
                wiki = false
                if (token === ':') { id = ':' }
                else { error = getError('define', ':'); ctx.parametric = false } 
              } else if (ctx.exeStack.length === 1 && ctx.exeStack[0].id === ':') {
                wiki = false
                if (RichDoc.valuePattern.test(token)) { id = 'value'; wiki = false }
                else { error = getError('define', 'value'); ctx.parametric = false }
                if (token === '"') { ctx.mode = ParseMode.string }
              } else {
                ctx.parametric = false
              }
              if (error) {
                ctx.exeStack.pop()
              }
            }
            let lowerToken = token.toLowerCase()
            if (!ctx.parametric) {
              if (token[0] === '$') { id = 'define'; error = getError('define-start') }
              else if (token === '"') { ctx.mode = ParseMode.string; id = token; wiki = false }
              else if (token === '(' || token === ')') { id = token; wiki = false }
              else if (crplData.words.indexOf(lowerToken) > -1) { id = lowerToken }
              else if (crplData.unitConstants.hasOwnProperty(lowerToken)) { id = lowerToken }
              else if (/-?\d+(.\d*)?/.test(token)) { wiki = false }
              else {
                symbolPatterns.forEach((re, tokenType) => {
                  if (re.test(token)) {
                    id = tokenType
                    if (RichDoc.keyofVarTracker.test(id)) {
                      let name = lowerToken.slice(2)
                      rToken.meta.var = name
                      this.getVar(name)[<keyof VarTracker>tokenType].push(rToken)
                    } else if (RichDoc.keyofFuncTracker.test(id)) {
                      let name = lowerToken.slice(1)
                      rToken.meta.func = name
                      this.getFunc(name)[<keyof FuncTracker>tokenType].push(rToken)
                    }
                  }
                })
                error = id ? undefined : getError('unknown')
                wiki = !!id
              }
            }
          }

          rToken.id = id; rToken.meta.wiki = wiki; rToken.error = error

          checkUnnest(rToken)
          checkWarp(rToken)
          addToken(rToken, line)
          checkUnwarp(rToken)
          checkNest(rToken)

          checkIJK(rToken)
        } else if (ctx.mode === ParseMode.string) {
          if (ctx.parametric && ctx.exeStack.length === 0) {
            // awful hacks in order to preserve a simple RichDoc.stackSpec schema
            let lastToken = <RichToken>ctx.branch.pop()
            lastToken.id = '"'
            ctx.exeStack.push(lastToken)
            let newBranch = new ParseTree()
            newBranch.start = lastToken
            ctx.branch.push(lastToken, newBranch)
            ctx.branch = newBranch
          }
          if (token === '"') {
            ctx.exeStack.pop()
            ctx.branch = ctx.branch.parent || ctx.branch
            addToken({ token, range, id: '"', meta: { } }, line)
            ctx.mode = ParseMode.normal
          } else {
            addToken({ token, range, id: 'string', meta: { } }, line)
          }
        } else if (ctx.mode === ParseMode.comment) {
          addToken({ token, range, id: '#' + token, meta: { } }, line)
        }
        // this.printTokens()

        return ''
      })
      if (ctx.mode === ParseMode.comment) {
        ctx.exeStack.pop()
        ctx.branch = ctx.branch.parent || ctx.branch
        addToken({
          token: '', id: 'endcomment', meta: { },
          range: new vscode.Range(line, text.length, line, text.length)
        }, line)
        ctx.mode = ParseMode.normal
      }
    })
    // diagnose unresolved blocks
    ctx.exeStack.forEach(rToken => {
      if (rToken.id === 'func') {
        rToken.error = ((ctx.exeStack.length > 1) || undefined) && getError('end-delim', 'func')
        return
      }
      if (rToken.error) { return }
      rToken.parent = <ParseTree>rToken.parent
      let startOffset = <0|2>spec.unmatched.get(<string>rToken.id)
      let errorToken = <RichToken>rToken.parent[rToken.parent.indexOf(rToken) - startOffset]
      if (errorToken.error) { return }
      errorToken.error = getError('start-delim', rToken.id, errorToken)
    })
    this.checkVariables()
    this.checkFunctions()
    this.printTokens()
  } catch (err) { console.error(err); throw err }}

  checkVariables() {
    this.vars.forEach((tracker, name) => {
      // let shorten = (l: RichToken[]) => l.map((rt) => rt.token)
      // console.log(name, shorten(tracker.define), shorten(tracker.write), shorten(tracker.read))
      // console.log('')
      if (tracker.declare.length) { return }
      if (tracker.define.length > 1) {
        tracker.define.forEach(t => t.error = getError(''))
      }
      let writes = tracker.define.concat(tracker.write)
      let reads = tracker.read.concat(tracker.delete, tracker.exists)
      if (!writes.length) {
        reads.forEach(rT => rT.warning = getError('no-write', undefined, rT))
      } else if (!reads.length) {
        writes.forEach(rT => rT.warning = getError('no-read', undefined, rT))
      }
    })
  }
  checkFunctions() {
    this.funcs.forEach((tracker, name) => {
      if (tracker.func.length > 1) {
        tracker.func.forEach(rT => rT.error = getError('multi-def', undefined, rT))
      } else if (!tracker.func.length) {
        tracker.call.forEach(rT => rT.error = getError('no-def', undefined, rT))
      }
    })
  }

  getVar(name: string) {
    let result = this.vars.get(name)
    if (!result) {
      result = { read: [], write: [], exists: [], delete: [], define: [], declare: [] }
      this.vars.set(name, result)
    }
    return surely(result)
  }
  getFunc(name: string) {
    let result = this.funcs.get(name)
    if (!result) {
      result = { func: [], call: [] }
      this.funcs.set(name, result)
    }
    return surely(result)
  }

  name () { return this.doc.uri.fsPath.replace(/.*[\\/]/, '') }
  dir () { return this.doc.uri.fsPath.replace(/([\\/])[^\\/]*$/, '$1') }
  updateDoc () { updateDoc(this.doc) }

  getWord(position: vscode.Position): WordInTheHand {
    let wordRange = this.doc.getWordRangeAtPosition(position, tokenPattern)
    let word = wordRange && this.doc.getText(wordRange)
    let tokenMatch = wordRange && this.tokens.flat.find((rToken) => {
      let intersection = (<vscode.Range>wordRange).intersection(rToken.range)
      return !!(intersection && !intersection.isEmpty)
    })
    return { wordRange, word, tokenMatch }
  }

  diagnose(diagnostics: vscode.DiagnosticCollection) {
    let result: vscode.Diagnostic[] = []
    result.push(
      ...this.tokens.flat
        .filter(t => t.error)
        .map(t => {
          let error = surely(t.error)
          let diagnostic = new vscode.Diagnostic(t.range, error.message)
          diagnostic.code = error.code
          return diagnostic
        }),
      ...this.tokens.flat
        .filter(t => t.warning)
        .map(t => {
          let warning = surely(t.warning)
          let diagnostic = new vscode.Diagnostic(t.range, warning.message, vscode.DiagnosticSeverity.Warning )
          diagnostic.code = warning.code
          return diagnostic}
        )
    )
    diagnostics.set(this.doc.uri, result)
  }

  printTokens(f: ((t: RichToken) => any) = t => t.token || (t.id ? `<${t.id}>` : t)) {
    type StringBranch = StringTree | string
    class StringTree extends Array<StringBranch> {}
    function niceTree(branch: ParseBranch): StringBranch {
      if (Array.isArray(branch)) {
        return branch.map(niceTree)
      } else {
        return f(branch) ? f(branch).toString() : branch
      }
    }
    console.log({
      lines: this.tokens.lines.map(line => line.map(rToken => rToken.token).join(', ')),
      tree: this.tokens.tree.map(niceTree)
    })
  }
}

function docuWikiDocToMD (crplHTML: string): string[] {
  try {
    // tidy up
    let docu = crplHTML.replace(/\r/g, '')
    docu = (<RegExpExecArray>/=====(.*\n)*/.exec(docu))[0];
    docu = docu.replace(/(===+\s*Examples|<\/textarea)(.*\n)*/, '');
    docu = docu.replace(/''|%%/g, '`');
    docu = entityDecode(docu)

    let splitKey = `~~~ ${Math.random()} ~~~`
    let matchedResult = docPattern.exec(docu)
    if (matchedResult === null) { throw new Error('400') }
    let [, id,, args, results, notation,, description] = matchedResult
    description = description.replace(/==+ *(.*?) *==+/g, `\n${splitKey}\n**$1**\n`)
      .replace(/\[\[.*\|(.*)\]\]/g, '$1')
      .replace(/<\/?note>/g, '\n')
      .replace(/<code>/g, '```crpl\n')
      .replace(/<\/code>/g, '\n```')
    // replace docuWiki table with MD table
    let newDescription = description.replace(/\^(.*)\n/g, '|$1\n|\n');
    while (newDescription !== description) {
      description = newDescription;
      newDescription = description.replace(/\^(.*)\n/, '|$1\n|---');
    }

    return [
      `\`${id}\`: ${notation}`,
      `[${args}] - [${results}]`,
      ...description.split(splitKey)
    ]
  } catch (err) {
    console.error(err)
    throw err
  }
}

function completionFilter(completionList: string[], rToken: RichToken, pattern: RegExp, prefix: string) {
  let word = rToken.token

  let niceWord = word.toLowerCase().replace(pattern, '')
  let result = completionList
    .filter(s => pattern.test(s))
    .map(s => s.replace(pattern, ''))
    .filter(completion => completion.toLowerCase().startsWith(niceWord))
    .map(s => prefix + s)
  return result.map(c => <vscode.CompletionItem>{
    label: c, range: rToken.range
  })
}

function updateDoc (doc: vscode.TextDocument) {
  if (vscode.languages.match(crplSelector, doc)) {
    let rDoc = documents.get(doc.uri.fsPath)
    if (!rDoc) {
      rDoc = new RichDoc(doc)
      documents.set(doc.uri.fsPath, rDoc)
    } else if (rDoc.doc.isClosed) {
      rDoc.doc = doc
    }

    rDoc.tokenize()
    rDoc.diagnose(diagnostics)
  }
}

export function activate(context: vscode.ExtensionContext) {
  if (false) { scrapeSignatures() }

  vscode.workspace.textDocuments.forEach(updateDoc)
  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(updateDoc),
    vscode.workspace.onDidChangeTextDocument(change => { updateDoc(change.document) }),
    vscode.workspace.onDidCloseTextDocument(doc => documents.delete(doc.uri.fsPath)),

    vscode.languages.registerHoverProvider(crplSelector, {
      async provideHover (document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.fsPath)
        let { wordRange, word, tokenMatch } = rDoc.getWord(position)
        if (word) {
          try {
            if (tokenMatch && tokenMatch.meta.wiki && !tokenMatch.error && tokenMatch.id !== undefined) {
              if (tokenMatch.id.startsWith('const_')) {
                return new vscode.Hover(`\`${tokenMatch.id.toUpperCase()}\`: ${(crplData.unitConstants as any)[tokenMatch.id]}`, wordRange)
              }
              let docs = await request(crplData.prefix + tokenMatch.id + crplData.suffix)
              return new vscode.Hover(docuWikiDocToMD(docs), wordRange)
            } else {
              return undefined
            }
          } catch(err) {
            if (err === 400) {
              return new vscode.Hover(`Unable to parse response from ${crplData.prefix + (<RichToken>tokenMatch).id + crplData.suffix}.`)
            } else {
              console.error(err)
            }
          }
        }
      }
    }),
    vscode.languages.registerCompletionItemProvider(crplSelector, {
      async provideCompletionItems (document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.fsPath)
        let { wordRange, word, tokenMatch } = rDoc.getWord(position.with(undefined, position.character - 1))
        if (word && tokenMatch && (<vscode.Range>wordRange).end.isEqual(position) ) {
          let pattern: RegExp = /^/
          prefixPatterns.forEach(p => {
            if (p.test(<string>word)) { pattern = p }
          });
          let prefix = (<RegExpExecArray>pattern.exec(word))[0]
          let longlist = completionFilter(
            rDoc.tokens.flat.filter(rt => tokenMatch !== rt).map(rt => rt.token),
            tokenMatch, pattern, prefix
          )
          longlist.push(...completionFilter(crplData.completionList, tokenMatch, pattern, prefix))
          let dupelog: string[] = []
          let result: vscode.CompletionItem[] = []
          longlist.forEach( (c, i) => {
            if (dupelog.indexOf(c.label) === -1) { dupelog.push(c.label); result.push(c) }
          })
          return result
        }
      }
    }),
    vscode.languages.registerDefinitionProvider(crplSelector, {
      async provideDefinition(document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.fsPath)
        let { tokenMatch } = rDoc.getWord(position)
        if (tokenMatch && (tokenMatch.meta.var)) {
          let tracker = rDoc.getVar(tokenMatch.meta.var)
          let definition = tracker.define[0] || tracker.write[0]
          if (definition) {
            return new vscode.Location(rDoc.doc.uri, definition.range)
          }
        } else if (tokenMatch && tokenMatch.meta.func){
          let definition = rDoc.getFunc(tokenMatch.meta.func).func[0]
          if (definition) {
            return new vscode.Location(rDoc.doc.uri, definition.range)
          }
        }
      }
    }),
    vscode.languages.registerReferenceProvider(crplSelector,  {
      async provideReferences(document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.fsPath)
        let { tokenMatch } = rDoc.getWord(position)
        if (tokenMatch && (tokenMatch.meta.var)) {
          let tracker = rDoc.getVar(tokenMatch.meta.var)
          return [...tracker.define, ...tracker.write, ...tracker.read, ...tracker.delete, ...tracker.exists, ...tracker.declare]
            .map(rt => new vscode.Location(rDoc.doc.uri, rt.range))
        } else if (tokenMatch && tokenMatch.meta.func){
          let tracker = rDoc.getFunc(tokenMatch.meta.func)
          return [...tracker.func, ...tracker.call]
            .map(rt => new vscode.Location(rDoc.doc.uri, rt.range))
        }
      }
    }),
    vscode.languages.registerRenameProvider(crplSelector, {
      async prepareRename(document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.fsPath)
        let { tokenMatch } = rDoc.getWord(position)
        let name = tokenMatch && (tokenMatch.meta.var || tokenMatch.meta.func)
        if (name) {
          return {
            range: (<RichToken>tokenMatch).range,
            placeholder: name
          }
        }
      },
      async provideRenameEdits(document, position, newName, token) {
        let rDoc = <RichDoc>documents.get(document.uri.fsPath)
        let { tokenMatch } = rDoc.getWord(position)
        let edit = new vscode.WorkspaceEdit()
        if (tokenMatch && tokenMatch.meta.var) {
          let name = tokenMatch.meta.var
          let tracker = rDoc.getVar(name);
          [...tracker.define, ...tracker.write, ...tracker.read, ...tracker.delete, ...tracker.exists]
            .forEach(rt => edit.replace(rDoc.doc.uri, rt.range, rt.token.replace(name, newName)))
          return edit
        } else if (tokenMatch && tokenMatch.meta.func){
          let name = tokenMatch.meta.func
          let tracker = rDoc.getFunc(name);
          [...tracker.func, ...tracker.call]
            .forEach(rt => edit.replace(rDoc.doc.uri, rt.range, rt.token.replace(name, newName)))
          return edit
        }
      }
    }),
  )
}

export function deactivate() {
}