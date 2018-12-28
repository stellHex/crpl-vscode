'use strict';
import * as vscode from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')
import {
  RichToken, ParseTree, ParseBranch, ParseMode,
  VarTracker, FuncTracker,
  scrapeSignatures, // ParseChunk,
  surely, stackSpec} from './helpers'
import { getError, ParseContext, Linter } from './lint'

const documents = new Map<string, RichDoc>()
const diagnostics = vscode.languages.createDiagnosticCollection('crpl')

const crplSelector: vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }
const tokenPattern = /(?:(?:<-|->|-\?|--|@|\$)\w+|:[a-z]\w*)\b|-?\b\d+(?:\.\d*)?\b|(?:<-!|->!|-\?!|--\?)(?=\s|$)|\w+|\S/gi
// note: this should be the same as language-configuration.json.wordPattern except with the addition of |\S
const symbolPatterns = new Map([
  ['read', /^<-\w+\b$/],
  ['write', /^->\w+\b$/],
  ['exists', /^-\?\w+\b$/],
  ['delete', /^--\w+\b$/],
  ['define', /^\$\w+\b$/],
  ['call', /^@\w+\b$/],
  ['func', /^:\w+\b$/],
  ['refread', /^<-!$/],
  ['refwrite', /^->!$/],
  ['refexists', /^-\?!$/],
  ['refdelete', /^--\?$/],
])
const prefixPatterns = [
  /^(<-|->|--|-\?|\$)(?=\w)/,
  /^(:|@)(?=\w)/
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
  static stackSpec = stackSpec
  static startToken(): RichToken { return {
    token: '',
    id: 'start',
    range: <vscode.Range>{
      start: <vscode.Position>{line: 0, character: 0},
      end: <vscode.Position>{line: 0, character: 0}
    },
    meta: {}
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
      doc: this,
      notLinting: false,
      disabledLints: new Set(),
      tokens, lines, tree,
      branch: tree,
      parametric: true,
      mode: ParseMode.normal,
      line: -1,
      exeStack: [],
      warpStack: []
    }
    
    const lint = new Linter(ctx)

    function addToken(rToken: RichToken, line: number) {
      tokens.push(rToken)
      ctx.branch.push(rToken)
      lines[line].push(rToken)
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
                  rToken.meta.notLinting = ctx.notLinting
                  rToken.meta.disabledLints = new Set(ctx.disabledLints)
                  this.getVar(rToken.meta.var).define.push(rToken)
                } else { ctx.parametric = false }
              } else if (ctx.exeStack.length === 1 && ctx.exeStack[0].id === 'define') {
                wiki = false
                if (token === ':') { id = ':' }
                else { error = lint.getError('define', ':'); ctx.parametric = false } 
              } else if (ctx.exeStack.length === 1 && ctx.exeStack[0].id === ':') {
                wiki = false
                if (RichDoc.valuePattern.test(token)) { id = 'value'; wiki = false }
                else { error = lint.getError('define', 'value'); ctx.parametric = false }
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
              if (token[0] === '$') {
                id = 'define'
                // need to include these here since checkVariables takes place later
                rToken.meta.notLinting = ctx.notLinting
                rToken.meta.disabledLints = new Set(ctx.disabledLints)
                error = lint.getError('definestart')
              }
              else if (token === '"') { ctx.mode = ParseMode.string; id = token; wiki = false }
              else if (token === '(' || token === ')') { id = token; wiki = false }
              else if (crplData.words.indexOf(lowerToken) > -1) { id = lowerToken }
              else if (crplData.unitConstants.hasOwnProperty(lowerToken)) { id = lowerToken }
              else if (/^-?\d+(.\d*)?$/.test(token)) { wiki = false }
              else {
                symbolPatterns.forEach((re, tokenType) => {
                  if (re.test(token)) {
                    id = tokenType
                    // need to include these here since checkVariables and checkFunctions take place later
                    rToken.meta.notLinting = ctx.notLinting
                    rToken.meta.disabledLints = new Set(ctx.disabledLints) 
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
                error = id ? undefined : lint.getError('unknown')
                wiki = !!id
              }
            }
          }

          rToken.id = id; rToken.meta.wiki = wiki; rToken.error = error

          lint.checkUnnest(rToken)
          lint.checkWarp(rToken)
          addToken(rToken, line)
          lint.checkUnwarp(rToken)
          lint.checkNest(rToken)

          lint.checkIJK(rToken)
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
          let rToken = { token, range, id: '#' + token, meta: {} }
          lint.checkDirectives(rToken)
          addToken(rToken, line)
        }
        // this.printTokens()

        return ''
      })
      if (ctx.mode === ParseMode.comment) {
        ctx.exeStack.pop()
        ctx.branch = ctx.branch.parent || ctx.branch
        let rToken = {
          token: '', id: 'endcomment', meta: { },
          range: new vscode.Range(line, text.length, line, text.length)
        }
        lint.checkDirectives(rToken)
        addToken(rToken, line)
        ctx.mode = ParseMode.normal
      }
    })
    // diagnose unresolved blocks
    ctx.exeStack.forEach(rToken => {
      if (rToken.id === 'func') {
        rToken.error = ((ctx.exeStack.length > 1) || undefined) && lint.getError('enddelim', 'func')
        return
      }
      if (rToken.error) { return }
      rToken.parent = <ParseTree>rToken.parent
      let startOffset = <0|2>spec.unmatched.get(<string>rToken.id)
      let errorToken = <RichToken>rToken.parent[rToken.parent.indexOf(rToken) - startOffset]
      if (errorToken.error) { return }
      errorToken.error = lint.getError('startdelim', rToken.id, errorToken)
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
        reads.forEach(rT => rT.warning = getError('unwritten', undefined, rT))
      } else if (!reads.length) {
        writes.forEach(rT => rT.warning = getError('unread', undefined, rT))
      }
    })
  }
  checkFunctions() {
    this.funcs.forEach((tracker, name) => {
      if (tracker.func.length > 1) {
        tracker.func.forEach(rT => rT.error = getError('multidef', undefined, rT))
      } else if (!tracker.func.length) {
        tracker.call.forEach(rT => rT.error = getError('nodef', undefined, rT))
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
        .filter(rT => {
          return rT.error && !rT.meta.notLinting
            && !(rT.meta.disabledLints && rT.meta.disabledLints.has(rT.error.code))
        })
        .map(rT => {
          let error = surely(rT.error)
          let diagnostic = new vscode.Diagnostic(rT.range, error.message)
          diagnostic.code = error.code
          diagnostic.source = 'crpl'
          return diagnostic
        }),
      ...this.tokens.flat
        .filter(rT => {
          return rT.warning && !rT.meta.notLinting
            && !(rT.meta.disabledLints && rT.meta.disabledLints.has(rT.warning.code))
        })
        .map(rT => {
          let warning = surely(rT.warning)
          let diagnostic = new vscode.Diagnostic(rT.range, warning.message, vscode.DiagnosticSeverity.Warning )
          diagnostic.code = warning.code
          diagnostic.source = 'crpl'
          return diagnostic
        })
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
        return f(branch) ? f(branch) : branch
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