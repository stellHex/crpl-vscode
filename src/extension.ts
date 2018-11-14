'use strict';
import * as vscode from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')
import {
  RichToken, ParseTree, ParseBranch, ParseMode,
  CRPLType, StackDelta, Stack, StackTracker,
  scrapeSignatures, sigSpec, signatures } from './helpers'

const crplSelector: vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }
const tokenPattern = /(?:<-|->|-\?|--|@|:|\$)[A-Za-z]\w*\b|-?\b\d+(?:\.\d*)?\b|(?:<-!|->!|-\?!|--\?)(?=\s|$)|\w+|\S/g
// note: this should be the same as language-configuration.json.wordPattern except with the addition of |\S
const symbolPatterns = new Map([
  [/<-[A-Za-z]\w*\b/, 'read'],
  [/->[A-Za-z]\w*\b/, 'write'],
  [/-\?[A-Za-z]\w*\b/, 'exists'],
  [/--[A-Za-z]\w*\b/, 'delete'],
  [/\$[A-Za-z]\w*\b/, 'define'],
  [/@[A-Za-z]\w*\b/, 'call'],
  [/:\w[A-Za-z]\w*\b/, 'func'],
  [/<-!/, 'refread'],
  [/->!/, 'refwrite'],
  [/-\?!/, 'refexists'],
  [/--\?/, 'refdelete'],
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
interface FuncTracker {
  func: RichToken[]
  call: RichToken[]
}
interface VarTracker {
  read: RichToken[], write: RichToken[],
  exists: RichToken[], delete: RichToken[],
  define: RichToken[]
}

let wom = (s: string) => ` without matching "${s}"`
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
      ['comment', 'endcomment'],
      ['"', '"']
    ]),
    unmatched: new Map<string, [string, 0|2]>([
      // number represents where the start of the block is
      // inside the ParseTree in relation to the error'd token
      // 0 = same token, eg ["once", [...]] -> "once" errors, its block starts at itself
      // 2 = different token, eg ["if", [...], "else", [...]] -> "else" errors, its block starts at "if"
      ['(', [')', 0]],
      ['do', ['loop', 0]],
      ['once', ['endonce', 0]], 
      ['if', ['endif', 0]],
      ['else', ['endif', 2]],
      ['while', ['repeat', 0]],
      ['repeat', ['endwhile', 2]],
      ['endcomment', ['comment', 0]],
      ['"', ['"', 0]]
    ]),
    unstarted: new Map<string, string>([
      [')', wom('(')],
      [':', ' without preceding "$" variable'], 
      ['value', ' without preceding "$" variable'],
      ['func', ': function definition inside another block'],
      ['loop', wom('do')],
      ['endonce', wom('once')],
      ['if', wom('endif')], ['else', wom('if')], ['endif', wom('if')],
      ['repeat', wom('while')],
      ['endwhile', wom('while') + ' or "repeat"'],
      ['"', wom('"')]
    ])
  }
  static startToken: RichToken = {
    token: '',
    range: <vscode.Range>{
      start: <vscode.Position>{line: 0, character: 0},
      end: <vscode.Position>{line: 0, character: 0}
    },
    meta: { wiki: false }
  }

  doc: vscode.TextDocument
  tokens: { flat: RichToken[], lines: RichToken[][], tree: ParseTree }
  funcs = new Map<string, FuncTracker>()
  vars = new Map<string, VarTracker>()
  constructor(docref: vscode.TextDocument | vscode.Uri) {
    if ((<vscode.TextDocument>docref).getText) {
      this.doc = <vscode.TextDocument>docref
    } else {
      this.doc = <vscode.TextDocument>vscode.workspace.textDocuments.find(doc => doc.uri === docref)
    }
    this.tokens = { flat: [], lines: [], tree: new ParseTree([RichDoc.startToken]) }
    this.tokenize()
  }

  tokenize() { try {
    let tokens: RichToken[] = this.tokens.flat = []
    let lines: RichToken[][] = this.tokens.lines = []
    let tree: ParseTree = this.tokens.tree = new ParseTree()
    this.funcs = new Map()
    this.vars = new Map()

    let spec = RichDoc.stackSpec
    let exeStack: RichToken[] = []
    let warpStack: RichToken[] = []
    let branch = tree
    let doc = this.doc
    let parametric = true
    let mode = ParseMode.normal

    function addToken(rToken: RichToken, line: number) {
      tokens.push(rToken)
      branch.push(rToken)
      lines[line].push(rToken)
    }

    function checkUnnest(rToken: RichToken) {
      let { id, token } = rToken
      if (id && spec.down.get(id) && id !== '"') {
        let top = exeStack.pop()
        let topid = top ? top.id : parametric ? 'start' : 'bottom'
        let matcher = spec.down.get(id)
        let matches = matcher instanceof RegExp ? (<RegExp>matcher).test(<string>topid) : matcher === topid
        if (matches) {
          branch = branch.parent || branch
        } else {
          let wrongness = spec.unstarted.get(id)
          rToken.error = rToken.error || `Unexpected token "${token}"${wrongness}.`
          if (top) { exeStack.push(top) }
        }
      }
    }
    function checkNest(rToken: RichToken) {
      let id = rToken.id
      if (id && spec.up.indexOf(id) > -1) {
        exeStack.push(rToken)
        let newBranch = new ParseTree()
        newBranch.start = rToken
        branch.push(newBranch)
        branch = newBranch
      }
    }

    function checkWarp(warpToken: RichToken, line: number) { // TODO
      if (warpToken.id === '(') {
        
      } else if (warpToken.id === ')') {

      }
    }

    function checkIJK(ijkToken: RichToken) {
      let depth = exeStack.filter(rToken => rToken.id === 'do').length
      if (ijkToken.id === 'i') {
        if (depth === 0) {
          ijkToken.error = '"I" must be used inside a "do" loop.'
        }
      } else if (ijkToken.id === 'j') {
        if (depth < 2) {
          ijkToken.error = '"J" must be used inside nested "do" loops.'
        }
      } else if (ijkToken.id === 'k') {
        if (depth === 0) {
          ijkToken.error = '"K" must be used inside "do" loops.'
        } else if (depth === 1) {
          ijkToken.warning = '"K" should\'t be used for an innermost "do" loop; use "I" instead.'
        } else if (depth === 2) {
          ijkToken.warning = '"K" should\'t be used for the second innermost "do" loop; use "J" instead.'
        }
      }
    }

    doc.getText().split('\n').forEach((text, line) => {
      lines.push([])
      // Here, replace is used as "for each match, do..."
      text.replace(tokenPattern, (token, offset) => {
        let range = new vscode.Range(line, offset, line, offset + token.length)

        if (mode === ParseMode.normal) {
          let id: string|undefined
          let wiki: boolean = true
          let error: string|undefined
          let rToken: RichToken = { token, error, range, meta: { wiki } }

          if (token === '#') { mode = ParseMode.comment; id = 'comment'; wiki = false } 
          else {
            if (parametric) {
              if (exeStack.length === 0) {
                if (token[0] === '$') {
                  id = 'define'
                  rToken.meta.var = token.slice(1)
                  this.getVar(rToken.meta.var).define.push(rToken)
                } else { parametric = false }
              } else if (exeStack.length === 1 && exeStack[0].id === 'define') {
                wiki = false
                if (token === ':') { id = ':' }
                else { error = 'Expected ":".'; parametric = false } 
              } else if (exeStack.length === 1 && exeStack[0].id === ':') {
                wiki = false
                if (RichDoc.valuePattern.test(token)) { id = 'value'; wiki = false }
                else { error = 'Expected numeric literal or string.'; parametric = false }
                if (token === '"') { mode = ParseMode.string }
              } else {
                parametric = false
              }
              if (error) {
                exeStack.pop()
              }
            }
            let lowerToken = token.toLowerCase()
            if (!parametric) {
              if (token[0] === '$') { id = 'define'; error = `Input variables must go at the start of the file.` }
              else if (token === '"') { mode = ParseMode.string; id = token; wiki = false }
              else if (token === '(' || token === ')') { id = token; wiki = false }
              else if (crplData.words.indexOf(lowerToken) > -1) { id = lowerToken }
              else if (crplData.unitConstants.hasOwnProperty(lowerToken)) { id = lowerToken }
              else if (/-?\d+(.\d*)?/.test(token)) { wiki = false }
              else {
                symbolPatterns.forEach((tokenType, re) => {
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
                error = id ? undefined : `Unknown token "${token}".`
                wiki = !!id
              }
            }
          }

          rToken.id = id; rToken.meta.wiki = wiki; rToken.error = error

          checkUnnest(rToken)
          addToken(rToken, line)
          checkWarp(rToken, line)
          checkNest(rToken)
          checkIJK(rToken)
        } else if (mode === ParseMode.string) {
          if (parametric && exeStack.length === 0) {
            // awful hacks in order to preserve a simple RichDoc.stackSpec schema
            let lastToken = <RichToken>branch.pop()
            lastToken.id = '"'
            exeStack.push(lastToken)
            let newBranch = new ParseTree()
            newBranch.start = lastToken
            branch.push(lastToken, newBranch)
            branch = newBranch
          }
          if (token === '"') {
            exeStack.pop()
            branch = branch.parent || branch
            addToken(<RichToken>{ token, range, id: '"', meta: {} }, line)
            mode = ParseMode.normal
          } else {
            addToken(<RichToken>{ token, range, id: 'string', meta: {} }, line)
          }
        } else if (mode === ParseMode.comment) {
          addToken(<RichToken>{ token, range, id: '#' + token, meta: {} }, line)
        }
        return ''
      })
      if (mode === ParseMode.comment) {
        exeStack.pop()
        branch = branch.parent || branch
        mode = ParseMode.normal
      }
    })
    // diagnose unresolved blocks
    exeStack.forEach(rToken => {
      if (rToken.id === 'func') {
        let wrongness = spec.unstarted.get(rToken.id)
        rToken.error = ((exeStack.length > 1) || undefined) && `Unexpected token "${rToken.token}"${wrongness}.`
        return
      }
      if (rToken.error) { return }
      rToken.parent = <ParseTree>rToken.parent
      let [ match, startOffset ] = <[string, 0|2]>spec.unmatched.get(<string>rToken.id)
      let errorToken = <RichToken>rToken.parent[rToken.parent.indexOf(rToken) - startOffset]
      if (errorToken.error) { return }
      errorToken.error = `"${errorToken.id}"` + wom(match)
    })
    this.checkVariables()
    this.checkFunctions()
    // this.printTokens()
  } catch (err) { console.log(err); throw err }}

  checkVariables() {
    this.vars.forEach((tracker, name) => {
      if (tracker.define.length > 1) {
        tracker.define.forEach(t => t.error = `Variable ${name} is defined more than once.`)
      }
      let writes = tracker.define.concat(tracker.write)
      let reads = tracker.read.concat(tracker.delete, tracker.exists)
      if (!writes.length) {
        reads.forEach(t => t.warning = `Variable ${name} is read from, but never written to.`)
      } else if (!reads.length) {
        writes.forEach(t => t.warning = `Variable ${name} is written to, but never read from.`)
      }
    })
  }
  checkFunctions() {
    this.funcs.forEach((tracker, name) => {
      if (tracker.func.length > 1) {
        tracker.func.forEach(t => t.error = `Function ${name} is defined more than once.`)
      }
      if (!tracker.func.length) {
        tracker.call.forEach(t => t.error = `Function ${name} is never defined.`)
      }
    });
  }

  getVar(name: string) {
    let result = this.vars.get(name)
    if (!result) {
      result = { read: [], write: [], exists: [], delete: [], define: [] }
      this.vars.set(name, result)
    }
    return <VarTracker>result
  }
  getFunc(name: string) {
    let result = this.funcs.get(name)
    if (!result) {
      result = { func: [], call: [] }
      this.funcs.set(name, result)
    }
    return <FuncTracker>result
  }
  
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
        .map(t => new vscode.Diagnostic(t.range, <string>t.error)),
      ...this.tokens.flat
        .filter(t => t.warning)
        .map(t => new vscode.Diagnostic(t.range, <string>t.warning, vscode.DiagnosticSeverity.Warning))
    )
    diagnostics.set(this.doc.uri, result)
  }

  printTokens(f: ((t: RichToken) => any) = t => t.token) {
    type StringBranch = StringTree | string
    class StringTree extends Array<StringBranch> {}
    function niceTree(branch: ParseBranch): StringBranch {
      if (Array.isArray(branch)) {
        return branch.map(niceTree)
      } else {
        return f(branch) ? f(branch).toString() : '???'
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
    if (matchedResult === null) { throw 400 }
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
    console.log(err)
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

export function activate(context: vscode.ExtensionContext) {
  if (false) { scrapeSignatures() }
  let documents: Map<string, RichDoc> = new Map
  let diagnostics = vscode.languages.createDiagnosticCollection('crpl')

  function updateDoc (doc: vscode.TextDocument) {
    if (vscode.languages.match(crplSelector, doc)) {
      let rDoc = documents.get(doc.uri.toString())
      if (!rDoc) {
        rDoc = new RichDoc(doc)
        documents.set(doc.uri.toString(), rDoc)
      }
      rDoc.tokenize()
      rDoc.diagnose(diagnostics)
    }
  }

  vscode.workspace.textDocuments.forEach(updateDoc)
  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(updateDoc),
    vscode.workspace.onDidChangeTextDocument(change => { updateDoc(change.document) }),
    vscode.workspace.onDidCloseTextDocument(doc => documents.delete(doc.uri.toString())),

    vscode.languages.registerHoverProvider(crplSelector, {
      async provideHover (document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.toString())
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
              console.log(err)
            }
          }
        }
      }
    }),
    vscode.languages.registerCompletionItemProvider(crplSelector, {
      async provideCompletionItems (document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.toString())
        let { wordRange, word, tokenMatch } = rDoc.getWord(position.with(undefined, position.character - 1))
        if (word && tokenMatch && (<vscode.Range>wordRange).end.isEqual(position) ) {
          let pattern: RegExp = /^/
          prefixPatterns.forEach(p => {
            if (p.test(<string>word)) { pattern = p }
          });
          let prefix = (<RegExpExecArray>pattern.exec(word))[0]
          let longlist = completionFilter(
            rDoc.tokens.flat.filter(rt => tokenMatch != rt).map(rt => rt.token),
            tokenMatch, pattern, prefix
          )
          longlist.push(...completionFilter(crplData.completionList, tokenMatch, pattern, prefix))
          let dupelog: string[] = []
          let result: vscode.CompletionItem[] = []
          longlist.forEach( (c, i) => {
            if (dupelog.indexOf(c.label) === -1) { dupelog.push(c.label); result.push(c) }
          })
          console.log(result.map(c => c.label).toString())
          return result
        }
      }
    }),
    vscode.languages.registerDefinitionProvider(crplSelector, {
      async provideDefinition(document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.toString())
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
    vscode.languages.registerReferenceProvider( crplSelector,  {
      async provideReferences(document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.toString())
        let { tokenMatch } = rDoc.getWord(position)
        if (tokenMatch && (tokenMatch.meta.var)) {
          let tracker = rDoc.getVar(tokenMatch.meta.var)
          return [...tracker.define, ...tracker.write, ...tracker.read, ...tracker.delete, ...tracker.exists]
            .map(rt => new vscode.Location(rDoc.doc.uri, rt.range))
        } else if (tokenMatch && tokenMatch.meta.func){
          let tracker = rDoc.getFunc(tokenMatch.meta.func)
          return [...tracker.func, ...tracker.call]
            .map(rt => new vscode.Location(rDoc.doc.uri, rt.range))
        }
      }
    }),
    // vscode.languages.registerRenameProvider,
    // vscode.languages.registerReferenceProvider
  )
}

export function deactivate() {
}