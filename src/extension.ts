'use strict';
import * as vscode from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')

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
  /^(:|)(?=[A-Za-z])/
]
const docPattern = /=====\s*(.*)\s*=====\s*(.*\s*)*?.*Arguments.*\^\s*\|\s*(.*?)\s*\|\s*(.*?)\s*\|(.*?)\|(.*\s*)*?===\s*Description.*\s*((.*\s*)*)/

enum crplType {
  b = 0b0001,
  i = 0b0111,
  f = 0b0101,
  s = 0b0001,
  l = 0b1001,
  n = 0b0001,
  o = 0b0000,
}

const sigSpec = {
  convertible (t1: crplType, t2: crplType) { return (t1 & t2) === t2 },
  inTypes: new Map([
    ['b', crplType.b], ['i', crplType.i], ['f', crplType.f],
    ['x', crplType.f], ['y', crplType.f], ['z', crplType.f],
    ['s', crplType.s],
    ['l', crplType.l],
    ['n', crplType.n]
  ]),
  outTypes: new Map([
    ['b', crplType.i], ['i', crplType.i], ['f', crplType.f],
    ['x', crplType.i], ['y', crplType.i], ['z', crplType.f],
    ['s', crplType.s],
    ['l', crplType.l],
    ['n', crplType.n]
  ])
}

interface RichToken {
  token: string
  range: vscode.Range
  id: string | undefined
  error: string | false
  wiki: boolean
  parent: ParseTree | undefined
}

interface WordInTheHand {
  wordRange: vscode.Range | undefined,
  word: string | undefined,
  tokenMatch: RichToken | undefined
}

type ParseBranch = ParseTree | RichToken
class ParseTree extends Array<ParseBranch> {
  parent: ParseTree | undefined
  push (...branches: ParseBranch[]) {
    branches.forEach(branch => branch.parent = this)
    return super.push(...branches)
  }
}

enum ParseMode {
  normal, string, comment
}

let wom = (s: string) => ` without matching "${s}"`
class RichDoc {
  static valuePattern = /"|-?\d+(\.\d*)?/
  static stackSpec = {
    up: ['(', 'define', ':', 'func', 'do', 'once', 'if', 'else', 'while', 'repeat'],
    down: new Map<string, string|RegExp>([
      [')', '('],
      ['define', 'start'], [':', 'define'], ['value', ':'],
      ['func', /^(func|start|bottom)$/],
      ['loop', 'do'],
      ['endonce', 'once'],
      ['else', 'if'],
      ['endif', /^(if|else)$/],
      ['repeat', 'while'],
      ['endwhile', 'repeat']
    ]),
    unmatched: new Map<string, [string, 0|2]>([
      // number represents where the start of the block is
      // inside the ParseTree in relation to the error'd token
      // 0 = same token, eg [once, [...]]
      // 2 = different token, eg [if, [...], else [...]]
      ['(', [')', 0]],
      ['do', ['loop', 0]],
      ['once', ['endonce', 0]], 
      ['if', ['endif', 0]],
      ['else', ['endif', 2]],
      ['while', ['repeat', 0]],
      ['repeat', ['endwhile', 2]]
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
      ['endwhile', wom('while') + ' or "repeat"']
    ])
  }
  static startToken = <RichToken>{
    token: '',
    range: <vscode.Range>{
      start: <vscode.Position>{line: 0, character: 0},
      end: <vscode.Position>{line: 0, character: 0}
    },
    error: false,
    wiki: false
  }

  doc: vscode.TextDocument
  tokens
    : { flat: RichToken[], lines: RichToken[][], tree: ParseTree }
    = { flat: [], lines: [], tree: <ParseTree>[RichDoc.startToken]}
  constructor(docref: vscode.TextDocument | vscode.Uri) {
    if ((<vscode.TextDocument>docref).getText) {
      this.doc = <vscode.TextDocument>docref
    } else {
      this.doc = <vscode.TextDocument>vscode.workspace.textDocuments.find(doc => doc.uri === docref)
    }
    this.tokenize()
  }

  tokenize() { try {
    let tokens: RichToken[] = this.tokens.flat = []
    let lines: RichToken[][] = this.tokens.lines = []
    let tree: ParseTree = this.tokens.tree = new ParseTree()

    let spec = RichDoc.stackSpec
    let exeStack: RichToken[] = []
    let branch = tree
    let doc = this.doc
    let parametric = true
    let mode = ParseMode.normal

    function addToken(rToken: RichToken, line: number) {
      tokens.push(rToken)
      branch.push(rToken)
      lines[line].push(rToken)
    }

    doc.getText().split('\n').forEach((text, line) => {
      lines.push([])
      // Here, replace is used as "for each match, do..."
      text.replace(tokenPattern, (token, offset) => {
        if (mode === ParseMode.normal) {
          let id: string|undefined
          let wiki: boolean = true
          let error: string|false = false

          if (token === '#') { mode = ParseMode.comment; id = 'comment'; wiki = false } 
          else {
            if (parametric) {
              if (exeStack.length === 0) {
                if (token[0] === '$') { id = 'define' }
                else { parametric = false }
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
              else if (token === '"') { mode = ParseMode.string; wiki = false }
              else if (token === '(' || token === ')') { id = token; wiki = false }
              else if (crplData.words.indexOf(lowerToken) > -1) { id = lowerToken }
              else if (crplData.unitConstants.hasOwnProperty(lowerToken)) { id = lowerToken }
              else if (/-?\d+(.\d*)?/.test(token)) { wiki = false }
              else {
                symbolPatterns.forEach((name, re) => {
                  if (re.test(token)) { id = name }
                })
                error = id ? false : `Unknown token "${token}".`
                wiki = !!id
              }
            }
          }

          let rToken = <RichToken>{
            token, id, error, wiki,
            range: new vscode.Range(
              line, offset,
              line, offset + token.length
            )
          }

          // go shallower
          if (id && spec.down.get(id)) {
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
          addToken(rToken, line)
          // go deeper
          if (id && RichDoc.stackSpec.up.indexOf(id) > -1) {
            exeStack.push(rToken)
            let newBranch = new ParseTree()
            branch.push(newBranch)
            branch = newBranch
          }
        } else if (mode === ParseMode.string && token === '"') {
          let rToken = <RichToken>{ 
            token, range: new vscode.Range(
              line, offset,
              line, offset + token.length
            )
          }
          addToken(rToken, line)
          mode = ParseMode.normal
        }
        return ''
      })
      if (mode === ParseMode.comment) { mode = ParseMode.normal }
    })
    exeStack.forEach(rToken => {
      if (rToken.id === 'func') {
        let wrongness = spec.unstarted.get(rToken.id)
        rToken.error = (exeStack.length > 1) && `Unexpected token "${rToken.token}"${wrongness}.`
        return
      }
      if (rToken.error) { return }
      rToken.parent = <ParseTree>rToken.parent
      let [ match, startOffset ] = <[string, 0|2]>RichDoc.stackSpec.unmatched.get(<string>rToken.id)
      let errorToken = <RichToken>rToken.parent[rToken.parent.indexOf(rToken) - startOffset]
      if (errorToken.error) { return }
      errorToken.error = `"${errorToken.id}"` + wom(match)
    })
    // this.printTokens()
  } catch (err) { console.log(err); throw err }} 

  getWord(position: vscode.Position): WordInTheHand {
    let wordRange = this.doc.getWordRangeAtPosition(position, tokenPattern)
    let word = wordRange && this.doc.getText(wordRange)
    let tokenMatch = wordRange && this.tokens.flat.find(rToken => (<vscode.Range>wordRange).isEqual(rToken.range))
    return { wordRange, word, tokenMatch }
  }

  diagnose(diagnostics: vscode.DiagnosticCollection) {
    let result = this.tokens.flat
      .filter(t => t.error)
      .map(t => new vscode.Diagnostic(t.range, <string>t.error))
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
      `[${args}] -- [${results}]`,
      ...description.split(splitKey)
    ]
  } catch (err) {
    console.log(err)
    throw err
  }
}

function completionFilter(completionList: string[], word: string, sortPrefix: string) {
  let pattern: RegExp = /^/
  prefixPatterns.forEach(p => {
    if (p.test(word)) { pattern = p }
  });
  let prefix = (<RegExpExecArray>pattern.exec(word))[0]

  let niceWord = word.toLowerCase().replace(pattern, '')
  let result = completionList
    .filter(s => s.startsWith(prefix))
    .map(s => s.replace(pattern, ''))
    .filter(completion => completion.toLowerCase().startsWith(niceWord))
    .map(s => prefix + s)
  return result.map(c => <vscode.CompletionItem>{
    label: c,
    sortText: sortPrefix + c
  })
}

export async function scrapeSignatures(groupSize: number = 10) {
  let groups: string[][] = []
  let group: string[]

  crplData.words.forEach((word, i) => {
    if (!(i % groupSize)) { group = []; groups.push(group) }
    group.push(word)
  })
  let result = []
  for (let i = 0; i < groups.length; i++) {
    console.log(i+1, '/', groups.length)
    result.push(await Promise.all(groups[i].map(async word => {
      type sigType = [(string|undefined)[], (string|undefined)[]]
      let item: { [word:string]: 'ERROR' | sigType } = {}
      try {
        let docs = await request(crplData.prefix + word + crplData.suffix)

        docs = docs.replace(/\r/g, '')
        docs = (<RegExpExecArray>/=====(.*\n)*/.exec(docs))[0];
        docs = docs.replace(/(===+\s*Examples|<\/textarea)(.*\n)*/, '');
        docs = docs.replace(/''|%%/g, '`');
        docs = entityDecode(docs)
        let prepresig = /\| *`? *((?:[bifxysln]\d? *?)*) *-+ *((?:[bifxysln]\d? *?)*) *`? *\|$/i.exec(docs)
        if (!prepresig) {
          throw new Error(`Error getting signature from ${word}`)
        }
        let presig = (<(string|null|undefined)[]>prepresig).slice(1, 3)
        let signature: sigType = [
          presig[0] ? (<string>presig[0]).split(/ +/).map(t => t[0].toLowerCase()) : [],
          presig[1] ? (<string>presig[1]).split(/ +/).map(t => t[0].toLowerCase()) : []
        ]
        item[word] = signature
        return item
      } catch (e) {
        console.log(word, e)
        item[word] = 'ERROR'
        return item
      }
    })))
  }

  console.log(
    JSON.stringify(result, null, ' ' )
    .replace(/\n  ( +|(?=}))/g,'')
    .replace( /\n [\[\]],?/g, '')
    .replace(/},?\n  {/g, ',\n')
  )
}

export function activate(context: vscode.ExtensionContext) {
  // scrapeSignatures()
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
            if (tokenMatch && tokenMatch.wiki && !tokenMatch.error && tokenMatch.id !== undefined) {
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
        } else {
          return undefined
        }
      }
    }),
    vscode.languages.registerCompletionItemProvider(crplSelector, {
      async provideCompletionItems (document, position, token) {
        let rDoc = <RichDoc>documents.get(document.uri.toString())
        let { wordRange, word } = rDoc.getWord(position.with(undefined, position.character - 1))
        if (word && (<vscode.Range>wordRange).end.isEqual(position)) {
          let longlist = completionFilter(rDoc.tokens.flat.map(rt => rt.token), word, '0')
          longlist.push(...completionFilter(crplData.completionList, word, '9'))
          let dupelog: string[] = []
          let result: vscode.CompletionItem[] = []
          longlist.forEach( (c, i) => {
            if (dupelog.indexOf(c.label) === -1) { dupelog.push(c.label); result.push(c) }
          })
          return result
        } else {
          return undefined
        }
      }
    })
  )
}

export function deactivate() {
}