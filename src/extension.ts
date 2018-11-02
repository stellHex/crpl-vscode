'use strict';
import * as vscode from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')

const crplSelector: vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }
const wordPattern = /(<-|->|-\?|--|@|:)[A-Za-z]\w*\b|\$?\b\w*\b(?=:)|-?\b\d+(\.\d*)?\b|(<-!|->!|-\?!|--\?)(?=\s|$)|\w+|[(:)]/
// note: this should be different from language-configuration.json.wordPattern only in the appending of |[(:)]
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

class RichDoc {
  static tokenPattern = /"[^"]*"|[$:][A-Za-z]\w*|[(:)]|#.*|\w+|\S/g
  static valuePattern = /"[^"]*"|-?\d+(\.\d*)?/
  static stackSpec = {
    up: ['(', 'define', ':', 'func', 'do', 'once', 'if', 'else', 'while', 'repeat'],
    down: new Map<string,string|RegExp>([
      [')', '('],
      ['define', 'start'], [':', 'define'], ['value', ':'],
      ['func', /^(func|start)$/],
      ['loop', 'do'],
      ['endonce', 'once'],
      ['else', 'if'],
      ['endif', /^(if|else)$/],
      ['repeat', 'while'],
      ['endwhile', 'repeat']
    ]),
    unmatched: new Map<string, string|undefined>([
      [')', 'without matched "("'],
      ['define', undefined],
      [':', 'without preceding "$" variable'],
      ['value', 'without preceding "$" variable'],
      ['func', 'inside another block'],
      ['loop', 'without matching "do"'],
      ['endonce', 'without matching "once"'],
      ['else', 'without matching "if"'],
      ['endif', 'without matching "if"'],
      ['repeat', 'without matching "while"'],
      ['endwhile', 'without matching "while" or "repeat"']
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
  tokens: RichToken[] = []
  tree: ParseTree = <ParseTree>[RichDoc.startToken]
  constructor(docref: vscode.TextDocument | vscode.Uri) {
    if ((<vscode.TextDocument>docref).getText) {
      this.doc = <vscode.TextDocument>docref
    } else {
      this.doc = <vscode.TextDocument>vscode.workspace.textDocuments.find(doc => doc.uri === docref)
    }
    this.tokenize()
  }

  tokenize() {
    this.tokens = []
    this.tree = new ParseTree()

    let spec = RichDoc.stackSpec
    let stack: RichToken[] = []
    let branch = this.tree
    let doc = this.doc
    let parametric = true
    
    // Here, replace is used as "for each match, do..."
    doc.getText().replace(RichDoc.tokenPattern, (token, offset) => {
      let id: string|undefined
      let wiki: boolean = true
      let error: string|false = false
      
      if (token[0] === '#') { id = 'comment'; wiki = false } 
      else {
        if (parametric) {
          if (stack.length === 0) {
            if (token[0] === '$') { id = 'define' }
            else { parametric = false }
          } else if (stack.length === 1 && stack[0].id === 'define') {
            wiki = false
            if (token === ':') { id = ':' }
            else { error = 'Expected ":".' } 
          } else if (stack.length === 1 && stack[0].id === ':') {
            wiki = false
            if (RichDoc.valuePattern.test(token)) { id = 'value' }
            else { error = 'Expected numeric literal or string.'; wiki = false } 
          } else {
            parametric = false
          }
        }
        let lowerToken = token.toLowerCase()
        if (!parametric) {
          if (token[0] === '$') { id = 'define'; error = `Input variables must go at the start of the file.` }
          else if (token[0] === '"') { wiki = false }
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

      let richToken = <RichToken>{
        token, id, error, wiki,
        range: new vscode.Range(doc.positionAt(offset), doc.positionAt(offset + token.length))
      }

      // go shallower
      if (id && spec.down.get(id)) {
        let top = stack.pop()
        let topid = top ? top.id : 'start'
        let matcher = spec.down.get(id)
        let matches = matcher instanceof RegExp ? (<RegExp>matcher).test(<string>topid) : matcher === topid
        if (matches) {
          branch = branch.parent || branch
        } else {
          let wrongness = spec.unmatched.get(id)
          richToken.error = !!wrongness && `Unexpected token "${token}" ${wrongness}.`
          if (top) { stack.push(top) }
        }
      }

      branch.push(richToken)
      
      // go deeper
      if (id && RichDoc.stackSpec.up.indexOf(id) > -1) {
        stack.push(richToken)
        let newBranch = new ParseTree()
        branch.push(newBranch)
        branch = newBranch
      }
      this.tokens.push(richToken)
      return ''
    })
    console.log(this.tree)
  }

  getWord(position: vscode.Position): WordInTheHand {
    let wordRange = this.doc.getWordRangeAtPosition(position, wordPattern)
    let word = wordRange && this.doc.getText(wordRange)
    let tokenMatch = wordRange && this.tokens.find(richToken => (<vscode.Range>wordRange).isEqual(richToken.range))
    return { wordRange, word, tokenMatch }
  }

  diagnose(diagnostics: vscode.DiagnosticCollection) {
    let result: vscode.Diagnostic[] = []
    diagnostics.set(this.doc.uri, result)
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

    let matchedResult = docPattern.exec(docu)
    if (matchedResult === null) { throw 400 }
    let [, id,, args, results, notation,, description] = matchedResult
    description = description.replace(/==+(.*?)==+/g, '\n\n-----\n\n**$1**\n');
    description = description.replace(/\[\[.*\|(.*)\]\]/g, '$1')
    // replace docuWiki table with MD table
    let newDescription = description.replace(/\^(.*)\n/g, '|$1\n|\n');
    while (newDescription !== description) {
      description = newDescription;
      newDescription = description.replace(/\^(.*)\n/, '|$1\n|---');
    }
    
    return [
      `\`${id}\`: ${notation}`,
      `[${args}] -- [${results}]`,
      description
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
    .map(s => s.replace(pattern, ''))
    .filter(completion => completion.toLowerCase().startsWith(niceWord))
    .map(s => prefix + s)
  return result.map(c => <vscode.CompletionItem>{
    label: c,
    sortText: sortPrefix + c
  })
}

export function activate(context: vscode.ExtensionContext) {
  const watcher = vscode.workspace.createFileSystemWatcher('.crpl')
  let documents: Map<string, RichDoc> = new Map
  let diagnostics = vscode.languages.createDiagnosticCollection('crpl')

  vscode.workspace.textDocuments.forEach(doc => { if (vscode.languages.match(crplSelector, doc)) { 
    let richDoc = new RichDoc(doc)
    documents.set(doc.uri.toString(), richDoc)
    richDoc.diagnose(diagnostics)
  }})
  
  context.subscriptions.push(
    watcher.onDidCreate(uri => {
      documents.set(uri.toString(), new RichDoc(uri))
    }),
    watcher.onDidChange(uri => {
      let doc = <RichDoc>documents.get(uri.toString())
      doc.tokenize()
      doc.diagnose(diagnostics)
    }),
    watcher.onDidDelete(uri => {
      documents.delete(uri.toString())
    }),
    vscode.languages.registerHoverProvider(crplSelector, {
      async provideHover (document, position, token) {
        let richDoc = <RichDoc>documents.get(document.uri.toString())
        let { wordRange, word, tokenMatch } = richDoc.getWord(position)
        if (word) {
          try {
            if (tokenMatch && tokenMatch.id !== undefined && tokenMatch.wiki) {
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
        let richDoc = <RichDoc>documents.get(document.uri.toString())
        let { wordRange, word } = richDoc.getWord(position.with(undefined, position.character - 1))
        if (word && (<vscode.Range>wordRange).end.isEqual(position)) {
          let longlist = completionFilter(richDoc.tokens.map(rt => rt.token), word, '0')
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