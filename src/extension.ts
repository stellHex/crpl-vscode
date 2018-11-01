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
}

interface WordInTheHand {
  wordRange: vscode.Range | undefined,
  word: string | undefined,
  tokenMatch: RichToken | undefined
}

type ParseTree = ParseTreeHelper | RichToken
interface ParseTreeHelper extends Array<ParseTree> {}

class RichDoc {
  static tokenPattern = /"[^"]*"|[$:]\w+|[(:)]|#.*|\S+/g
  static valuePattern = /"[^"]*"|-?\d+(\.\d*)?/
  static stackSpec = {
    up: ['(', 'func', 'do', 'once', 'if', 'else', 'while', 'repeat'],
    down: new Map<string,string|RegExp>([
      [')', '('],
      ['func', /^(func|start)$/],
      ['loop', 'do'],
      ['endonce', 'once'],
      ['else', 'if'],
      ['endif', /^(if|else)$/],
      ['repeat', 'while'],
      ['endwhile', 'repeat']
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
  tree: ParseTree = [RichDoc.startToken]
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
    this.tree = [RichDoc.startToken]
    let stack: RichToken[] = []
    function pop() { return stack.length ? stack.pop() : '' }
    function push(token: RichToken) { stack.push(token) }

    let doc = this.doc
    let parametric = true
    let comment = false
    // Here, replace is used as "for each match, do..."
    doc.getText().replace(RichDoc.tokenPattern, (token, offset) => {
      let id: string|undefined
      let wiki: boolean = false
      let error: string|false = false
      let lowerToken = token.toLowerCase()
      if (token[0] === '"') { id = 'value' }
      else if (crplData.words.indexOf(lowerToken) > -1) { id = lowerToken }
      else if (crplData.unitConstants.hasOwnProperty(lowerToken)) { id = lowerToken }
      else if (/-?\d+(.\d*)?/.test(token)) { id = 'value' }  
      else {
        symbolPatterns.forEach((name, re) => {
          if (re.test(token)) {
            id = name
            wiki = true
          }
        })
        error = id ? `Unknown token "${token}."` : false
      }
      let richToken = <RichToken>{
        token, id, error, wiki,
        range: new vscode.Range(doc.positionAt(offset), doc.positionAt(offset + token.length))
      }
      this.tokens.push(richToken)
      return ''
    })
  }

  getWord(position: vscode.Position): WordInTheHand {
    let wordRange = this.doc.getWordRangeAtPosition(position, wordPattern)
    let word = wordRange && this.doc.getText(wordRange)
    let tokenMatch = wordRange && this.tokens.find(richToken => (<vscode.Range>wordRange).isEqual(richToken.range))
    return { wordRange, word, tokenMatch }
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
    if (matchedResult === null) { throw new Error('Ill-formatted doc page retrieved.') }
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

let documents: Map<string, RichDoc> = new Map

export function activate(context: vscode.ExtensionContext) {

  const watcher = vscode.workspace.createFileSystemWatcher('.crpl')
  const push: (sub: vscode.Disposable) => number = context.subscriptions.push.bind(context.subscriptions)

  vscode.workspace.textDocuments.forEach(doc => { if (vscode.languages.match(crplSelector, doc)) { 
    documents.set(doc.uri.toString(), new RichDoc(doc))
  }})
  push(watcher.onDidCreate(uri => {
    documents.set(uri.toString(), new RichDoc(uri))
  }))
  push(watcher.onDidChange(uri => {
    (<RichDoc>documents.get(uri.toString())).tokenize()
  }))
  push(watcher.onDidDelete(uri => {
    documents.delete(uri.toString())
  }))
  push(vscode.languages.registerHoverProvider(crplSelector, {
    async provideHover (document, position, token) {
      let richDoc = <RichDoc>documents.get(document.uri.toString())
      let { wordRange, word, tokenMatch } = richDoc.getWord(position)
      if (word) {
        try {
          if (tokenMatch && tokenMatch.id !== undefined) {
            if (tokenMatch.id.startsWith('const_')) {
              return new vscode.Hover(`\`${tokenMatch.id.toUpperCase()}\`: ${(crplData.unitConstants as any)[tokenMatch.id]}`, wordRange)
            }
            let docs = await request(crplData.prefix + tokenMatch.id + crplData.suffix)
            return new vscode.Hover(docuWikiDocToMD(docs), wordRange)
          } else {
            return undefined
          }
        } catch(err) { console.log(err) }
      } else {
        return undefined
      }
    }
  }))
  push(vscode.languages.registerCompletionItemProvider(crplSelector, {
    async provideCompletionItems (document, position, token) {
      let richDoc = <RichDoc>documents.get(document.uri.toString())
      let { wordRange, word } = richDoc.getWord(position.with(undefined, position.character - 1))
      if (word && (<vscode.Range>wordRange).end.isEqual(position)) {
        let result = completionFilter(richDoc.tokens.map(rt => rt.token), word, '0')
        result.push(...completionFilter(crplData.completionList, word, '9'))
        let dupelog: string[] = []
        result.forEach( (c, i) => {
          if (dupelog.indexOf(c.label) === -1) { dupelog.push(c.label) }
          else { delete result[i] }
        })
        return result
      } else {
        return undefined
      }
    }
  }))
}

export function deactivate() {
}