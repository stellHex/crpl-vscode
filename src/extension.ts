'use strict';
import * as vscode from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplDocs = require('./crpl-docs.json')

const crplSelector: vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }
const wordPattern = /(<-|->|-\?|--|@|:)[A-Za-z]\w*\b|\$?\b\w*\b:|\b\w+(\.\d*)?\b|(<-!|->!|-\?!|--\?)(?=\s|$)/
const symbolPatterns: Map<RegExp, string> = new Map([
  [/<-[A-Za-z]\w*\b/, 'read'],
  [/->[A-Za-z]\w*\b/, 'write'],
  [/-\?[A-Za-z]\w*\b/, 'exists'],
  [/--[A-Za-z]\w*\b/, 'delete'],
  [/@[A-Za-z]\w*\b/, 'call'],
  [/\w[A-Za-z]\w*:/, 'func'],
  [/<-!/, 'refread'],
  [/->!/, 'refwrite'],
  [/-\?!/, 'refexists'],
  [/--\?/, 'refdelete'],
])
const docPattern = /=====\s*(.*)\s*=====\s*(.*\s*)*?.*Arguments.*\^\s*\|\s*(.*?)\s*\|\s*(.*?)\s*\|(.*?)\|(.*\s*)*?===\s*Description.*\s*((.*\s*)*)/

interface RichToken {
  token: string
  range: vscode.Range
  id: string | undefined
  error: boolean
}

interface WordInTheHand {
  wordRange: vscode.Range | undefined,
  word: string | undefined,
  tokenMatch: RichToken | undefined
}

class RichDoc {
  static tokenPattern = /"[^"]*"|\S+/g

  doc: vscode.TextDocument
  tokens: RichToken[] = []
  constructor(docref: vscode.TextDocument | vscode.Uri) {
    if ((<vscode.TextDocument>docref).getText) {
      this.doc = <vscode.TextDocument>docref
    } else {
      this.doc = <vscode.TextDocument>vscode.workspace.textDocuments.find(doc => doc.uri === docref)
    }
    this.tokenize()
  }

  tokenize() {
    let doc: vscode.TextDocument = this.doc
    this.tokens = []
    // Here, replace is used as "for each match, do..."
    doc.getText().replace(RichDoc.tokenPattern, (token, offset) => {
      let id: string | undefined
      let error = false
      let lowerToken = token.toLowerCase()
      if (token[0] === '"') { id = undefined }
      else if (crplDocs.words.indexOf(lowerToken) > -1) { id = lowerToken }
      else if (crplDocs.unitConstants.hasOwnProperty(lowerToken)) { id = lowerToken }
      else {
        symbolPatterns.forEach((name, re) => {
          if (re.test(token)) { id = name }
        })
        error = !token
      }
      this.tokens.push({
        token, id, error,
        range: new vscode.Range(doc.positionAt(offset), doc.positionAt(offset + token.length))
      })
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
              return new vscode.Hover(`\`${tokenMatch.id.toUpperCase()}\`: ${(crplDocs.unitConstants as any)[tokenMatch.id]}`, wordRange)
            }
            let docs = await request(crplDocs.prefix + tokenMatch.id + crplDocs.suffix)
            return new vscode.Hover(docuWikiDocToMD(docs), wordRange)
          } else {
            return undefined
          }
        } catch(err){ console.log(err) }
      } else {
        return undefined
      }
    }
  }))
  push(vscode.languages.registerCompletionItemProvider(crplSelector, {
    async provideCompletionItems (document, position, token) {
      let richDoc = <RichDoc>documents.get(document.uri.toString())
      let { wordRange, word, tokenMatch } = richDoc.getWord(position)
      if (word && (<vscode.Range>wordRange).end.isEqual(position)) {
        return crplDocs.completionList
          .filter(completion => completion.toLowerCase().startsWith((<string>word).toLowerCase()))
          .map(completion => new vscode.CompletionItem(completion))
      } else {
        return undefined
      }
    }
  }))
}

export function deactivate() {
}