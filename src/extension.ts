'use strict';
import * as vscode from 'vscode';
import * as tokenizer from 'vscode-textmate'
import * as tmcrpl from './crpl.tmLanguage.json'

const MODE: vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }
const wordPattern =  new RegExp("(<-|->|-\\?|--|@)[A-Za-z]\\w*\\b|\\$?\\b\\w*\\b:|\\b\\w+(\\.\\d*)?\\b|(<-!|->!|-\\?!|--\\?)(?=\\s|$)")
const symbolPatterns: Map<RegExp, string> = new Map([
  [/<-[A-Za-z]\w*\b/, 'read'],
  [/->[A-Za-z]\w*\b/, 'write'],
  [/-?[A-Za-z]\w*\b/, 'exists'],
  [/--[A-Za-z]\w*\b/, 'delete'],
  [/<-!/, 'refread'],
  [/->!/, 'refwrite'],
  [/-?!/, 'refexists'],
  [/--?/, 'refdelete'],
])

function tokenize(doc: vscode.TextDocument): [string, vscode.Position][] {
  return []
}

class RichDoc {
  doc: vscode.TextDocument
  tokens: [string, vscode.Position][]
  constructor(doc: vscode.TextDocument) {
    this.doc = doc
    this.tokens = tokenize(doc)
  }
}

let documents: RichDoc[] = []

export function activate(context: vscode.ExtensionContext) {

  const watcher = vscode.workspace.createFileSystemWatcher('.crpl')
  const push: (sub: vscode.Disposable) => number = context.subscriptions.push.bind(context.subscriptions)

  push(watcher.onDidCreate(uri => {

  }))
  push(watcher.onDidDelete(uri => {
    // documents.
  }))
  // vscode.workspace.onDidChangeTextDocument(

  // )
  push(vscode.languages.registerHoverProvider(MODE, {
    provideHover (document, position, token) {
      let wordRange = document.getWordRangeAtPosition(position, wordPattern)
      let word = wordRange && document.getText(wordRange)
      return word ? new vscode.Hover(word, wordRange) : undefined
    }
  }))
}

export function deactivate() {
}