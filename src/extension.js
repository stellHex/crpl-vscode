const vscode = require('vscode')

const MODE = vscode.DocumentFilter = { language: 'crpl', scheme: 'file' }

// vscode.HoverProvider = 0

module.exports = { activate (ctx) {
  let push = ctx.subscriptions.push.bind(ctx.subscriptions)
  push(vscode.languages.registerHoverProvider(MODE, {
    provideHover (document, position, token) {
      console.log(document.getText(document.getWordRangeAtPosition(position)))
      return { contents: '```Hover!```' }
    }
  }))
  push(vscode.commands.registerCommand('extension.sayGoodbye', function () {
    vscode.window.showInformationMessage('Toodles!')
  }))
} }
