import * as vscode from 'vscode'

export interface RichToken {
  token: string
  range: vscode.Range
  id: string | undefined
  error: string | false
  wiki: boolean
  parent: ParseTree | undefined
}

export type ParseBranch = ParseTree | RichToken
export class ParseTree extends Array<ParseBranch> {
  parent: ParseTree | undefined
  push (...branches: ParseBranch[]) {
    branches.forEach(branch => branch.parent = this)
    return super.push(...branches)
  }
}

export enum ParseMode {
  normal, string, comment
}