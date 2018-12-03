import {
  RichToken, ParseTree, ParseMode, ParseChunk, // ParseBranch,
  VarTracker, FuncTracker } from './helpers'
import { TextDocument } from 'vscode';

interface RichDoc {
  doc: TextDocument
  tokens: { flat: RichToken[], lines: RichToken[][], tree: ParseTree }
  funcs: Map<string, FuncTracker>
  vars: Map<string, VarTracker>
  getVar: (name: string) => VarTracker
}

export interface ParseContext {
  commentMode?: Directive
  linting: boolean
  disabledLints: Set<String>
  branch: ParseTree
  parametric: boolean
  mode: ParseMode
  line: number
  exeStack: RichToken[]
  warpStack: (ParseChunk|ParseChunk[])[]
}

type DirectiveProcessor = (rDoc: RichDoc, rToken: RichToken, context: ParseContext) => void

interface ArgDirective {
  args: { validator: RegExp | string[], processor: DirectiveProcessor }
  processor?: DirectiveProcessor
  withoutArgsProcessor?: DirectiveProcessor
}
interface MiddleDirective {
  successors: DirectiveList
  processor?: DirectiveProcessor
}
interface LastDirective {
  processor: DirectiveProcessor
}
export type Directive = ArgDirective | MiddleDirective | LastDirective

class DirectiveList extends Map<string, Directive> { set() { return this } }
const namePattern = /[A-Za-z]\w*(?=\s)/
export const directives = new DirectiveList([[
  'lint',
  { successors: new DirectiveList([
    ['vars',
      { args: { validator: namePattern, processor: addToVars('declare') } }
    ], ['gets',
      { args: { validator: namePattern, processor: addToVars('read') } }
    ], ['sets',
      { args: { validator: namePattern, processor: addToVars('write') } }
    ], ['disable', {
      args: { validator: /^[]/, processor: function (rD, rT, ctx) { ctx.disabledLints.add(rT.token.toLowerCase()) } },
      withoutArgsProcessor: function (rD, rT, ctx) { ctx.linting = false } }
    ], ['hide', {
      args: { validator: /^[]/, processor: function (rD, rT, ctx) { 
        rD.tokens.lines[ctx.line].forEach(token => {
          if (token.warning && token) {} // TODO
        })
      } },
      withoutArgsProcessor: function (rD, rT, ctx) { 
        rD.tokens.lines[ctx.line].forEach(rToken => {
          if (rToken.error) rToken.meta.suppressErrors = true
        })
      } }
    ], ['enable', {
      args: { validator: /^[]/, processor: function (rD, rT, ctx) { ctx.disabledLints.delete(rT.token.toLowerCase()) } },
      withoutArgsProcessor: function (rD, rT, ctx) { ctx.linting = true} }
    // ], ['locals', { 
    //     processor: function (rD, rT, ctx) {},
    //     args: { validator: namePattern, processor: function (rD, rT, ctx) {} }
    //   }
    ]
  ])}
]])

function addToVars (refType: keyof VarTracker): DirectiveProcessor {
  return function (rD, rT) {
    let token = rT.token.toLowerCase()
    rT.meta.var = token
    rD.getVar(token)[refType].push(rT)
  }
}

let wom = (s: string) => ` without matching "${s}"`
type Messenger =
  string | Map<string, string> | ((rT: RichToken) => string) | [Map<string, string>, ((rT: RichToken, data: string) => string)]
export const crplErrors = new Map<string, Messenger> ([
  ['start-warp', 'Warp statements can\'t go at the beginning of the file!'],
  ['ijk',
    new Map([
      ['i', '"I" must be used inside a "do" loop.'],
      ['j', '"J" must be used inside nested "do" loops.'],
      ['k', '"K" must be used inside "do" loops.'],
      ['ik', '"K" should\'t be used for an innermost "do" loop; use "I" instead.'],
      ['jk', '"K" should\'t be used for the second innermost "do" loop; use "J" instead.']
    ])
  ], ['start-delim', [
    new Map([
      ['(', ')'],
      ['do', 'loop'],
      ['once', 'endonce'], 
      ['if', 'endif'],
      ['else', 'endif'],
      ['while', 'repeat'],
      ['repeat', 'endwhile'],
      ['comment', 'endcomment'],
      ['"', '"']
    ]),
    (rT, match) => `"${rT.id}"` + wom(match)
  ]], ['end-delim', [
    new Map([
      [')', wom('(')],
      [':', ' without preceding "$" variable'], 
      ['value', ' without preceding "$" variable'],
      ['func', ': function definition inside another block'],
      ['loop', wom('do')],
      ['endonce', wom('once')],
      ['if', wom('endif')],
      ['else', wom('if')],
      ['endif', wom('if')],
      ['repeat', wom('while')],
      ['endwhile', wom('while') + ' or "repeat"'],
      ['"', wom('"')]
    ]),
    (rT, wrongness) => `Unexpected token "${rT.token}"${wrongness}.`
  ]], ['define',
    new Map([
      [':', 'Expected ":".'],
      ['value','Expected numeric literal or string.']
    ])
  ],
  ['define-start', 'Input variables must go at the beginning of the file.'],
  ['unknown', (rT) => `Unknown token "${rT.token}".`],
  ['no-write', rT => `Variable ${rT.meta.var} is read from, but never written to.`],
  ['no-read', rT => `Variable ${rT.meta.var} is written to, but never read from.`],
  ['multi-def', rT => `Function ${rT.meta.func} is defined more than once.`],
  ['no-def', rT => `Function ${rT.meta.func} is never defined.`]
])
export function getError(code: string, key?: string, rT?: RichToken, ) {
  let messenger = crplErrors.get(code)
  let message
  if (typeof messenger === 'string') message = messenger
  else if (messenger instanceof Map && key) message = messenger.get(key)
  else if (messenger instanceof Function && rT) message = messenger(rT)
  else if (messenger instanceof Array && key && rT) message = messenger[1](rT, <string>messenger[0].get(key))
  return message ? { message, code } : undefined
}