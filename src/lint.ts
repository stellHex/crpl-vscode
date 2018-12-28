import {
  RichToken, ParseTree, ParseMode, ParseChunk, ParseBranch,
  VarTracker, FuncTracker, stackSpec } from './helpers'
import { TextDocument } from 'vscode';

interface RichDoc {
  doc: TextDocument
  tokens: { flat: RichToken[], lines: RichToken[][], tree: ParseTree }
  funcs: Map<string, FuncTracker>
  vars: Map<string, VarTracker>
  getVar: (name: string) => VarTracker
}

export interface ParseContext {
  doc: RichDoc
  commentMode?: Directive
  commentArgs?: boolean
  notLinting: boolean
  disabledLints: Set<string>
  branch: ParseTree
  tokens: RichToken[]
  lines: RichToken[][]
  tree: ParseTree
  parametric: boolean
  mode: ParseMode
  line: number
  exeStack: RichToken[]
  warpStack: (ParseChunk|ParseChunk[])[]
}

function addToVars (refType: keyof VarTracker): DirectiveProcessor {
  return function (rT, ctx) {
    let token = rT.token.toLowerCase()
    rT.meta.var = token
    ctx.doc.getVar(token)[refType].push(rT)
  }
}

let wom = (s: string) => ` without matching "${s}"`
type Messenger =
  string | Map<string, string> | ((rT: RichToken) => string) | [Map<string, string>, ((rT: RichToken, data: string) => string)]
const crplErrors = new Map<string, Messenger> ([
  ['warpstart', 'Warp statements can\'t go at the beginning of the file!'],
  ['ijk',
    new Map([
      ['i', '"I" must be used inside a "do" loop.'],
      ['j', '"J" must be used inside nested "do" loops.'],
      ['k', '"K" must be used inside "do" loops.'],
      ['ik', '"K" should\'t be used for an innermost "do" loop; use "I" instead.'],
      ['jk', '"K" should\'t be used for the second innermost "do" loop; use "J" instead.']
    ])
  ],
  ['startdelim', [
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
  ]],
  ['enddelim', [
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
  ]],
  ['define',
    new Map([
      [':', 'Expected ":".'],
      ['value','Expected numeric literal or string.']
    ])
  ],
  ['definestart', 'Input variables must go at the beginning of the file.'],
  ['lintarg', rT => `Unknown argument "${rT.token}" for this directive.`],
  ['lintsub', rT => `Unknown subdirective "${rT.token ? rT.token : `<${rT.id}>`}" for this directive.`],
  ['unknown', rT => `Unknown token "${rT.token}".`],
  ['unwritten', rT => `Variable ${rT.meta.var} is read from, but never written to.`],
  ['unread', rT => `Variable ${rT.meta.var} is written to, but never read from.`],
  ['multidef', rT => `Function ${rT.meta.func} is defined more than once.`],
  ['nodef', rT => `Function ${rT.meta.func} is never defined.`]
])

type DirectiveProcessor = (rToken: RichToken, context: ParseContext) => void

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

class DirectiveList extends Map<string, Directive> {}
const namePattern = /\w+(?=\s|$)/
export const directives = new DirectiveList([[
  'lint',
  { successors: new DirectiveList([
    ['vars',
      { args: { validator: namePattern, processor: addToVars('declare') } }
    ], ['gets',
      { args: { validator: namePattern, processor: addToVars('read') } }
    ], ['sets',
      { args: { validator: namePattern, processor: addToVars('write') } }
      // ], ['locals', { 
      //     processor: function (rT, ctx) {},
      //     args: { validator: namePattern, processor: function (rT, ctx) {} }
      //   }
    ], ['disable', {
      args: { 
        validator: Array.from(crplErrors.keys()),
        processor: function (rT, ctx) { ctx.disabledLints.add(rT.token.toLowerCase()) }
      },
      withoutArgsProcessor: function (rT, ctx) { ctx.notLinting = false }
    }], ['hide', {
      args: { 
        validator: Array.from(crplErrors.keys()),
        processor: function (rT, ctx) { 
          ctx.lines[ctx.line].forEach(token => {
            token.meta.disabledLints = token.meta.disabledLints || new Set()
            token.meta.disabledLints.add(rT.token)
          })
        }
      },
      withoutArgsProcessor: function (rT, ctx) { 
        ctx.lines[ctx.line].forEach(rToken => {
          rToken.meta.notLinting = true
        })
      }
    }], ['enable', {
      args: {
        validator: Array.from(crplErrors.keys()),
        processor: function (rT, ctx) { ctx.disabledLints.delete(rT.token.toLowerCase()) }
      },
      withoutArgsProcessor: function (rT, ctx) { ctx.notLinting = false; ctx.disabledLints.clear() }
    }]
  ])}
]])
  
export function getError(code: string, key?: string, rT?: RichToken) {
  let messenger = crplErrors.get(code)
  let message
  if (typeof messenger === 'string') message = messenger
  else if (messenger instanceof Map && key) message = messenger.get(key)
  else if (messenger instanceof Function && rT) message = messenger(rT)
  else if (messenger instanceof Array && key && rT) message = messenger[1](rT, <string>messenger[0].get(key))
  return message ? { message, code } : undefined
}

export class Linter {
  constructor (public ctx: ParseContext) {}
  
  getError(code: string, key?: string, rT?: RichToken) {
    if (this.ctx.notLinting || this.ctx.disabledLints.has(code)) { return undefined }
    let messenger = crplErrors.get(code)
    let message
    if (typeof messenger === 'string') message = messenger
    else if (messenger instanceof Map && key) message = messenger.get(key)
    else if (messenger instanceof Function && rT) message = messenger(rT)
    else if (messenger instanceof Array && key && rT) message = messenger[1](rT, <string>messenger[0].get(key))
    return message ? { message, code } : undefined
  }
  
  checkNest(rToken: RichToken) {
    let ctx = this.ctx
    let id = rToken.id
    if (id && stackSpec.up.indexOf(id) > -1) {
      ctx.exeStack.push(rToken)
      let newBranch = new ParseTree()
      newBranch.start = rToken
      ctx.branch.push(newBranch)
      ctx.branch = newBranch
    }
  }
  checkUnnest(rToken: RichToken) {
    let ctx = this.ctx
    let id = rToken.id
    if (id && stackSpec.down.get(id) && id !== '"') {
      let top = rToken.meta.blockPredecessor || ctx.exeStack.pop()
      let topid = top ? top.id : ctx.parametric ? 'start' : 'bottom'
      let matcher = stackSpec.down.get(id)
      let matches = matcher instanceof RegExp ? matcher.test(<string>topid) : matcher === topid
      if (matches) {
        ctx.branch = ctx.branch.parent || ctx.branch
        rToken.meta.blockPredecessor = top
        if (top) { top.meta.blockSuccessor = rToken }
      } else {
        rToken.error = rToken.error || this.getError('bad-end-delim', id, rToken)
        if (top) { ctx.exeStack.push(top) }
      }
    }
  }
  checkWarp(warpToken: RichToken) {
    let ctx = this.ctx
    if (warpToken.id === '(') {
      let wsMember: ParseChunk[] = []
      function pop() {
        let result = ctx.branch.pop()
        if (Array.isArray(result)) {
          ctx.tokens.splice(ctx.tokens.lastIndexOf(<RichToken>(<ParseTree>result)[0]))
        } else {
          ctx.tokens.pop()
        }
        return result
      }
      function warp() {
        if (ctx.branch.length) {
          let prevToken = <RichToken>pop()
          if (prevToken.id === 'endcomment') { // ignore comments
            let body = <ParseTree>pop(), start = <RichToken>pop() 
            wsMember.push([start, body, prevToken])
            warp()
          } else if (prevToken.id === '"') { // treat whole strings as one thing
            let body = <ParseTree>pop(), start = <RichToken>pop()
            finishWarp([start, body, prevToken])
          } else {
            if(Array.isArray(ctx.branch[ctx.branch.length-1])){
              ctx.branch = <ParseTree>ctx.branch[ctx.branch.length-1]
            }
            finishWarp(prevToken)
          }
        } else {
          ctx.branch = ctx.branch.parent || ctx.branch
          finishWarp(<RichToken>pop())
        }
      }
      const finishWarp = (prev: ParseChunk) => {
        let realMember = wsMember.length ? wsMember : prev
        wsMember.push(prev)
        if (ctx.branch === ctx.tree && (
          ctx.branch[ctx.branch.length-2] && (<RichToken>ctx.branch[ctx.branch.length-2]).id === ':'
          || prev === ctx.tree[0])
        ) {
          warpToken.error = this.getError('warpstart')
          this.unwarp(realMember)
        } else {
          ctx.warpStack.push(realMember)
        }
      }
      warp()
    }
  }
  checkUnwarp(warpToken: RichToken) {
    const ctx = this.ctx
    if (warpToken.id === ')') {
      if (ctx.warpStack.length) {
        let toUnwarp = <ParseChunk|ParseChunk[]>ctx.warpStack.pop()
        this.unwarp(toUnwarp)
      } else {
        warpToken.error = this.getError('warpstart')
      }
    }
  }
  unwarp(wsMember: ParseChunk|ParseChunk[]) {
    const ctx = this.ctx
    if (Array.isArray(wsMember)) {
      if (wsMember[1] instanceof ParseTree) {
        (<ParseBranch[]>wsMember).forEach(push)
      } else {
        while (wsMember.length) {
          this.unwarp(<ParseChunk|ParseChunk[]>wsMember.pop())
        }
      }
    } else {
      this.checkUnnest(wsMember)
      push(wsMember)
      this.checkNest(wsMember)
    }
    function push (b: ParseBranch) { ctx.tokens.push(...flatten(b)); ctx.branch.push(b) }
    function flatten(br: ParseBranch) {
      if (Array.isArray(br)) {
        let result: RichToken[] = []
        br.forEach(b => result.push(...flatten(b)))
        return result
      } else { return [br] }
    }
  }
  checkIJK(ijkToken: RichToken) {
    const depth = this.ctx.exeStack.filter(rToken => rToken.id === 'do').length
    if (ijkToken.id === 'i') {
      if (depth === 0) {
        ijkToken.error = this.getError('ijk', 'i')
      }
    } else if (ijkToken.id === 'j') {
      if (depth < 2) {
        ijkToken.error = this.getError('ijk', 'j')
      }
    } else if (ijkToken.id === 'k') {
      if (depth === 0) {
        ijkToken.error = this.getError('ijk', 'k')
      } else if (depth === 1) {
        ijkToken.error = this.getError('ijk', 'ik')
      } else if (depth === 2) {
        ijkToken.error = this.getError('ijk', 'jk')
      }
    }
  }
  checkDirectives(rToken: RichToken) {
    const ctx = this.ctx
    const token = rToken.token
    if (ctx.notLinting) { return }
    else if (ctx.commentMode) {
      if ((<ArgDirective>ctx.commentMode).args) {
        const directive = <ArgDirective>ctx.commentMode
        const validator = directive.args.validator
        let valid = validator instanceof RegExp ? validator.test(token) : validator.indexOf(token) > -1
        if (token === ",") {
          return
        } else if (valid) {
          ctx.commentArgs = true
          directive.args.processor(rToken, ctx)
        } else if (/^\W+$/.test(token) || rToken.id == 'endcomment') {
          ctx.commentMode = undefined
          if (!ctx.commentArgs && directive.withoutArgsProcessor) {
            directive.withoutArgsProcessor(rToken, ctx)
          }
        } else {
          ctx.commentArgs = true
          rToken.warning = this.getError('lintarg', undefined, rToken)
        }
      } else if ((<MiddleDirective>ctx.commentMode).successors) {
        const directive = <MiddleDirective>ctx.commentMode
        let newDirective = ctx.commentMode = directive.successors.get(token)
        if (!newDirective) { rToken.warning = this.getError('lintsub', undefined, rToken) }
        else {
          if (newDirective.processor) { newDirective.processor(rToken, ctx) }
          if ((<ArgDirective>newDirective).args) { ctx.commentArgs = false }
        }
      } else {
        ctx.commentMode = directives.get(token)
      }
    } else {
      ctx.commentMode = directives.get(token)
    }
  }
}