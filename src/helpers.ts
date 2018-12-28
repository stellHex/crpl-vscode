import { Range } from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')

export interface RichToken {
  token: string
  range: Range
  id?: string
  error?: { message: string, code: string }
  warning?: { message: string, code: string }
  parent?: ParseTree
  meta: TokenMeta
}

export interface TokenMeta {
  var?: string       // name of referenced variable
  func?: string      // name of defined or called function
  comment?: string[] // documentation for function defs
  delta?: StackDelta // stack and type validation
  wiki?: boolean     // whether to look it up if hovered over
  blockPredecessor?: RichToken  // eg "if" or "else" for "endif"
  blockSuccessor?: RichToken    // eg "else" or "endif" for "if"
  // these next two are only necessary when the errors are via lint hide
  // or when lint disabling errors which aren't added during the main first pass (such as in checkVariables)
  notLinting?: boolean          // whether all errors and warnings have been suppressed 
  disabledLints?: Set<string>   // exists if specific errors and warnings have been suppressed
}

export type ParseChunk = // TODO (maybe) convert main parser to be `ParseChunk[]`s instead of `[...ParseChunk]`s
    [RichToken, ParseTree, RichToken, ParseTree, RichToken]
  | [RichToken, ParseTree, RichToken]
  | RichToken
export type ParseBranch = ParseTree | RichToken
export class ParseTree extends Array<ParseBranch> {
  start?: RichToken
  parent?: ParseTree
  push (...branches: ParseBranch[]) {
    branches.forEach(branch => branch.parent = this)
    return super.push(...branches)
  }
  splice(start: number, deleteCount:number, ...items: ParseBranch[]) {
    items.forEach(branch => branch.parent = this)
    return super.splice(start, deleteCount, ...items)
  }
}

export enum ParseMode {
  normal, string, comment
}

export enum CRPLType {
  b = 0b0001, // bool
  i = 0b0111, // int
  f = 0b0101, // float
  s = 0b0001, // string
  l = 0b1001, // list
  n = 0b0001, // any
  o = 0b0000, // bottom of stack
  listBit = 0b10000,
  bb = b + listBit,
  ii = i + listBit,
  ff = f + listBit,
  ss = s + listBit,
  ll = l + listBit,
  nn = n + listBit,
  shrug = 255 // for when we don't know what's happening and have decided we don't care
}

export const sigSpec = {
  convertible (t1: CRPLType, t2: CRPLType) { return (t1 & t2) === t2 },
  isList (t: CRPLType) { return t && CRPLType.listBit},
  inTypes: new Map([
    ['b', CRPLType.b], ['i', CRPLType.i], ['f', CRPLType.f],
    ['x', CRPLType.f], ['y', CRPLType.f], ['z', CRPLType.f],
    ['fi', CRPLType.f],
    ['s', CRPLType.s],
    ['l', CRPLType.l],
    ['n', CRPLType.n],
    ['b*', CRPLType.bb], ['i*', CRPLType.ii], ['f*', CRPLType.ff],
    ['x*', CRPLType.ff], ['y*', CRPLType.ff], ['z*', CRPLType.ff],
    ['fi*', CRPLType.ff],
    ['s*', CRPLType.ss],
    ['l*', CRPLType.ll],
    ['n*', CRPLType.nn], ['*', CRPLType.nn]
  ]),
  outTypes: new Map([
    ['b', CRPLType.i], ['i', CRPLType.i], ['f', CRPLType.f],
    ['x', CRPLType.i], ['y', CRPLType.i], ['z', CRPLType.f],
    ['fi', CRPLType.f],
    ['s', CRPLType.s],
    ['l', CRPLType.l],
    ['n', CRPLType.n],
    ['b*', CRPLType.ii], ['i*', CRPLType.ii], ['f*', CRPLType.ff],
    ['x*', CRPLType.ii], ['y*', CRPLType.ii], ['z*', CRPLType.ff],
    ['fi*', CRPLType.ff],
    ['s*', CRPLType.ss],
    ['l*', CRPLType.ll],
    ['n*', CRPLType.nn], ['*', CRPLType.nn]
  ])
}

export interface FuncTracker {
  func: RichToken[]
  call: RichToken[]
}
export interface VarTracker {
  read: RichToken[], write: RichToken[],
  exists: RichToken[], delete: RichToken[],
  define: RichToken[], declare: RichToken[]
}

export class StackDelta {
  private raw: ArrayBuffer
  input: Uint8Array
  output: Uint8Array
  // TODO: all of these
  dup: boolean; dup2: boolean; swap: boolean
  arithmetic: boolean // whether the command preserves integerness
  reflow: boolean     // whether the command is flow control
  listout: boolean    // whether the command creates a list, as in: i0 i1...
  constructor (
    [input, output, odd]: [number[], number[], boolean?],
    { dup = false, dup2 = false, swap = false, arithmetic = false,  reflow = false, listout = false }
  ) {
    let ins = input.length
    let outs = output.length
    this.raw = new ArrayBuffer(ins + outs)
    this.input = new Uint8Array(this.raw, 0, ins)
    this.output = new Uint8Array(this.raw, ins, outs)
    input.forEach((t, i) => this.input[i] = t);
    output.forEach((t, i) => this.output[i] = t);
    this.dup = dup; this.dup2 = dup2; this.swap = swap;
    this.arithmetic = arithmetic; this.reflow = reflow; this.listout = listout
  }
  patch (stack: Stack) {
    let arithmeticType = CRPLType.i
    this.input.reduceRight((_, expected, i) => {
      let popped = stack.pop(expected)
      if (popped === undefined) {
        throw { popped, place: i, expected }
      }
      arithmeticType &= popped
      if (!sigSpec.convertible(popped, expected)) {
        throw { popped, place: i, expected }
      }
      return _
    }, null)
    if (this.arithmetic) {
      stack.push(...(this.output.map(t => arithmeticType | t)))
    } else {
      stack.push(...this.output)
    }
  }
}

export class Stack extends Array<number> {
  constructor (initial: number[] = []) {
    super(1024)
    initial.forEach((t, i) => this[i] = t);
  }
  pop (expected?: number) { // TODO: implement proper star types
    if (this.length) {
      let result = super.pop()
      if (result && sigSpec.isList(result)) {
        this.push(result)
      }
      return result
    } else {
      return 0
    }
  }
  clear () {
    this.splice(0, this.length)
  }
  patch (delta: StackDelta) {
    delta.patch(this)
  }
}

export class StackTracker {
  base: Stack
  constructor (base: number[]) {
    this.base = new Stack(base)
  }
  patch(deltas: StackDelta[]) {
    let stack = new Stack(this.base)
    deltas.forEach(delta => stack.patch(delta))
  }
}

let localSignatures = new Map<string, StackDelta|StackTracker>()
Object.entries(crplData.signatures).forEach(([token, sigWithBadType]) => {
  let sigArray = <[string[], string[], boolean?]>sigWithBadType
  let reflow = /^(break|do|else|endif|endonce|endwhile|loop|once|endonce|repeat|return|while)$/.test(token)
  let arithmetic = sigArray[0][0] === 'fi'
  let dup = token === 'dup'
  let dup2 = token === 'dup2'
  let swap = token === 'swap'
  let listout = /^(get.*unitsinrange|getcoreswithvar)/.test(token)
  
  let deltaArray: [CRPLType[], CRPLType[], boolean?] = [
    sigArray[0].map(t => <CRPLType>sigSpec.inTypes.get(t)),
    sigArray[1].map(t => <CRPLType>sigSpec.outTypes.get(t)),
    sigArray[2]
  ]
  if (
    /^(clearstack|(pre|ap)pendstacktolist)$/.test(token)
    || token[0] === ':'
  ) {
    return localSignatures.set(token, new StackTracker(deltaArray[1]))
  }
  localSignatures.set(token, new StackDelta(deltaArray, {dup, dup2, swap, arithmetic, reflow, listout}))
})

export const signatures = localSignatures

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
      let item: { [word:string]: sigType } = {}
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
        item[word] = [['ERROR'],[]]
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

export const stackSpec = {
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
    ['endcomment', 'comment'],
    ['"', '"']
  ]),
  unmatched: new Map<string, 0|2>([
    // number represents where the start of the block is
    // inside the ParseTree in relation to the error'd token
    // 0 = same token, eg ["once", [...]] -> "once" errors, its block starts at itself
    // 2 = different token, eg ["if", [...], "else", [...]] -> "else" errors, its block starts at "if"
    ['(', 0],
    ['do', 0],
    ['once', 0], 
    ['if', 0],
    ['else', 2],
    ['while', 0],
    ['repeat', 2],
    ['comment', 0],
    ['"', 0]
  ]),
}

export function surely<T>(x: T) { return <Exclude<T, undefined|null>>x }