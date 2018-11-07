import { Range } from 'vscode'
import * as request from 'request-promise'
import entityDecode = require('decode-html')
import crplData = require('./crpl-data.json')
import { ParseTree } from './parseHelpers'

export enum crplType {
  b = 0b0001, // bool
  i = 0b0111, // int
  f = 0b0101, // float
  s = 0b0001, // string
  l = 0b1001, // list
  n = 0b0001, // any
  o = 0b0000, // bottom of stack
}

export interface StackToken {
  token: string
  range: Range
  id: string | undefined
  error: string | false
  parent: ParseTree | undefined
  delta: StackDelta
}

export const spec = {
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

export class StackDelta {
  private raw: ArrayBuffer
  input: Uint8Array
  output: Uint8Array
  constructor ([input, output]: [number[], number[]]) {
    let ins = input.length
    let outs = output.length
    this.raw = new ArrayBuffer(ins + outs)
    this.input = new Uint8Array(this.raw, 0, ins)
    this.output = new Uint8Array(this.raw, ins, outs)
    input.forEach((t, i) => this.input[i] = t);
    output.forEach((t, i) => this.output[i] = t);
  }
}

export class Stack extends Array<number> {
  constructor (initial: number[] = []) {
    super(1024)
    initial.forEach((t, i) => this[i] = t);
  }
  pop (expected?: number) { // TODO: implement star types
    if (this.length) {
      let result = super.pop()
      if (!this.length && result === 255) {
        // if the bottom of the stack is "*" then it shouldn't go away when popped
        // -1 can be converted into any type b/c two's complement
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
    delta.input.reduceRight((prev, expected: crplType, i) => {
      let popped = <crplType>this.pop(expected)
      if (!spec.convertible(popped, expected)) {
        throw { popped, place: i, expected }
      }
      return prev
    }, null)
    this.push(...delta.output)
  }
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