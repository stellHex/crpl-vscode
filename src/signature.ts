import { RichToken, ParseTree, ParseBranch } from './parseHelpers'

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
      return 0 // TODO: change 0 from being ANY type to EMPTY type
    }
  }
  clear () {
    this.splice(0, this.length)
  }
  patch (delta: StackDelta) {
    delta.input.reduceRight((prev, expected, i) => {
      let popped = this.pop(expected)
      if (!coerceable(popped, expected)) {
        throw { popped, place: i, expected }
      }
      return prev
    }, null)
    this.push(...delta.output)
  }
}