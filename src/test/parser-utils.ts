import { ParseError } from 'parsnip-ts/error'
import { Node } from '../parser/ast'

export function assertSuccessfulParse<T>(
  result: T | ParseError
): asserts result is T {
  if (result instanceof ParseError) {
    throw result
  }
}

export function assertFailedParse<T>(
  result: T | ParseError
): asserts result is T {
  if (!(result instanceof ParseError)) {
    throw new Error('Expected parsing to fail but it succeeded')
  }
}

export function assertNodeType<T extends Node['__type']>(
  node: Node,
  type: T
): asserts node is Node & { __type: T } {
  if (node.__type !== type) {
    throw new Error(`Expected a ${type}, received a ${node.__type}`)
  }
}
