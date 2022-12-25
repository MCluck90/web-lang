import { Source } from 'parsnip-ts'
import { ParseError } from 'parsnip-ts/error'
import { createIntegerNode } from '../parser/ast'
import {
  assertFailedParse,
  assertNodeType,
  assertSuccessfulParse,
} from './parser-utils'

describe('assertSuccessfulParse', () => {
  it('should throw when given a ParseError', () => {
    expect(() =>
      assertSuccessfulParse(new ParseError(1, 1, '', new Source('', 0)))
    ).toThrow()
  })

  it('should not throw when given a non-ParseError', () => {
    assertSuccessfulParse(null)
  })
})

describe('assertFailedParse', () => {
  it('should not throw when given a ParseError', () => {
    assertFailedParse(new ParseError(1, 1, '', new Source('', 0)))
  })

  it('should throw when given a non-ParseError', () => {
    expect(() => assertFailedParse(null)).toThrow()
  })
})

describe('assertNodeType', () => {
  it('should succeed when given a node with a given type', () => {
    assertNodeType(createIntegerNode(1), 'Integer')
  })

  it('should throw when given a node which does not match the given type', () => {
    expect(() => assertNodeType(createIntegerNode(1), 'String')).toThrow()
  })
})
