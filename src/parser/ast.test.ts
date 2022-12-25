import {
  createBlockNode,
  createIntegerNode,
  createStringNode,
  isNodeType,
} from './ast'

describe('isNodeType', () => {
  test('returns false for null or undefined', () => {
    expect(isNodeType('AnonymousType')(null)).toBe(false)
    expect(isNodeType('AnonymousType')(undefined)).toBe(false)
  })

  test.each([
    createIntegerNode(1),
    createStringNode('test'),
    createBlockNode([]),
  ])(`returns true when given a node and it's own type`, (node) => {
    expect(isNodeType(node.__type)(node)).toBe(true)
  })
})
