import {
  createArgumentListNode,
  createAssignmentNode,
  createBinaryExpressionNode,
  createBlockNode,
  createBooleanNode,
  createFloatingPointNode,
  createFunctionCallNode,
  createFunctionExpressionNode,
  createHTMLNode,
  createIdentifierNode,
  createIfNode,
  createIntegerNode,
  createJsAsmNode,
  createObjectLiteralNode,
  createParameterListNode,
  createProgramNode,
  createPropertyAccessNode,
  createStringNode,
  createUnaryExpressionNode,
  createVariableAccessNode,
  isAnExpressionNode,
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

describe('isAnExpressionNode', () => {
  test.each([
    createAssignmentNode(createIdentifierNode('a'), createIntegerNode(1)),
    createBinaryExpressionNode(createIntegerNode(1), '+', createIntegerNode(1)),
    createBooleanNode(true),
    createFloatingPointNode(1.5),
    createFunctionCallNode(
      createVariableAccessNode(createIdentifierNode('a')),
      createArgumentListNode([])
    ),
    createFunctionExpressionNode(
      createParameterListNode([]),
      createBlockNode([])
    ),
    createHTMLNode('html', []),
    createIfNode(createBooleanNode(true), createBlockNode([]), null),
    createIntegerNode(1),
    createJsAsmNode('console.log'),
    createObjectLiteralNode([]),
    createPropertyAccessNode(
      createVariableAccessNode(createIdentifierNode('a')),
      []
    ),
    createStringNode('hello'),
    createUnaryExpressionNode('!', createBooleanNode(false)),
    createVariableAccessNode(createIdentifierNode('a')),
  ])('returns true for expressions', (node) => {
    expect(isAnExpressionNode(node)).toBe(true)
  })

  test('returns false for non-expression nodes', () => {
    expect(isAnExpressionNode(createProgramNode([], [], null))).toBe(false)
  })
})
