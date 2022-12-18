import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import { createBinaryExpressionNode, createIntegerNode, Node } from './ast'
import { _expression } from './expressions'

describe('_expression', () => {
  test.each([['1', 1] as const, ['-1', -1] as const, ['9_001', 9001] as const])(
    'can parse integers',
    (source, value) => {
      const integer = _expression.parseToEnd(source)
      assertSuccessfulParse(integer)
      assertNodeType(integer, 'Integer')
      expect(integer.value).toBe(value)
    }
  )

  test.each([
    ['1.0', 1.0] as const,
    ['-1.0', -1.0] as const,
    ['9_00.1', 900.1] as const,
  ])('can parse floating point numbers', (source, value) => {
    const floatingPoint = _expression.parseToEnd(source)
    assertSuccessfulParse(floatingPoint)
    assertNodeType(floatingPoint, 'FloatingPoint')
    expect(floatingPoint.value).toBe(value)
  })

  test.each([
    ['1 + 2', 'Integer' as const, 'Integer' as const],
    ['x + 2', 'VariableAccess' as const, 'Integer' as const],
    ['1 + x', 'Integer' as const, 'VariableAccess' as const],
    ['x + x', 'VariableAccess' as const, 'VariableAccess' as const],
  ])('can parse addition', () => {
    const source = '1 + 2'
    const binaryExpression = _expression.parseToEnd(source)
    assertSuccessfulParse(binaryExpression)
    assertNodeType(binaryExpression, 'BinaryExpression')
    expect(binaryExpression.operator).toBe('+')

    const { left, right } = binaryExpression
    assertNodeType(left, 'Integer')
    assertNodeType(right, 'Integer')
  })

  test.each([
    ['1 - 2', 'Integer' as const, 'Integer' as const],
    ['x - 2', 'VariableAccess' as const, 'Integer' as const],
    ['1 - x', 'Integer' as const, 'VariableAccess' as const],
    ['x - x', 'VariableAccess' as const, 'VariableAccess' as const],
  ])('can parse subtraction', () => {
    const source = '1 - 2'
    const binaryExpression = _expression.parseToEnd(source)
    assertSuccessfulParse(binaryExpression)
    assertNodeType(binaryExpression, 'BinaryExpression')
    expect(binaryExpression.operator).toBe('-')

    const { left, right } = binaryExpression
    assertNodeType(left, 'Integer')
    assertNodeType(right, 'Integer')
  })

  test.each([
    ['1 * 2', 'Integer' as const, 'Integer' as const],
    ['x * 2', 'VariableAccess' as const, 'Integer' as const],
    ['1 * x', 'Integer' as const, 'VariableAccess' as const],
    ['x * x', 'VariableAccess' as const, 'VariableAccess' as const],
  ])('can parse multiplication', () => {
    const source = '1 * 2'
    const binaryExpression = _expression.parseToEnd(source)
    assertSuccessfulParse(binaryExpression)
    assertNodeType(binaryExpression, 'BinaryExpression')
    expect(binaryExpression.operator).toBe('*')

    const { left, right } = binaryExpression
    assertNodeType(left, 'Integer')
    assertNodeType(right, 'Integer')
  })

  test.each([
    ['1 / 2', 'Integer' as const, 'Integer' as const],
    ['x / 2', 'VariableAccess' as const, 'Integer' as const],
    ['1 / x', 'Integer' as const, 'VariableAccess' as const],
    ['x / x', 'VariableAccess' as const, 'VariableAccess' as const],
  ])('can parse division', (source, leftNodeType, rightNodeType) => {
    const binaryExpression = _expression.parseToEnd(source)
    assertSuccessfulParse(binaryExpression)
    assertNodeType(binaryExpression, 'BinaryExpression')
    expect(binaryExpression.operator).toBe('/')

    const { left, right } = binaryExpression
    assertNodeType(left, leftNodeType)
    assertNodeType(right, rightNodeType)
  })

  test.each([
    [
      '1 + 2 * 3',
      createBinaryExpressionNode(
        createIntegerNode(1),
        '+',
        createBinaryExpressionNode(
          createIntegerNode(2),
          '*',
          createIntegerNode(3)
        )
      ),
    ],
    [
      '1 * 2 + 3',
      createBinaryExpressionNode(
        createBinaryExpressionNode(
          createIntegerNode(1),
          '*',
          createIntegerNode(2)
        ),
        '+',
        createIntegerNode(3)
      ),
    ],
    [
      '1 - 2 / 3',
      createBinaryExpressionNode(
        createIntegerNode(1),
        '-',
        createBinaryExpressionNode(
          createIntegerNode(2),
          '/',
          createIntegerNode(3)
        )
      ),
    ],
    [
      '1 / 2 - 3',
      createBinaryExpressionNode(
        createBinaryExpressionNode(
          createIntegerNode(1),
          '/',
          createIntegerNode(2)
        ),
        '-',
        createIntegerNode(3)
      ),
    ],
  ])('term vs. factor precedence', (source, output) => {
    const expression = _expression.parseToEnd(source)
    assertSuccessfulParse(expression)
    expect(expression).toEqual(output)
  })
})
