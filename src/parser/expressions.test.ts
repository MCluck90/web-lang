import exp from 'constants'
import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import {
  createBinaryExpressionNode,
  createIdentifierNode,
  createIntegerNode,
  createPropertyAccessNode,
  createVariableAccessNode,
  Node,
} from './ast'
import { _expression } from './expressions'

describe('_expression', () => {
  test.each([['1', 1] as const, ['9_001', 9001] as const])(
    'can parse positive integers',
    (source, value) => {
      const integer = _expression.parseToEnd(source)
      assertSuccessfulParse(integer)
      assertNodeType(integer, 'Integer')
      expect(integer.value).toBe(value)
    }
  )

  test.each([['-1', 1] as const, ['-9_001', 9001] as const])(
    'can parse negative integers',
    (source, value) => {
      const unaryExpression = _expression.parseToEnd(source)
      assertSuccessfulParse(unaryExpression)
      assertNodeType(unaryExpression, 'UnaryExpression')
      expect(unaryExpression.operator).toBe('-')

      const integer = unaryExpression.expression
      assertNodeType(integer, 'Integer')
      expect(integer.value).toBe(value)
    }
  )

  test.each([['1.0', 1.0] as const, ['9_00.1', 900.1] as const])(
    'can parse positive floating point numbers',
    (source, value) => {
      const floatingPoint = _expression.parseToEnd(source)
      assertSuccessfulParse(floatingPoint)
      assertNodeType(floatingPoint, 'FloatingPoint')
      expect(floatingPoint.value).toBe(value)
    }
  )

  test.each([['-1.0', 1.0] as const, ['-9_00.1', 900.1] as const])(
    'can parse negative floating point numbers',
    (source, value) => {
      const unaryExpression = _expression.parseToEnd(source)
      assertSuccessfulParse(unaryExpression)
      assertNodeType(unaryExpression, 'UnaryExpression')
      expect(unaryExpression.operator).toBe('-')

      const floatingPoint = unaryExpression.expression
      assertNodeType(floatingPoint, 'FloatingPoint')
      expect(floatingPoint.value).toBe(value)
    }
  )

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

  test('can parse property access', () => {
    const source = 'x.y'
    const propertyAccess = _expression.parseToEnd(source)
    assertSuccessfulParse(propertyAccess)
    assertNodeType(propertyAccess, 'PropertyAccess')
    expect(propertyAccess).toEqual(
      createPropertyAccessNode(
        createVariableAccessNode(createIdentifierNode('x')),
        [createIdentifierNode('y')]
      )
    )
  })

  test.each([
    [
      'x.y + 1',
      createBinaryExpressionNode(
        createPropertyAccessNode(
          createVariableAccessNode(createIdentifierNode('x')),
          [createIdentifierNode('y')]
        ),
        '+',
        createIntegerNode(1)
      ),
    ],
    [
      '1 + x.y',
      createBinaryExpressionNode(
        createIntegerNode(1),
        '+',
        createPropertyAccessNode(
          createVariableAccessNode(createIdentifierNode('x')),
          [createIdentifierNode('y')]
        )
      ),
    ],
    [
      '1 + 2 * x.y',
      createBinaryExpressionNode(
        createIntegerNode(1),
        '+',
        createBinaryExpressionNode(
          createIntegerNode(2),
          '*',
          createPropertyAccessNode(
            createVariableAccessNode(createIdentifierNode('x')),
            [createIdentifierNode('y')]
          )
        )
      ),
    ],
    [
      'x.y * 2 + 1',
      createBinaryExpressionNode(
        createBinaryExpressionNode(
          createPropertyAccessNode(
            createVariableAccessNode(createIdentifierNode('x')),
            [createIdentifierNode('y')]
          ),
          '*',
          createIntegerNode(2)
        ),
        '+',
        createIntegerNode(1)
      ),
    ],
    [
      '1 + x.y * 2',
      createBinaryExpressionNode(
        createIntegerNode(1),
        '+',
        createBinaryExpressionNode(
          createPropertyAccessNode(
            createVariableAccessNode(createIdentifierNode('x')),
            [createIdentifierNode('y')]
          ),
          '*',
          createIntegerNode(2)
        )
      ),
    ],
  ])('property access has correct precedence', (source, expected) => {
    const expression = _expression.parseToEnd(source)
    assertSuccessfulParse(exp)
    expect(expression).toEqual(expected)
  })
})