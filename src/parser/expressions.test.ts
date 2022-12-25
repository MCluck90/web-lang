import exp from 'constants'
import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import {
  createArgumentListNode,
  createBinaryExpressionNode,
  createFunctionCallNode,
  createIdentifierNode,
  createIntegerNode,
  createObjectPropertyNode,
  createPropertyAccessNode,
  createStringNode,
  createVariableAccessNode,
  createNamedTypeNode,
  NodeType,
} from './ast'
import { _block, _expression, _variableDeclaration } from './expressions'

describe('Booleans', () => {
  test.each([
    ['true', true],
    ['false', false],
  ])('can parse booleans', (source, value) => {
    const bool = _expression.parseToEnd(source)
    assertSuccessfulParse(bool)
    assertNodeType(bool, 'Boolean')
    expect(bool.value).toBe(value)
  })
})

describe('Numbers', () => {
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
})

describe('Strings', () => {
  test.each([
    ['"Hello"', 'Hello'],
    ["'World'", 'World'],
  ])('can parse strings', (source, value) => {
    const string = _expression.parseToEnd(source)
    assertSuccessfulParse(string)
    assertNodeType(string, 'String')
    expect(string.value).toBe(value)
  })
})

describe('Arithmetic', () => {
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

describe('Property Access', () => {
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

describe('Parenthesized Expressions', () => {
  test('can parse expressions surrounded by parentheses', () => {
    const source = '(1 + 2)'
    const expression = _expression.parseToEnd(source)
    assertSuccessfulParse(expression)
  })

  test('parentheses have higher precedence', () => {
    const source = '(1 + 2) * 3'
    const expression = _expression.parseToEnd(source)
    assertSuccessfulParse(expression)
    expect(expression).toEqual(
      createBinaryExpressionNode(
        createBinaryExpressionNode(
          createIntegerNode(1),
          '+',
          createIntegerNode(2)
        ),
        '*',
        createIntegerNode(3)
      )
    )
  })
})

describe('Function Calls', () => {
  test.each([
    ['func()', createVariableAccessNode(createIdentifierNode('func')), []],
    [
      'hello("world")',
      createVariableAccessNode(createIdentifierNode('hello')),
      [createStringNode('world')],
    ],
    [
      'add(1, 2)',
      createVariableAccessNode(createIdentifierNode('add')),
      [createIntegerNode(1), createIntegerNode(2)],
    ],
    [
      'log(add(1, 2), add(3, 4))',
      createVariableAccessNode(createIdentifierNode('log')),
      [
        createFunctionCallNode(
          createVariableAccessNode(createIdentifierNode('add')),
          createArgumentListNode([createIntegerNode(1), createIntegerNode(2)])
        ),
        createFunctionCallNode(
          createVariableAccessNode(createIdentifierNode('add')),
          createArgumentListNode([createIntegerNode(3), createIntegerNode(4)])
        ),
      ],
    ],
  ])('can parse function calls', (source, callee, args) => {
    const functionCall = _expression.parseToEnd(source)
    assertSuccessfulParse(functionCall)
    assertNodeType(functionCall, 'FunctionCall')
    expect(functionCall.callee).toEqual(callee)
    expect(functionCall.argumentList.arguments).toEqual(args)
  })

  test('function calls have lower precedence than property access', () => {
    const source = 'console.log()'
    const fnCall = _expression.parseToEnd(source)
    assertSuccessfulParse(fnCall)
    assertNodeType(fnCall, 'FunctionCall')
    assertNodeType(fnCall.callee, 'PropertyAccess')
  })

  test('function calls have higher precedence than multiplication', () => {
    const source = 'getX() * 10'
    const binaryExpression = _expression.parseToEnd(source)
    assertSuccessfulParse(binaryExpression)
    assertNodeType(binaryExpression, 'BinaryExpression')
    assertNodeType(binaryExpression.left, 'FunctionCall')
  })
})

describe('Object Literals', () => {
  test.each([
    ['{}', []],
    [
      '{ age: 100 }',
      [
        createObjectPropertyNode(
          createIdentifierNode('age'),
          createIntegerNode(100)
        ),
      ],
    ],
    [
      '{ age: 100, name: "Mike" }',
      [
        createObjectPropertyNode(
          createIdentifierNode('age'),
          createIntegerNode(100)
        ),
        createObjectPropertyNode(
          createIdentifierNode('name'),
          createStringNode('Mike')
        ),
      ],
    ],
  ])('can parse object literals', (source, properties) => {
    const objectLiteral = _expression.parseToEnd(source)
    assertSuccessfulParse(objectLiteral)
    assertNodeType(objectLiteral, 'ObjectLiteral')
    expect(objectLiteral.properties).toEqual(properties)
  })

  test('can parse nested objects', () => {
    const source = `
      {
        name: 'Mike',
        accounts: {
          github: {
            username: 'MCluck90'
          }
        }
      }
    `
    const person = _expression.parseToEnd(source)
    assertSuccessfulParse(person)
    assertNodeType(person, 'ObjectLiteral')
    expect(person.properties).toHaveLength(2)

    const accountsProp = person.properties[1]
    expect(accountsProp.key.value).toBe('accounts')
    const accountsObject = accountsProp.value
    assertNodeType(accountsObject, 'ObjectLiteral')
    expect(accountsObject.properties).toHaveLength(1)

    const githubObject = accountsObject.properties[0].value
    assertNodeType(githubObject, 'ObjectLiteral')
    expect(githubObject.properties).toHaveLength(1)
    expect(githubObject.properties[0].key.value).toBe('username')
    expect(githubObject.properties[0].value).toEqual(
      createStringNode('MCluck90')
    )
  })
})

describe('Variable Declarations', () => {
  test('can parse immutable variable declarations', () => {
    const source = 'let hello = "world"'
    const variableDeclaration = _variableDeclaration.parseToEnd(source)
    assertSuccessfulParse(variableDeclaration)
    assertNodeType(variableDeclaration, 'VariableDeclaration')
    expect(variableDeclaration.identifier.value).toBe('hello')
    expect(variableDeclaration.mutable).toBe(false)
    expect(variableDeclaration.type).toBe(null)
    expect(variableDeclaration.initializer).toEqual(createStringNode('world'))
  })

  test('can parse mutable variable declarations', () => {
    const source = 'let mut hello = "world"'
    const variableDeclaration = _variableDeclaration.parseToEnd(source)
    assertSuccessfulParse(variableDeclaration)
    assertNodeType(variableDeclaration, 'VariableDeclaration')
    expect(variableDeclaration.identifier.value).toBe('hello')
    expect(variableDeclaration.mutable).toBe(true)
    expect(variableDeclaration.type).toBe(null)
    expect(variableDeclaration.initializer).toEqual(createStringNode('world'))
  })

  test('can parse variable declarations with a type annotation', () => {
    const source = 'let x: int = 5'
    const variableDeclaration = _variableDeclaration.parseToEnd(source)
    assertSuccessfulParse(variableDeclaration)
    assertNodeType(variableDeclaration, 'VariableDeclaration')
    expect(variableDeclaration.type).toEqual(
      createNamedTypeNode('int', [], false)
    )
  })
})

describe('Blocks', () => {
  test.each(['{}', '{ }', '{\n}'])('can parse empty blocks', (source) => {
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(0)
  })

  test.each([
    ['{ 1 }', 'Integer' as NodeType],
    ['{ add(1, 2) }', 'FunctionCall' as NodeType],
  ])('can parse blocks with a single expression', (source, expressionType) => {
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(1)
    assertNodeType(block.statements[0], expressionType)
  })

  test.each([
    [
      `
      {
        1
        2
      }
    `,
      2,
    ],
    [
      `
      {
        1
        2
        log()
      }
    `,
      3,
    ],
  ])('can parse blocks with multiple expressions', (source, count) => {
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(count)
  })

  test('allow separating expressions with semicolons', () => {
    const source = '{ 1; 2; 3; }'
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(3)
  })

  test('can parse blocks with a variable declaration', () => {
    const source = `
      {
        let x = 1
      }
    `
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(1)
    assertNodeType(block.statements[0], 'VariableDeclaration')
  })

  test('can parse blocks with multiple variable declarations', () => {
    const source = `
      {
        let x = 1
        let mut y = 2
      }
    `
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(2)
    assertNodeType(block.statements[0], 'VariableDeclaration')
    assertNodeType(block.statements[1], 'VariableDeclaration')
  })

  test('can mix expressions and variable declarations', () => {
    const source = `
      {
        10
        let x = 1
        20
        let mut y = 2
        30
      }
    `
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.statements).toHaveLength(5)
    assertNodeType(block.statements[0], 'Integer')
    assertNodeType(block.statements[1], 'VariableDeclaration')
    assertNodeType(block.statements[2], 'Integer')
    assertNodeType(block.statements[3], 'VariableDeclaration')
    assertNodeType(block.statements[4], 'Integer')
  })
})

describe('If Expressions', () => {
  test('can parse if expressions', () => {
    const source = 'if (true) {}'
    const ifExpr = _expression.parseToEnd(source)
    assertSuccessfulParse(ifExpr)
    assertNodeType(ifExpr, 'If')
    assertNodeType(ifExpr.condition, 'Boolean')
    expect(ifExpr.body.statements).toHaveLength(0)
    expect(ifExpr.else_).toBe(null)
  })

  test('can parse if expressions with bodies', () => {
    const source = 'if (true) { "test"; 1; id }'
    const ifExpr = _expression.parseToEnd(source)
    assertSuccessfulParse(ifExpr)
    assertNodeType(ifExpr, 'If')
    assertNodeType(ifExpr.condition, 'Boolean')
    expect(ifExpr.body.statements).toHaveLength(3)
    expect(ifExpr.else_).toBe(null)
  })

  test('can parse if expressions with else blocks', () => {
    const source = 'if (true) { } else { "test"; 1; id }'
    const ifExpr = _expression.parseToEnd(source)
    assertSuccessfulParse(ifExpr)
    assertNodeType(ifExpr, 'If')
    assertNodeType(ifExpr.condition, 'Boolean')
    expect(ifExpr.body.statements).toHaveLength(0)
    expect(ifExpr.else_).not.toBe(null)
    expect(ifExpr.else_?.body.statements).toHaveLength(3)
  })
})

describe('Assignment', () => {
  test('can parse assignments', () => {
    const source = 'hello = "world"'
    const assignment = _expression.parseToEnd(source)
    assertSuccessfulParse(assignment)
    assertNodeType(assignment, 'Assignment')
    expect(assignment.left.value).toBe('hello')
    assertNodeType(assignment.right, 'String')
    expect(assignment.right.value).toBe('world')
  })
})
