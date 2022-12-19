import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import { NodeType } from './ast'
import { _block } from './block'

describe('_block', () => {
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
