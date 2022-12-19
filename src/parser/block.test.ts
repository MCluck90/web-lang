import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import { NodeType } from './ast'
import { _block } from './block'

describe('_block', () => {
  test.each(['{}', '{ }', '{\n}'])('can parse empty blocks', (source) => {
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.expressions).toHaveLength(0)
  })

  test.each([
    ['{ 1 }', 'Integer' as NodeType],
    ['{ add(1, 2) }', 'FunctionCall' as NodeType],
  ])('can parse blocks with a single expression', (source, expressionType) => {
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.expressions).toHaveLength(1)
    assertNodeType(block.expressions[0], expressionType)
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
    expect(block.expressions).toHaveLength(count)
  })

  test('allow separating expressions with semicolons', () => {
    const source = '{ 1; 2; 3; }'
    const block = _block.parseToEnd(source)
    assertSuccessfulParse(block)
    assertNodeType(block, 'Block')
    expect(block.expressions).toHaveLength(3)
  })
})
