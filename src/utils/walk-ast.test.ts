import { parseProgram } from '../parser'
import { assertSuccessfulParse } from '../test/parser-utils'
import { walkAstDepthFirst } from './walk-ast'

describe('walkAstDepthFirst', () => {
  test('should correctly walk over an arithmetic expression', () => {
    const source = `1 + 2 * 3`
    const program = parseProgram(source)
    assertSuccessfulParse(program)

    const expected = ['3', '2', '*', '1', '+']
    const stack: string[] = []
    walkAstDepthFirst(program, {
      Integer(node) {
        stack.push(node.value.toString())
      },

      BinaryExpression(node) {
        stack.push(node.operator)
      },
    })

    expect(stack).toEqual(expected)
  })
})
