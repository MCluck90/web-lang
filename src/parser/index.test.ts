import { parseProgram } from '.'
import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'

describe('Programs', () => {
  test('can parse program with only a render block', () => {
    const source = 'render {}'
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(0)
    expect(program.render).not.toBe(null)
  })

  test('can parse a program with a type definition', () => {
    const source = `
      type Todo { title: string }
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(1)
  })

  test('can parse a program with a type definition and a render block', () => {
    const source = `
      type Todo { title: string }

      render {}
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(1)
    expect(program.render).not.toBeNull()
  })

  test('can parse arbitrary expressions', () => {
    const source = `
      x + 2
      Stdout.writeLn("Hello")
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(2)
  })
})
