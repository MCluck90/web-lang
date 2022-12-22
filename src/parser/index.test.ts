import { parseProgram } from '.'
import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'

describe('parseProgram', () => {
  test('can parse program with only a render block', () => {
    const source = 'render {}'
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(0)
    expect(program.render).not.toBe(null)
  })

  test('can parse a program with a type definition and a remote', () => {
    const source = `
      type Todo { title: string }

      remote GetTodo {
        GET /todo

        response: Todo
      }
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(2)
  })

  test('can parse a program with mixed type definitions and remotes', () => {
    const source = `
      type Todo { title: string }

      remote GetTodo {
        GET /todo

        response: Todo
      }

      type Person {
        name: string,
        age: int,
      }

      remote PostPerson {
        POST /person
        body: Person
      }

      render {}
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.statements).toHaveLength(4)
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
