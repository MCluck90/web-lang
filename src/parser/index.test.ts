import { parseProgram } from '.'
import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'

describe('parseProgram', () => {
  test('can parse an empty program', () => {
    const source = ''
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.typeDefinitions).toHaveLength(0)
    expect(program.remoteDefinitions).toHaveLength(0)
  })

  test('can parse an empty program with random whitespace', () => {
    const source = ' \r\n\t  \n\r\t \t\n '
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.typeDefinitions).toHaveLength(0)
    expect(program.remoteDefinitions).toHaveLength(0)
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
    expect(program.typeDefinitions).toHaveLength(1)
    expect(program.remoteDefinitions).toHaveLength(1)
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
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    assertNodeType(program, 'Program')
    expect(program.typeDefinitions).toHaveLength(2)
    expect(program.remoteDefinitions).toHaveLength(2)
  })
})
