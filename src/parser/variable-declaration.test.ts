import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import { createNamedTypeNode, createStringNode } from './ast'
import { _variableDeclaration } from './variable-declaration'

describe('_variableDeclaration', () => {
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
