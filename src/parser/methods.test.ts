import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import { assertNotNull } from '../test/utils'
import { _methodDefinition } from './methods'

describe('_methodDefinition', () => {
  test('can parse an empty method', () => {
    const source = 'hello() { }'
    const methodDefinition = _methodDefinition.parseToEnd(source)
    assertSuccessfulParse(methodDefinition)
    assertNodeType(methodDefinition, 'MethodDefinition')
    expect(methodDefinition.name.value).toBe('hello')
    expect(methodDefinition.parameterList.parameters).toHaveLength(0)
    expect(methodDefinition.body.statements).toHaveLength(0)
  })

  test('can parse a method with an implicitly typed parameter', () => {
    const source = 'hello(world) { }'
    const methodDefinition = _methodDefinition.parseToEnd(source)
    assertSuccessfulParse(methodDefinition)
    assertNodeType(methodDefinition, 'MethodDefinition')
    expect(methodDefinition.name.value).toBe('hello')

    expect(methodDefinition.parameterList.parameters).toHaveLength(1)
    const parameter = methodDefinition.parameterList.parameters[0]
    expect(parameter.name.value).toBe('world')
    expect(parameter.type).toBe(null)
  })

  test('can parse a method with an explicitly typed parameter', () => {
    const source = 'hello(world: string) { }'
    const methodDefinition = _methodDefinition.parseToEnd(source)
    assertSuccessfulParse(methodDefinition)
    assertNodeType(methodDefinition, 'MethodDefinition')
    expect(methodDefinition.name.value).toBe('hello')

    expect(methodDefinition.parameterList.parameters).toHaveLength(1)
    const parameter = methodDefinition.parameterList.parameters[0]
    expect(parameter.name.value).toBe('world')

    const type = parameter.type
    assertNotNull(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('string')
  })

  test('can accept anonymous types for parameters', () => {
    const source = 'greet(person: { name: string }) { }'
    const methodDefinition = _methodDefinition.parseToEnd(source)
    assertSuccessfulParse(methodDefinition)
    assertNodeType(methodDefinition, 'MethodDefinition')
    expect(methodDefinition.name.value).toBe('greet')

    expect(methodDefinition.parameterList.parameters).toHaveLength(1)
    const parameter = methodDefinition.parameterList.parameters[0]
    expect(parameter.name.value).toBe('person')

    const anonymousType = parameter.type
    assertNotNull(anonymousType)
    assertNodeType(anonymousType, 'AnonymousType')
    const objectType = anonymousType.type
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(1)
  })

  test('can specify a return type', () => {
    const source = 'getCount(): int {}'
    const methodDefinition = _methodDefinition.parseToEnd(source)
    assertSuccessfulParse(methodDefinition)
    assertNodeType(methodDefinition, 'MethodDefinition')
    expect(methodDefinition.name.value).toBe('getCount')
    const returnType = methodDefinition.returnType
    assertNotNull(returnType)
    assertNodeType(returnType, 'NamedType')
    expect(returnType.name).toBe('int')
  })

  test('can parse parameter list with hanging comma', () => {
    const source = `
      add(
        x,
        y,
      ) {}
    `

    const methodDefinition = _methodDefinition.parseToEnd(source)
    assertSuccessfulParse(methodDefinition)
    assertNodeType(methodDefinition, 'MethodDefinition')
    expect(methodDefinition.parameterList.parameters).toHaveLength(2)
  })
})
