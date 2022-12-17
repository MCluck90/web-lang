import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import {
  createIdentifierNode,
  createPropertyKeyNode,
  createTypePropertyNode,
  createNamedTypeNode,
} from './ast'
import {
  _typeDefinition,
  _typeProperty,
  _namedType,
  _objectType,
} from './types'

describe('_objectType', () => {
  test('can parse an empty type block', () => {
    const source = '{}'
    const objectType = _objectType.parseToEnd(source)
    assertSuccessfulParse(objectType)
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(0)
  })

  test('can parse an object type with one property', () => {
    const source = '{ age: int }'
    const objectType = _objectType.parseToEnd(source)
    assertSuccessfulParse(objectType)
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(1)
    expect(objectType.properties[0]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('age')),
        createNamedTypeNode('int', [], false)
      )
    )
  })

  test('can parse an object type with more than one property', () => {
    const source = `
      {
        age: int,
        name: string,
        address: Address
      }
    `
    const objectType = _objectType.parseToEnd(source)
    assertSuccessfulParse(objectType)
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(3)
    expect(objectType.properties[0]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('age')),
        createNamedTypeNode('int', [], false)
      )
    )
    expect(objectType.properties[1]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('name')),
        createNamedTypeNode('string', [], false)
      )
    )
    expect(objectType.properties[2]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('address')),
        createNamedTypeNode('Address', [], false)
      )
    )
  })

  test('can parse an object type with a trailing comma', () => {
    const source = `
      {
        age: int,
        name: string,
        address: Address,
      }
    `
    const objectType = _objectType.parseToEnd(source)
    assertSuccessfulParse(objectType)
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(3)
    expect(objectType.properties[0]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('age')),
        createNamedTypeNode('int', [], false)
      )
    )
    expect(objectType.properties[1]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('name')),
        createNamedTypeNode('string', [], false)
      )
    )
    expect(objectType.properties[2]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('address')),
        createNamedTypeNode('Address', [], false)
      )
    )
  })
})

describe('_typeDefinition', () => {
  test('can parse a type definition', () => {
    const source = 'type Person { age: int }'
    const typeDefinition = _typeDefinition.parseToEnd(source)
    assertSuccessfulParse(typeDefinition)
    assertNodeType(typeDefinition, 'TypeDefinition')
    expect(typeDefinition.name).toEqual(createIdentifierNode('Person'))

    const objectType = typeDefinition.type
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties[0].name.value.value).toBe('age')

    const intType = objectType.properties[0].type
    assertNodeType(intType, 'NamedType')
    expect(intType.name).toBe('int')
  })
})

describe('_namedType', () => {
  test('can parse built-in types', () => {
    const source = 'int'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('int')
  })

  test('can parse custom types', () => {
    const source = 'Address'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('Address')
  })

  test('can parse array types', () => {
    const source = 'int[]'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('int')
    expect(type.isArray).toBe(true)
  })
})
