import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import {
  createIdentifierNode,
  createPropertyKeyNode,
  createTypePropertyNode,
  createNamedTypeNode,
} from './ast'
import { _typeDefinition, _namedType, _objectType } from './types'

describe('Object Types', () => {
  test.each(['{}', '{ /* this is a comment */ }', '{ // line comment\n }'])(
    'can parse an empty type block',
    (source) => {
      const objectType = _objectType.parseToEnd(source)
      assertSuccessfulParse(objectType)
      assertNodeType(objectType, 'ObjectType')
      expect(objectType.properties).toHaveLength(0)
    }
  )

  test.each([
    '{ age: int }',
    `
      {
        // age of the person
        age: int
      }
    `,
    '{ /* age of the person */ age: int }',
    '{ age: int /* age of the person */ }',
    '{ age: /* comment */ int }',
  ])('can parse an object type with one property', (source) => {
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

describe('Type Definitions', () => {
  test.each([
    'type Person { age: int }',
    'type /* comment */ Person { age: int }',
    'type Person /* comment */ { age: int }',
    `
    type 
    // comment
    Person {
      age: int
    }
    `,
  ])('can parse a type definition', (source) => {
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

describe('Named Types', () => {
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

  test('can parse a generic argument', () => {
    const source = 'List<int>'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('List')
    expect(type.genericArguments).toHaveLength(1)

    const t = type.genericArguments[0]
    assertNodeType(t, 'NamedType')
    expect(t.name).toBe('int')
  })

  test('can parse 2 generic arguments', () => {
    const source = 'Map<int, string>'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('Map')
    expect(type.genericArguments).toHaveLength(2)

    const t = type.genericArguments[0]
    assertNodeType(t, 'NamedType')
    expect(t.name).toBe('int')

    const u = type.genericArguments[1]
    assertNodeType(u, 'NamedType')
    expect(u.name).toBe('string')
  })

  test('can parse multiple generic arguments', () => {
    const source = 'ComplexType<int, string, bool>'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('ComplexType')
    expect(type.genericArguments).toHaveLength(3)

    const t1 = type.genericArguments[0]
    assertNodeType(t1, 'NamedType')
    expect(t1.name).toBe('int')

    const t2 = type.genericArguments[1]
    assertNodeType(t2, 'NamedType')
    expect(t2.name).toBe('string')

    const t3 = type.genericArguments[2]
    assertNodeType(t3, 'NamedType')
    expect(t3.name).toBe('bool')
  })

  test('can parse nested generic arguments', () => {
    const source = 'Map<int, List<string>>'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('Map')
    expect(type.genericArguments).toHaveLength(2)

    const t1 = type.genericArguments[0]
    assertNodeType(t1, 'NamedType')
    expect(t1.name).toBe('int')

    const t2 = type.genericArguments[1]
    assertNodeType(t2, 'NamedType')
    expect(t2.name).toBe('List')
    expect(t2.genericArguments).toHaveLength(1)

    const t3 = t2.genericArguments[0]
    assertNodeType(t3, 'NamedType')
    expect(t3.name).toBe('string')
  })

  test('can pass object types as generics', () => {
    const source = 'List<{ name: string, age: int, }>'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('List')
    expect(type.genericArguments).toHaveLength(1)

    const t = type.genericArguments[0]
    assertNodeType(t, 'AnonymousType')
    const objectType = t.type
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(2)
  })

  test('can pass arrays of object types as generics', () => {
    const source = 'List<{ name: string, age: int, }[]>'
    const type = _namedType.parseToEnd(source)
    assertSuccessfulParse(type)
    assertNodeType(type, 'NamedType')
    expect(type.name).toBe('List')
    expect(type.genericArguments).toHaveLength(1)

    const t = type.genericArguments[0]
    assertNodeType(t, 'AnonymousType')
    expect(t.isArray).toBe(true)
    const objectType = t.type
    assertNodeType(objectType, 'ObjectType')
    expect(objectType.properties).toHaveLength(2)
  })
})
