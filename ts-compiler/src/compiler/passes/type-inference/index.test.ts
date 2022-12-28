import { inferTypes } from '.'
import {
  createBooleanNode,
  createIntegerNode,
  createStringNode,
} from '../../../parser/ast'
import { createConcreteNamedType } from './typed-ast'

describe('Type Inference', () => {
  describe('Literal Values', () => {
    test('infers booleans as booleans', () => {
      const typedBoolean = inferTypes(createBooleanNode(true))
      expect(typedBoolean.$type).toEqual(createConcreteNamedType('bool', []))
    })

    test('infers integers as integers', () => {
      const typedInteger = inferTypes(createIntegerNode(1))
      expect(typedInteger.$type).toEqual(createConcreteNamedType('int', []))
    })

    test('infers strings as strings', () => {
      const typedString = inferTypes(createStringNode('hello'))
      expect(typedString.$type).toEqual(createConcreteNamedType('string', []))
    })
  })
})
