import { pair, list, maybe, Parser, error, lazy, constant } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import {
  createPropertyKeyNode,
  createTypeDefinitionNode,
  createTypePropertyNode,
  createNamedTypeNode,
  TypeNode,
  createObjectTypeNode,
} from './ast'
import { token, between } from './combinators'
import { _identifier } from './common'
import { _builtInTypeKeyword, _typeKeyword } from './keywords'

export let _type: Parser<TypeNode> = error('Not yet implemented')

const _typeName = _builtInTypeKeyword.or(_identifier.map((i) => i.value))
const _genericArguments = between(
  [token(/</y), token(/>/y)],
  maybe(
    list(
      lazy(() => _type),
      token(/,/y)
    )
  ).map((genericArguments) => genericArguments ?? [])
)
const _isArray = token(/\[]/y).and(constant(true)).or(constant(false))
export const _namedType = pair(_typeName, maybe(_genericArguments)).bind(
  ([name, genericArgs]) =>
    _isArray.map((isArray) =>
      createNamedTypeNode(name, genericArgs ?? [], isArray)
    )
)

export const _propertyKey = _identifier.map(createPropertyKeyNode)

export const _typeProperty = pair(
  _propertyKey,
  token(/:/y).and(lazy(() => _type))
).map((args) => createTypePropertyNode(...args))

export const _objectType = between(
  [token(/{/y), maybe(token(/,/y)).and(token(/}/y))],
  maybe(list(_typeProperty, token(/,/y))).map((properties) => properties ?? [])
).map(createObjectTypeNode)

export const _anonymousType = _objectType

_type = _namedType.or(_anonymousType)

export const _typeDefinition = _typeKeyword
  .and(pair(_identifier, _objectType))
  .map((args) => createTypeDefinitionNode(...args))
