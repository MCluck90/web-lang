import { list, maybe, Parser, error, lazy, constant } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import {
  createPropertyKeyNode,
  createTypeDefinitionNode,
  createTypePropertyNode,
  createNamedTypeNode,
  TypeNode,
  createObjectTypeNode,
  createAnonymousTypeNode,
} from './ast'
import { between, token, _colon } from './common'
import { _identifier } from './identifier'
import { _builtInTypeKeyword, _typeKeyword } from './keywords'

export let _type: Parser<TypeNode> = error('Not yet implemented')

const _typeName = _builtInTypeKeyword.or(_identifier.map((i) => i.value))
const _genericArguments = between(
  [token(/</y), token(/>/y)],
  list(
    lazy(() => _type),
    token(/,/y)
  )
)
const _isArray = token(/\[]/y).and(constant(true)).or(constant(false))
export const _namedType = seq([
  _typeName,
  maybe(_genericArguments),
  _isArray,
]).map(([name, genericArgs, isArray]) =>
  createNamedTypeNode(name, genericArgs ?? [], isArray)
)

export const _propertyKey = _identifier.map(createPropertyKeyNode)

export const _typeProperty = seq([
  _propertyKey,
  _colon.and(lazy(() => _type)),
]).map((args) => createTypePropertyNode(...args))

export const _objectType = between(
  [token(/{/y), maybe(token(/,/y)).and(token(/}/y))],
  maybe(list(_typeProperty, token(/,/y))).map((properties) => properties ?? [])
).map(createObjectTypeNode)

export const _anonymousType = seq([_objectType, _isArray]).map(
  ([type, isArray]) => createAnonymousTypeNode(type, isArray)
)

_type = _namedType.or(_anonymousType)

export const _typeDefinition = _typeKeyword
  .and(seq([_identifier, _objectType]))
  .map((args) => createTypeDefinitionNode(...args))
