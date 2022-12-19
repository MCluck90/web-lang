import { maybe } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import {
  createMethodDefinitionNode,
  createParameterListNode,
  createParameterNode,
} from './ast'
import { _block } from './block'
import { between, trailingCommaList, _colon, _parens } from './common'
import { _identifier } from './identifier'
import { _type } from './types'

const _parameter = seq([_identifier, maybe(_colon.and(_type))]).map(
  ([name, type]) => createParameterNode(name, type)
)
const _parameterList = between(_parens, trailingCommaList(_parameter)).map(
  createParameterListNode
)
export const _methodDefinition = seq([
  _identifier,
  _parameterList,
  maybe(_colon.and(_type)),
  _block,
]).map((args) => createMethodDefinitionNode(...args))
