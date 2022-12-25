import { list, maybe, Parser, text, zeroOrMore } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import { ws } from 'parsnip-ts/whitespace'
import {
  createRenderNode,
  createProgramNode,
  createUseSelectorNode,
  createUsePackageNode,
} from './ast'
import { between, token, trailingCommaList, _braces } from './common'
import { _block, _statement } from './expressions'
import { _identifier } from './identifier'
import { _renderKeyword, _useKeyword } from './keywords'
import { _typeDefinition } from './types'

const _pathSeperator = token(/\//y)
const _wildcard = token(/\*/y) as Parser<'*'>

const _useSelector = seq([
  _identifier.or(_wildcard),
  maybe(token(/as/y).and(_identifier)),
]).map(([name, alias]) => {
  if (name === '*' && alias === null) {
    throw new Error('Must give an alias to wildcard imports')
  }

  return createUseSelectorNode(name, alias)
})
const _useSelectors = between(_braces, trailingCommaList(_useSelector))

const _usePackage = token(/@/y)
  .and(
    seq([
      _identifier,
      text(':'),
      _identifier,
      text('/'),
      maybe(seq([list(_identifier, _pathSeperator), _pathSeperator])),
      _useSelectors,
    ])
  )
  .map(([scope, , package_, , pathPieces, selectors]) => {
    return createUsePackageNode(
      scope.value,
      package_.value,
      `/${pathPieces ? pathPieces[0].join('/') : ''}`,
      selectors
    )
  })

export const _use = _useKeyword.and(_usePackage)

export const _render = _renderKeyword.and(_block).map(createRenderNode)

export const _program = seq([
  zeroOrMore(_use),
  zeroOrMore(between([ws, ws], _typeDefinition.or(_statement))).map(
    (statements) =>
      statements.filter((s): s is Exclude<typeof s, null> => s !== null)
  ),
  ws.and(maybe(_render.skip(ws))),
]).map(([useStatements, statements, render]) => {
  return createProgramNode(useStatements, statements, render)
})
