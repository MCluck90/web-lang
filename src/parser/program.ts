import {
  constant,
  error,
  list,
  maybe,
  Parser,
  text,
  zeroOrMore,
} from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import { ws } from 'parsnip-ts/whitespace'
import {
  createRenderNode,
  createProgramNode,
  createUseSelectorNode,
  createUsePackageNode,
  UseNode,
  UseSelectorNode,
} from './ast'
import { between, token, trailingCommaList, _braces } from './common'
import { _block, _statement } from './expressions'
import { _identifier } from './identifier'
import { _renderKeyword, _useKeyword } from './keywords'
import { _typeDefinition } from './types'

const _pathSeperator = token(/\//y)
const _wildcard = token(/\*/y) as Parser<'*'>

const _useSelector: Parser<UseSelectorNode> = seq([
  _identifier.or(_wildcard),
  maybe(token(/as/y).and(_identifier)),
]).bind(([name, alias]) => {
  if (name === '*' && alias === null) {
    return error('Must give an alias to wildcard imports')
  }

  return constant(createUseSelectorNode(name, alias))
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
      pathPieces === null
        ? ''
        : '/' + pathPieces[0].map((identifier) => identifier.value).join('/'),
      selectors
    )
  })

export const _use: Parser<UseNode> = _useKeyword.and(_usePackage)

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
