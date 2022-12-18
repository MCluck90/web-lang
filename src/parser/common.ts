import { constant, error, Parser } from 'parsnip-ts'
import { createIdentifierNode, IdentifierNode } from './ast'
import { token } from './combinators'
import { _keyword } from './keywords'

export const _comma = token(/,/y)
export const _colon = token(/:/y)
export const _parens = [token(/\(/y), token(/\)/y)] as const
export const _braces = [token(/{/y), token(/}/y)] as const

export const _identifier: Parser<IdentifierNode> = token(
  /[a-z_][a-z0-9_]*/iy
).bind((identifier) =>
  _keyword.matchesToEnd(identifier)
    ? error(`Expected an identifier, got ${identifier}`)
    : constant(createIdentifierNode(identifier))
)
