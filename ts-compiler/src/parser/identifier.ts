import { Parser, error, constant } from 'parsnip-ts'
import { IdentifierNode, createIdentifierNode } from './ast'
import { token } from './common'
import { _keyword } from './keywords'

export const _identifier: Parser<IdentifierNode> = token(
  /[a-z_][a-z0-9_]*/iy
).bind((identifier) =>
  _keyword.matchesToEnd(identifier)
    ? error(`Expected an identifier, got ${identifier}`)
    : constant(createIdentifierNode(identifier))
)
