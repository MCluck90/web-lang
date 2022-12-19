import { zeroOrMore } from 'parsnip-ts'
import { ws } from 'parsnip-ts/whitespace'
import { createBlockNode, ExpressionNode } from './ast'
import { between, token, _braces } from './common'
import { _expression } from './expressions'

export const _block = between(
  _braces,
  zeroOrMore(between([ws, ws], _expression.or(token(/;/y)))).map(
    (expressionsOrTerminators) =>
      // TODO: If last one is a terminator, map to a void value so we get that for the return type
      expressionsOrTerminators.filter(
        (s): s is ExpressionNode => typeof s !== 'string'
      )
  )
).map(createBlockNode)
