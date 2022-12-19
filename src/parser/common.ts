import {
  Parser,
  constant,
  zeroOrMore,
  error,
  pair,
  maybe,
  list,
} from 'parsnip-ts'
import { cStyleComment } from 'parsnip-ts/comments'
import { createToken } from 'parsnip-ts/token'
import { ws } from 'parsnip-ts/whitespace'
import { IdentifierNode, createIdentifierNode } from './ast'

export const between = <T>(
  [start, end]: readonly [Parser<unknown>, Parser<unknown>],
  parser: Parser<T>
): Parser<T> => start.and(parser).bind((result) => end.and(constant(result)))

export const token = createToken(zeroOrMore(cStyleComment.or(ws)))

export const _comma = token(/,/y)
export const _colon = token(/:/y)
export const _parens = [token(/\(/y), token(/\)/y)] as const
export const _braces = [token(/{/y), token(/}/y)] as const

export const commaDelimited = <T>(parser: Parser<T>): Parser<T[]> =>
  pair(maybe(list(parser, _comma)), maybe(_comma)).map(([first]) => first ?? [])
