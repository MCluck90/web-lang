import { constant, Parser } from 'parsnip-ts'
import { createToken } from 'parsnip-ts/token'
import { ws } from 'parsnip-ts/whitespace'

export const between = <T>(
  [start, end]: readonly [Parser<unknown>, Parser<unknown>],
  parser: Parser<T>
): Parser<T> => start.and(parser).bind((result) => end.and(constant(result)))

export const token = createToken(ws)
