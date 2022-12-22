import { zeroOrMore } from 'parsnip-ts'
import { ws } from 'parsnip-ts/whitespace'
import { createBlockNode } from './ast'
import { between, _braces } from './common'
import { _expression } from './expressions'
import { _statement } from './statement'
import { _variableDeclaration } from './variable-declaration'

export const _block = between(
  _braces,
  zeroOrMore(between([ws, ws], _statement)).map((statements) =>
    statements.filter((s): s is Exclude<typeof s, null> => s !== null)
  )
).map(createBlockNode)
