import { token } from './common'
import { _expression } from './expressions'
import { _jsAsm } from './js-asm'
import { _variableDeclaration } from './variable-declaration'

export const _statement = _variableDeclaration
  .or(_expression)
  .or(_jsAsm)
  .or(token(/;/y).map(() => null))
