import { token } from './common'
import { _expression } from './expressions'
import { _variableDeclaration } from './variable-declaration'

export const _statement = _variableDeclaration
  .or(_expression)
  .or(token(/;/y).map(() => null))
