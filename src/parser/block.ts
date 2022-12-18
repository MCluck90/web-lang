import { ws } from 'parsnip-ts/whitespace'
import { createBlockNode } from './ast'
import { between } from './combinators'
import { _braces } from './common'

// TODO
export const _block = between(_braces, ws).map(() => createBlockNode([]))
