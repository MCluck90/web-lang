import { _jsAsmKeyword } from './keywords'
import { blockComment } from 'parsnip-ts/comments'
import { ws } from 'parsnip-ts/whitespace'
import { createJsAsmNode } from './ast'

const _captureJS = blockComment('(*', '*)').map((s) => s.slice(2, -2))
export const _jsAsm = _jsAsmKeyword
  .and(ws.and(_captureJS))
  .skip(ws)
  .map(createJsAsmNode)
