import { maybe, zeroOrMore } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import { ws } from 'parsnip-ts/whitespace'
import { createRenderNode, createProgramNode } from './ast'
import { between, _braces } from './common'
import { _block, _statement } from './expressions'
import { _renderKeyword } from './keywords'
import { _methodDefinition } from './methods'
import { _typeDefinition } from './types'

export const _render = _renderKeyword.and(_block).map(createRenderNode)

export const _program = seq([
  zeroOrMore(between([ws, ws], _typeDefinition.or(_statement))).map(
    (statements) =>
      statements.filter((s): s is Exclude<typeof s, null> => s !== null)
  ),
  ws.and(maybe(_render.skip(ws))),
]).map(([statements, render]) => {
  return createProgramNode(statements, render)
})
