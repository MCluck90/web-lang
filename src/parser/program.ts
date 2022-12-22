import { maybe, zeroOrMore } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import { ws } from 'parsnip-ts/whitespace'
import { createRenderNode, createProgramNode, isNodeType } from './ast'
import { _block } from './block'
import { between, _braces } from './common'
import { _renderKeyword } from './keywords'
import { _methodDefinition } from './methods'
import { _remoteDefinition } from './remote'
import { _statement } from './statement'
import { _typeDefinition } from './types'

export const _render = _renderKeyword.and(_block).map(createRenderNode)

export const _program = seq([
  zeroOrMore(
    between([ws, ws], _typeDefinition.or(_remoteDefinition).or(_statement))
  ).map((statements) =>
    statements.filter((s): s is Exclude<typeof s, null> => s !== null)
  ),
  ws.and(maybe(_render.skip(ws))),
]).map(([statements, render]) => {
  return createProgramNode(statements, render)
})
