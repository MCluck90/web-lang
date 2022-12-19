import { constant, zeroOrMore } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import { ws } from 'parsnip-ts/whitespace'
import { createMainNode, createProgramNode, isNodeType } from './ast'
import { between, _braces } from './common'
import { _mainKeyword } from './keywords'
import { _methodDefinition } from './methods'
import { _remoteDefinition } from './remote'
import { _typeDefinition } from './types'

const _mainBody = between(_braces, zeroOrMore(_methodDefinition))
const _main = _mainKeyword.and(_mainBody).map(createMainNode)

export const _program = seq([
  zeroOrMore(between([ws, ws], _typeDefinition.or(_remoteDefinition))),
  _main,
]).map(([typesAndRemotes, main]) => {
  const typeDefinitions = typesAndRemotes.filter(isNodeType('TypeDefinition'))
  const remoteDefinitions = typesAndRemotes.filter(
    isNodeType('RemoteDefinition')
  )
  return createProgramNode(typeDefinitions, remoteDefinitions, main)
})
