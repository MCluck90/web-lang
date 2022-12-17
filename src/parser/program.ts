import { constant, zeroOrMore } from 'parsnip-ts'
import { ws } from 'parsnip-ts/whitespace'
import { createProgramNode, isNodeType } from './ast'
import { _remoteDefinition } from './remote'
import { _typeDefinition } from './types'

export const _program = zeroOrMore(_typeDefinition.or(_remoteDefinition)).bind(
  (typesAndRemotes) => {
    const typeDefinitions = typesAndRemotes.filter(isNodeType('TypeDefinition'))
    const remoteDefinitions = typesAndRemotes.filter(
      isNodeType('RemoteDefinition')
    )
    return ws.and(
      constant(createProgramNode(typeDefinitions, remoteDefinitions))
    )
  }
)
