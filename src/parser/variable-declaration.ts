import { maybe } from 'parsnip-ts'
import { seq } from 'parsnip-ts/seq'
import { createVariableDeclarationNode } from './ast'
import { token, _colon } from './common'
import { _expression } from './expressions'
import { _identifier } from './identifier'
import { _letKeyword, _mutKeyword } from './keywords'
import { _type } from './types'

export const _variableDeclaration = _letKeyword
  .and(
    seq([
      maybe(_mutKeyword),
      _identifier,
      maybe(_colon.and(_type)),
      token(/=/y),
      _expression,
    ])
  )
  .map(([mut, identifier, type, , initializer]) =>
    createVariableDeclarationNode(identifier, mut !== null, type, initializer)
  )
