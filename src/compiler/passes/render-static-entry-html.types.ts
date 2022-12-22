import { ProgramNode } from '../../parser/ast'
import { HTMLModule, JSModule } from '../index.types'

export type Input = { program: ProgramNode; startupJsModules: JSModule[] }
export type Output = HTMLModule | null
