import { ProgramNode } from '../parser/ast'
import {
  CompilerConfig,
  CompilerOutput,
  HTMLModule,
  JSModule,
} from './index.types'
import { constantFolding } from './passes/constant-folding'
import { emitJs } from './passes/emit-js'
import { renderStaticEntryHtmlPass } from './passes/render-static-entry-html'

export const compileProgram = (
  config: CompilerConfig,
  program: ProgramNode
): CompilerOutput => {
  program = constantFolding(program)

  const jsModules: JSModule[] = []
  const rootJsModule = emitJs(program)
  if (rootJsModule !== null) {
    jsModules.push(rootJsModule)
  }

  const htmlModules: HTMLModule[] = []

  const staticHtml = renderStaticEntryHtmlPass({
    program,
    startupJsModules: jsModules,
  })
  if (staticHtml !== null) {
    htmlModules.push(staticHtml)
  }
  return {
    html: htmlModules,
    isomorphicJs: jsModules,
  }
}
