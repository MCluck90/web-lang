import { ProgramNode } from '../parser/ast'
import { CompilerConfig, CompilerOutput, HTMLModule } from './index.types'
import { constantFolding } from './passes/constant-folding'
import { emitJs } from './passes/emit-js'
import { inferEnvironment } from './passes/environment-inference'
import { renderStaticEntryHtmlPass } from './passes/render-static-entry-html'
import { inferTypes } from './passes/type-inference'

export const compileProgram = (
  config: CompilerConfig,
  program: ProgramNode
): CompilerOutput => {
  const typedProgram = inferTypes(program)
  const withEnvironment = inferEnvironment(typedProgram)
  const finalProgram = constantFolding(withEnvironment)

  const rootJsModules = emitJs(finalProgram)

  const htmlModules: HTMLModule[] = []
  const staticHtml = renderStaticEntryHtmlPass({
    program,
    startupJsModules: rootJsModules.frontend ? [rootJsModules.frontend] : [],
  })
  if (staticHtml !== null) {
    htmlModules.push(staticHtml)
  }
  return {
    html: htmlModules,
    isomorphicJs: rootJsModules.isomorphic ? [rootJsModules.isomorphic] : [],
    frontendJs: rootJsModules.frontend ? [rootJsModules.frontend] : [],
    backendJs: rootJsModules.backend ? [rootJsModules.backend] : [],
  }
}
