import { ProgramNode } from '../parser/ast'
import {
  CompilerConfig,
  CompilerOutput,
  HTMLModule,
  JSModule,
} from './index.types'
import { constantFolding } from './passes/constant-folding'
import { directToJs } from './passes/direct-to-js'
import { generateRootJsFromAssemblyBlocks } from './passes/generate-root-js'
import { renderStaticEntryHtmlPass } from './passes/render-static-entry-html'

const generateHtmlDocument = (body: string) =>
  `
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
  ${body}
</body>
</html>
    `.trim()

export const compileProgram = (
  config: CompilerConfig,
  program: ProgramNode
): CompilerOutput => {
  program = constantFolding(program)

  const jsModules: JSModule[] = []
  const rootJsModule = directToJs(program)
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
