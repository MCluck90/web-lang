import { ProgramNode } from '../parser/ast'
import { CompilerConfig, CompilerOutput, HTMLModule } from './index.types'
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
  const result = renderStaticEntryHtmlPass(program)

  const htmlModules: HTMLModule[] = []
  if (result !== null) {
    htmlModules.push(result)
  }
  return {
    html: htmlModules,
    frontendJs: [],
  }
}
