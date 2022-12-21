import * as fs from 'fs'
import { ParseError } from 'parsnip-ts/error'
import * as path from 'path'
import { compileProgram } from './compiler'
import { CompilerConfig } from './compiler/index.types'
import { parseProgram } from './parser'

const usage = () => {
  process.stderr.write('Usage: yarn start [parse] <file>\n')
}

if (process.argv.length < 4) {
  usage()
  process.exit(1)
}

const [, , subcommand, filePath] = process.argv
const testConfig: CompilerConfig = {
  projectName: 'Test',
}
switch (subcommand) {
  case 'compile':
  case 'parse': {
    const fullFilePath = path.join(process.cwd(), filePath)
    const source = fs.readFileSync(fullFilePath).toString()
    const result = parseProgram(source)
    if (result instanceof ParseError) {
      console.error(result)
      process.exit(1)
    }

    if (subcommand === 'parse') {
      console.log(JSON.stringify(result, null, 2))
      break
    }

    const parseResult = parseProgram(source)
    if (parseResult instanceof ParseError) {
      console.error(parseResult)
      process.exit(1)
    }

    const compileResult = compileProgram(testConfig, parseResult)

    const outputDirectory = path.join(process.cwd(), '_build')
    if (!fs.existsSync(outputDirectory)) {
      fs.mkdirSync(outputDirectory)
    }
    for (const htmlModule of compileResult.html) {
      const htmlPath = path.join(
        outputDirectory,
        `${htmlModule.moduleName}.html`
      )
      fs.writeFileSync(htmlPath, htmlModule.contents)
    }

    const frontendJsDirectory = path.join(outputDirectory, 'frontend-js')
    if (
      compileResult.frontendJs.length > 0 &&
      !fs.existsSync(frontendJsDirectory)
    ) {
      fs.mkdirSync(frontendJsDirectory)
    }
    for (const jsModule of compileResult.frontendJs) {
      const htmlPath = path.join(outputDirectory, `${jsModule.moduleName}.js`)
      fs.writeFileSync(htmlPath, jsModule.contents)
    }
    console.log('Compilation succeeded')
    break
  }

  default:
    usage()
    process.exit(1)
}
