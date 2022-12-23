import { execSync, spawn, spawnSync } from 'child_process'
import * as fs from 'fs'
import { ParseError } from 'parsnip-ts/error'
import * as path from 'path'
import { compileProgram } from './compiler'
import { CompilerConfig } from './compiler/index.types'
import { parseProgram } from './parser'
import { DepthFirstVisitor } from './utils/ast-visitor'

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

const outputDirectory = path.join(process.cwd(), '_build')
switch (subcommand) {
  case 'rebuild':
    if (fs.existsSync(outputDirectory)) {
      fs.rmSync(outputDirectory, { recursive: true })
    }

  case 'compile':
  case 'run':
  case 'parse': {
    const fullFilePath = path.join(process.cwd(), filePath)
    const source = fs.readFileSync(fullFilePath).toString()
    const program = parseProgram(source)
    if (program instanceof ParseError) {
      console.error(program)
      process.exit(1)
    }

    if (subcommand === 'parse') {
      console.log(JSON.stringify(program, null, 2))
      break
    }

    const compileResult = compileProgram(testConfig, program)
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

    const isomorphicJsDirectory = path.join(outputDirectory, 'isomorphic-js')
    if (
      compileResult.isomorphicJs.length > 0 &&
      !fs.existsSync(isomorphicJsDirectory)
    ) {
      fs.mkdirSync(isomorphicJsDirectory)
    }
    for (const jsModule of compileResult.isomorphicJs) {
      const htmlPath = path.join(
        isomorphicJsDirectory,
        `${jsModule.moduleName}.js`
      )
      fs.writeFileSync(htmlPath, jsModule.contents)
    }
    console.log('Compilation succeeded')
    break
  }

  default:
    usage()
    process.exit(1)
}

if (subcommand === 'run') {
  console.log('==================\n\n')
  const node = spawn('node', ['_build/isomorphic-js/main.js'])
  node.stdout.on('data', (data) => process.stdout.write(data))
  node.stderr.on('data', (data) => process.stderr.write(data))
  node.on('error', (error) => console.error(error))
}
