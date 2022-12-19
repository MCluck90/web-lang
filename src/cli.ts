import * as fs from 'fs'
import { ParseError } from 'parsnip-ts/error'
import * as path from 'path'
import { compileProgram } from './compiler'
import { parseProgram } from './parser'

const usage = () => {
  process.stderr.write('Usage: yarn start [parse] <file>\n')
}

if (process.argv.length < 4) {
  usage()
  process.exit(1)
}

const [, , subcommand, filePath] = process.argv
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

    const compileResult = compileProgram(parseResult)
    fs.writeFileSync(path.basename(fullFilePath) + '.html', compileResult.html)
    console.log('Compilation succeeded')
    break
  }

  default:
    usage()
    process.exit(1)
}
