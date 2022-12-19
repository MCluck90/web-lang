import * as fs from 'fs'
import { ParseError } from 'parsnip-ts/error'
import * as path from 'path'
import { parseProgram } from './parser'

const usage = () => {
  process.stderr.write('Usage: yarn start [parse] <file>\n')
}

if (process.argv.length < 4) {
  usage()
  process.exit(1)
}

const [, , subcommand, filePath] = process.argv
if (subcommand !== 'parse') {
  usage()
  process.exit(1)
}

const fullFilePath = path.join(process.cwd(), filePath)
const source = fs.readFileSync(fullFilePath).toString()
const result = parseProgram(source)
if (result instanceof ParseError) {
  console.error(result)
  process.exit(1)
}

console.log(JSON.stringify(result, null, 2))
