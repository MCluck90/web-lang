import * as fs from 'fs'
import * as path from 'path'
import { parseProgram } from '../src/parser'
import { assertSuccessfulParse } from '../src/test/parser-utils'

const examplesDir = path.join(__dirname, '../examples')
const examples = fs.readdirSync(examplesDir).filter((f) => f.endsWith('.web'))

test.concurrent.each(examples)('parse %s', (exampleName) => {
  const source = fs.readFileSync(path.join(examplesDir, exampleName)).toString()
  assertSuccessfulParse(parseProgram(source))
})
