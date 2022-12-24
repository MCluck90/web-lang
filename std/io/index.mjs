import * as fs from 'fs'
import * as path from 'path'

export const readFileByLine = (file) =>
  fs.readFileSync(path.join(process.cwd(), file)).toString().split('\n')

const _console = console
export { _console as console }
