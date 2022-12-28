import { isNodeType } from '../../parser/ast'
import { JSModule } from '../index.types'
import { Input, Output } from './generate-root-js.types'

export const generateRootJsFromAssemblyBlocks = (program: Input): Output => {
  let contents = ''
  for (const statement of program.statements) {
    if (isNodeType('JsAsm')(statement)) {
      contents += statement.code
    }
  }

  if (contents) {
    return new JSModule('main', contents)
  }
  return null
}
