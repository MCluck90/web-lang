import {
  assertFailedParse,
  assertNodeType,
  assertSuccessfulParse,
} from '../test/parser-utils'
import { _jsAsm } from './js-asm'

describe('_jsAsm', () => {
  test('can extract a chunk of JavaScript code', () => {
    const source = `
      %js-asm% (*
        console.log('Hello world')
      *)
    `
    const jsAsm = _jsAsm.parseToEnd(source)
    assertSuccessfulParse(jsAsm)
    assertNodeType(jsAsm, 'JsAsm')
    expect(jsAsm.code.trim()).toBe("console.log('Hello world')")
  })

  test('supports nested blocks', () => {
    const source = `%js-asm% (* {} *)`
    const jsAsm = _jsAsm.parseToEnd(source)
    assertSuccessfulParse(jsAsm)
    assertNodeType(jsAsm, 'JsAsm')
    expect(jsAsm.code.trim()).toBe('{}')
  })
})
