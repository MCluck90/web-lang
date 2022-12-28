import { inferEnvironment } from '.'
import { parseProgram } from '../../../parser'
import {
  createBooleanNode,
  createIdentifierNode,
  createIntegerNode,
  createStringNode,
  createVariableAttributeListNode,
  createVariableDeclarationNode,
} from '../../../parser/ast'
import { assertSuccessfulParse } from '../../../test/parser-utils'
import { CustomOrderVisitor } from '../../../utils/ast-visitor'
import { AstNodeWithEnvironment } from './environment-ast'

describe('Environment Inference', () => {
  test.each([
    createIntegerNode(1),
    createBooleanNode(true),
    createStringNode('test'),
  ])('primitives are isomorphic', (node) => {
    expect(inferEnvironment(node).$environment).toBe('isomorphic')
  })

  test('variables with a `Frontend` attribute are marked for the frontend', () => {
    const variableDeclaration = createVariableDeclarationNode(
      [createVariableAttributeListNode([createIdentifierNode('Frontend')])],
      createIdentifierNode('test'),
      false,
      null,
      createIntegerNode(1)
    )
    expect(inferEnvironment(variableDeclaration).$environment).toBe('frontend')
  })

  test('variables with a `Backend` attribute are marked for the backend', () => {
    const variableDeclaration = createVariableDeclarationNode(
      [createVariableAttributeListNode([createIdentifierNode('Backend')])],
      createIdentifierNode('test'),
      false,
      null,
      createIntegerNode(1)
    )
    expect(inferEnvironment(variableDeclaration).$environment).toBe('backend')
  })

  test('variables with both `Frontend` and `Backend` attributes are marked as isomorphic', () => {
    const variableDeclaration = createVariableDeclarationNode(
      [
        createVariableAttributeListNode([
          createIdentifierNode('Frontend'),
          createIdentifierNode('Backend'),
        ]),
      ],
      createIdentifierNode('test'),
      false,
      null,
      createIntegerNode(1)
    )
    expect(inferEnvironment(variableDeclaration).$environment).toBe(
      'isomorphic'
    )
  })

  test('variables declared with a given environment should report that same environment when accessed', () => {
    const source = `
      #[Backend]
      let value = 0

      value
    `
    const program = parseProgram(source)
    assertSuccessfulParse(program)
    inferEnvironment(program)
    let foundVariableAccess = false
    const visitor = new CustomOrderVisitor<AstNodeWithEnvironment>({
      visitVariableAccess(node) {
        foundVariableAccess = true
        expect(node.name.value).toBe('value')
        expect(node.$environment).toBe('backend')
      },
    })
    visitor.visitProgram(program)
    expect(foundVariableAccess).toBe(true)
  })
})
