import {
  AstNode,
  createAnonymousTypeNode,
  createBlockNode,
  createIntegerNode,
  createObjectTypeNode,
  createProgramNode,
  createRenderNode,
  createUseRelativeNode,
} from '../parser/ast'
import { CustomOrderVisitor } from './ast-visitor'

describe('CustomOrderVisitor', () => {
  describe('visitAnonymousType', () => {
    test('visits object types', () => {
      const objectType = createObjectTypeNode([])
      const anonymousType = createAnonymousTypeNode(objectType, false)
      const visitedNodes = new Set<AstNode>()
      const visitor = new CustomOrderVisitor({
        visitNode(node) {
          visitedNodes.add(node)
        },
      })
      visitor.visitAnonymousType(anonymousType, [])
      expect(visitedNodes.size).toBe(2)
      expect(visitedNodes.has(objectType)).toBe(true)
    })
  })

  describe('visitProgram', () => {
    test('visits use statements, statements, and the render node', () => {
      const useStatements = [
        createUseRelativeNode('', []),
        createUseRelativeNode('', []),
        createUseRelativeNode('', []),
      ]
      const statements = [
        createIntegerNode(1),
        createIntegerNode(2),
        createIntegerNode(3),
      ]
      const render = createRenderNode(createBlockNode([]))
      const program = createProgramNode(useStatements, statements, render)

      const allNodes = new Set<AstNode>([
        ...useStatements,
        ...statements,
        render,
        program,
      ])

      const visitor = new CustomOrderVisitor({
        visitNode(node) {
          allNodes.delete(node)
        },
      })
      visitor.visitProgram(program)

      expect(allNodes.size).toBe(0)
    })

    test('calls custom visitProgram', () => {
      const program = createProgramNode([], [], null)
      const visitProgram = jest.fn()
      const visitor = new CustomOrderVisitor({
        visitProgram,
      })
      visitor.visitProgram(program)

      expect(visitProgram).toHaveBeenCalledTimes(1)
    })

    test('does not do built-in walking when given a custom visitor', () => {
      const useStatements = [
        createUseRelativeNode('', []),
        createUseRelativeNode('', []),
        createUseRelativeNode('', []),
      ]
      const statements = [
        createIntegerNode(1),
        createIntegerNode(2),
        createIntegerNode(3),
      ]
      const render = createRenderNode(createBlockNode([]))
      const program = createProgramNode(useStatements, statements, render)

      const visitedNodes = new Set<AstNode>()

      const visitor = new CustomOrderVisitor({
        visitProgram(node) {
          visitedNodes.add(node)
        },
      })
      visitor.visitProgram(program)

      expect(visitedNodes.size).toBe(1)
      expect(visitedNodes.has(program)).toBe(true)
    })
  })
})
