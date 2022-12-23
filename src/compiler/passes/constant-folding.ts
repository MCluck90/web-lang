import {
  createFloatingPointNode,
  createIntegerNode,
  isNodeType,
  ProgramNode,
} from '../../parser/ast'
import { DepthFirstVisitor } from '../../utils/ast-visitor'

export type Input = ProgramNode
export type Output = ProgramNode

export const constantFolding = (program: Input): Output => {
  const visitor = new DepthFirstVisitor({
    visitBinaryExpression(node) {
      const left = node.left
      const right = node.right
      if (isNodeType('Integer')(left) && isNodeType('Integer')(right)) {
        switch (node.operator) {
          case '*':
            return createIntegerNode(left.value * right.value)
          case '+':
            return createIntegerNode(left.value + right.value)
          case '-':
            return createIntegerNode(left.value - right.value)
          case '/':
            return createIntegerNode(left.value / right.value)
        }
      } else if (
        isNodeType('FloatingPoint')(left) &&
        isNodeType('FloatingPoint')(right)
      ) {
        switch (node.operator) {
          case '*':
            return createFloatingPointNode(left.value * right.value)
          case '+':
            return createFloatingPointNode(left.value + right.value)
          case '-':
            return createFloatingPointNode(left.value - right.value)
          case '/':
            return createFloatingPointNode(left.value / right.value)
        }
      }
    },
  })

  visitor.visitProgram(program)

  return program
}
