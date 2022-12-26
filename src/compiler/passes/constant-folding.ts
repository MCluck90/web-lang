import {
  ASTNode,
  BinaryExpressionNode,
  createFloatingPointNode,
  createIntegerNode,
  ExpressionNode,
  isNodeType,
  ProgramNode,
} from '../../parser/ast'
import { DepthFirstVisitor } from '../../utils/ast-visitor'

export type Input = ProgramNode
export type Output = ProgramNode

export const constantFolding = (program: Input): Output => {
  const foldConstants = (node: BinaryExpressionNode): ExpressionNode | void => {
    const left = isNodeType('BinaryExpression')(node.left)
      ? foldConstants(node.left)
      : node.left
    const right = isNodeType('BinaryExpression')(node.right)
      ? foldConstants(node.right)
      : node.right
    if (isNodeType('Integer')(left) && isNodeType('Integer')(right)) {
      switch (node.operator) {
        case '*':
          return createIntegerNode(left.value * right.value)
        case '+':
          return createIntegerNode(left.value + right.value)
        case '-':
          return createIntegerNode(left.value - right.value)
        case '/':
          return createIntegerNode(Math.floor(left.value / right.value))
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
  }
  const visitor = new DepthFirstVisitor<ASTNode>({
    visitBinaryExpression: foldConstants,
  })

  visitor.visitProgram(program)

  return program
}
