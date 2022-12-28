import {
  AstNode,
  BinaryExpressionNode,
  createFloatingPointNode,
  createIntegerNode,
  ExpressionNode,
  isNodeType,
} from '../../parser/ast'
import { DepthFirstVisitor } from '../../utils/ast-visitor'

export const constantFolding = <T extends AstNode>(node: T): T => {
  const foldConstants = (node: BinaryExpressionNode): ExpressionNode => {
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
    return node
  }
  const visitor = new DepthFirstVisitor<AstNode>({
    visitBinaryExpression: foldConstants,
  })

  // This is safe. TypeScript just can't prove it.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return (visitor[`visit${node.__type}`] as any)(node as never, [])
}
