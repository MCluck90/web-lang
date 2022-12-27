import {
  createConcreteNamedType,
  TypedAstNode,
  TypedBooleanNode,
  TypedIntegerNode,
  TypedStringNode,
} from './typed-ast'
import { AstNode, StringNode } from '../../../parser/ast'
import { CustomOrderVisitor } from '../../../utils/ast-visitor'

export const inferTypes = (node: AstNode): TypedAstNode => {
  const inferenceVisitor = new CustomOrderVisitor<AstNode, TypedAstNode>({
    visitBoolean(node): TypedBooleanNode {
      return { ...node, $type: createConcreteNamedType('bool', []) }
    },
    visitInteger(node): TypedIntegerNode {
      return { ...node, $type: createConcreteNamedType('int', []) }
    },
    visitString(node: StringNode): TypedStringNode {
      return { ...node, $type: createConcreteNamedType('string', []) }
    },
  })

  // This is safe. TypeScript just can't prove it.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return (inferenceVisitor[`visit${node.__type}`] as any)(node as never, [])
}
