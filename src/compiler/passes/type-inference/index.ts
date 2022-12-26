import {
  createConcreteNamedType,
  TypedAnonymousTypeNode,
  TypedArgumentListNode,
  TypedAstNode,
  TypedBinaryExpressionNode,
  TypedBlockNode,
  TypedBooleanNode,
  TypedElseNode,
  TypedFloatingPointNode,
  TypedFunctionCallNode,
  TypedFunctionExpressionNode,
  TypedHTMLNode,
  TypedIdentifierNode,
  TypedIfNode,
  TypedIntegerNode,
  TypedJsAsmNode,
  TypedMethodDefinitionNode,
  TypedNamedTypeNode,
  TypedObjectLiteralNode,
  TypedObjectPropertyNode,
  TypedObjectTypeNode,
  TypedParameterListNode,
  TypedParameterNode,
  TypedProgramNode,
  TypedPropertyAccessNode,
  TypedPropertyKeyNode,
  TypedRenderNode,
  TypedStringNode,
  TypedTypeDefinitionNode,
  TypedTypePropertyNode,
  TypedUnaryExpressionNode,
  TypedUseNode,
  TypedUseSelectorNode,
  TypedVariableAccessNode,
  TypedVariableAttributeListNode,
  TypedVariableDeclarationNode,
  TypedWhileNode,
} from './typed-ast'
import {
  ASTNode,
  AnonymousTypeNode,
  ArgumentListNode,
  BlockNode,
  IdentifierNode,
  MethodDefinitionNode,
  NamedTypeNode,
  ObjectPropertyNode,
  ObjectTypeNode,
  ParameterListNode,
  ParameterNode,
  ProgramNode,
  PropertyKeyNode,
  RenderNode,
  TypeDefinitionNode,
  TypePropertyNode,
  VariableDeclarationNode,
  AssignmentNode,
  BinaryExpressionNode,
  FloatingPointNode,
  FunctionCallNode,
  FunctionExpressionNode,
  HTMLNode,
  IntegerNode,
  JsAsmNode,
  ObjectLiteralNode,
  PropertyAccessNode,
  StringNode,
  UnaryExpressionNode,
  VariableAccessNode,
  BooleanNode,
  ElseNode,
  IfNode,
  UseNode,
  UseSelectorNode,
  WhileNode,
  VariableAttributeListNode,
} from '../../../parser/ast'

export const inferTypes = <T extends ASTNode>(node: T): T & TypedAstNode => {
  const inferenceVisitor = {
    visitNode<T extends ASTNode>(node: T): T & TypedAstNode {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      return (inferenceVisitor[`visit${node.__type}`] as any)(node as never)
    },
    visitAnonymousType(node: AnonymousTypeNode): TypedAnonymousTypeNode {
      throw new Error('Not yet implemented')
    },
    visitArgumentList(node: ArgumentListNode): TypedArgumentListNode {
      throw new Error('Not yet implemented')
    },
    visitAssignment(node: AssignmentNode[]) {
      throw new Error('Not yet implemented')
    },
    visitBinaryExpression(
      node: BinaryExpressionNode
    ): TypedBinaryExpressionNode {
      throw new Error('Not yet implemented')
    },
    visitBlock(node: BlockNode): TypedBlockNode {
      throw new Error('Not yet implemented')
    },
    visitBoolean(node: BooleanNode): TypedBooleanNode {
      return { ...node, $type: createConcreteNamedType('bool', []) }
    },
    visitElse(node: ElseNode): TypedElseNode {
      throw new Error('Not yet implemented')
    },
    visitFloatingPoint(node: FloatingPointNode): TypedFloatingPointNode {
      throw new Error('Not yet implemented')
    },
    visitFunctionCall(node: FunctionCallNode): TypedFunctionCallNode {
      throw new Error('Not yet implemented')
    },
    visitFunctionExpression(
      node: FunctionExpressionNode
    ): TypedFunctionExpressionNode {
      throw new Error('Not yet implemented')
    },
    visitHTML(node: HTMLNode): TypedHTMLNode {
      throw new Error('Not yet implemented')
    },
    visitIdentifier(node: IdentifierNode): TypedIdentifierNode {
      throw new Error('Not yet implemented')
    },
    visitIf(node: IfNode): TypedIfNode {
      throw new Error('Not yet implemented')
    },
    visitInteger(node: IntegerNode): TypedIntegerNode {
      return { ...node, $type: createConcreteNamedType('int', []) }
    },
    visitJsAsm(node: JsAsmNode): TypedJsAsmNode {
      throw new Error('Not yet implemented')
    },
    visitMethodDefinition(
      node: MethodDefinitionNode
    ): TypedMethodDefinitionNode {
      throw new Error('Not yet implemented')
    },
    visitNamedType(node: NamedTypeNode): TypedNamedTypeNode {
      throw new Error('Not yet implemented')
    },
    visitObjectLiteral(node: ObjectLiteralNode): TypedObjectLiteralNode {
      throw new Error('Not yet implemented')
    },
    visitObjectProperty(node: ObjectPropertyNode): TypedObjectPropertyNode {
      throw new Error('Not yet implemented')
    },
    visitObjectType(node: ObjectTypeNode): TypedObjectTypeNode {
      throw new Error('Not yet implemented')
    },
    visitParameter(node: ParameterNode): TypedParameterNode {
      throw new Error('Not yet implemented')
    },
    visitParameterList(node: ParameterListNode): TypedParameterListNode {
      throw new Error('Not yet implemented')
    },
    visitProgram(node: ProgramNode): TypedProgramNode {
      throw new Error('Not yet implemented')
    },
    visitPropertyAccess(node: PropertyAccessNode): TypedPropertyAccessNode {
      throw new Error('Not yet implemented')
    },
    visitPropertyKey(node: PropertyKeyNode): TypedPropertyKeyNode {
      throw new Error('Not yet implemented')
    },
    visitRender(node: RenderNode): TypedRenderNode {
      throw new Error('Not yet implemented')
    },
    visitString(node: StringNode): TypedStringNode {
      return { ...node, $type: createConcreteNamedType('string', []) }
    },
    visitTypeDefinition(node: TypeDefinitionNode): TypedTypeDefinitionNode {
      throw new Error('Not yet implemented')
    },
    visitTypeProperty(node: TypePropertyNode): TypedTypePropertyNode {
      throw new Error('Not yet implemented')
    },
    visitUnaryExpression(node: UnaryExpressionNode): TypedUnaryExpressionNode {
      throw new Error('Not yet implemented')
    },
    visitUse(node: UseNode): TypedUseNode {
      throw new Error('Not yet implemented')
    },
    visitUseSelector(node: UseSelectorNode): TypedUseSelectorNode {
      throw new Error('Not yet implemented')
    },
    visitVariableAccess(node: VariableAccessNode): TypedVariableAccessNode {
      throw new Error('Not yet implemented')
    },
    visitVariableAttributeList(
      node: VariableAttributeListNode
    ): TypedVariableAttributeListNode {
      throw new Error('Not yet implemented')
    },
    visitVariableDeclaration(
      node: VariableDeclarationNode
    ): TypedVariableDeclarationNode {
      throw new Error('Not yet implemented')
    },
    visitWhile(node: WhileNode): TypedWhileNode {
      throw new Error('Not yet implemented')
    },
  } satisfies Record<`visit${ASTNode['__type']}` | 'visitNode', unknown>
  return inferenceVisitor.visitNode(node)
}
