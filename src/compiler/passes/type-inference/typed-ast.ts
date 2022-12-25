import {
  AnonymousTypeNode,
  ArgumentListNode,
  BlockNode,
  ElseNode,
  IdentifierNode,
  MethodBodyNode,
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
  UseSelectorNode,
  VariableDeclarationNode,
  WhileNode,
  AssignmentNode,
  BinaryExpressionNode,
  BooleanNode,
  FloatingPointNode,
  FunctionCallNode,
  FunctionExpressionNode,
  HTMLNode,
  IfNode,
  IntegerNode,
  JsAsmNode,
  ObjectLiteralNode,
  PropertyAccessNode,
  StringNode,
  UnaryExpressionNode,
  VariableAccessNode,
  UsePackageNode,
  UseAbsoluteNode,
  UseRelativeNode,
} from '../../../parser/ast'

export interface ConcreteNamedType {
  __type: 'ConcreteType'
  name: string
  genericArguments: ConcreteNamedType[]
}
export const createConcreteNamedType = (
  name: string,
  genericArguments: ConcreteNamedType[]
): ConcreteNamedType => ({
  __type: 'ConcreteType',
  name,
  genericArguments,
})

export type InferredType = ConcreteNamedType | 'unknown'

export interface TypedAnonymousTypeNode extends AnonymousTypeNode {
  $type: InferredType
}
export interface TypedArgumentListNode extends ArgumentListNode {
  $type: InferredType
}
export interface TypedAssignmentNode extends AssignmentNode {
  $type: InferredType
}
export interface TypedBinaryExpressionNode extends BinaryExpressionNode {
  $type: InferredType
}
export interface TypedBlockNode extends BlockNode {
  $type: InferredType
}
export interface TypedBooleanNode extends BooleanNode {
  $type: InferredType
}
export interface TypedElseNode extends ElseNode {
  $type: InferredType
}
export interface TypedFloatingPointNode extends FloatingPointNode {
  $type: InferredType
}
export interface TypedFunctionCallNode extends FunctionCallNode {
  $type: InferredType
}
export interface TypedFunctionExpressionNode extends FunctionExpressionNode {
  $type: InferredType
}
export interface TypedHTMLNode extends HTMLNode {
  $type: InferredType
}
export interface TypedIdentifierNode extends IdentifierNode {
  $type: InferredType
}
export interface TypedIfNode extends IfNode {
  $type: InferredType
}
export interface TypedIntegerNode extends IntegerNode {
  $type: InferredType
}
export interface TypedJsAsmNode extends JsAsmNode {
  $type: InferredType
}
export interface TypedMethodBodyNode extends MethodBodyNode {
  $type: InferredType
}
export interface TypedMethodDefinitionNode extends MethodDefinitionNode {
  $type: InferredType
}
export interface TypedNamedTypeNode extends NamedTypeNode {
  $type: InferredType
}
export interface TypedObjectLiteralNode extends ObjectLiteralNode {
  $type: InferredType
}
export interface TypedObjectPropertyNode extends ObjectPropertyNode {
  $type: InferredType
}
export interface TypedObjectTypeNode extends ObjectTypeNode {
  $type: InferredType
}
export interface TypedParameterListNode extends ParameterListNode {
  $type: InferredType
}
export interface TypedParameterNode extends ParameterNode {
  $type: InferredType
}
export interface TypedProgramNode extends ProgramNode {
  $type: InferredType
}
export interface TypedPropertyAccessNode extends PropertyAccessNode {
  $type: InferredType
}
export interface TypedPropertyKeyNode extends PropertyKeyNode {
  $type: InferredType
}
export interface TypedRenderNode extends RenderNode {
  $type: InferredType
}
export interface TypedStringNode extends StringNode {
  $type: InferredType
}
export interface TypedTypeDefinitionNode extends TypeDefinitionNode {
  $type: InferredType
}
export interface TypedTypePropertyNode extends TypePropertyNode {
  $type: InferredType
}
export interface TypedUnaryExpressionNode extends UnaryExpressionNode {
  $type: InferredType
}
export interface TypedUseAbsoluteNode extends UseAbsoluteNode {
  $type: InferredType
}
export interface TypedUsePackageNode extends UsePackageNode {
  $type: InferredType
}
export interface TypedUseRelativeNode extends UseRelativeNode {
  $type: InferredType
}
export type TypedUseNode =
  | TypedUseAbsoluteNode
  | TypedUsePackageNode
  | TypedUseRelativeNode
export interface TypedUseSelectorNode extends UseSelectorNode {
  $type: InferredType
}
export interface TypedVariableAccessNode extends VariableAccessNode {
  $type: InferredType
}
export interface TypedVariableDeclarationNode extends VariableDeclarationNode {
  $type: InferredType
}
export interface TypedWhileNode extends WhileNode {
  $type: InferredType
}

export type TypedAstNode =
  | TypedAnonymousTypeNode
  | TypedArgumentListNode
  | TypedAssignmentNode
  | TypedBinaryExpressionNode
  | TypedBlockNode
  | TypedBooleanNode
  | TypedElseNode
  | TypedFloatingPointNode
  | TypedFunctionCallNode
  | TypedFunctionExpressionNode
  | TypedHTMLNode
  | TypedIdentifierNode
  | TypedIfNode
  | TypedIntegerNode
  | TypedJsAsmNode
  | TypedMethodBodyNode
  | TypedMethodDefinitionNode
  | TypedNamedTypeNode
  | TypedObjectLiteralNode
  | TypedObjectPropertyNode
  | TypedObjectTypeNode
  | TypedParameterListNode
  | TypedParameterNode
  | TypedProgramNode
  | TypedPropertyAccessNode
  | TypedPropertyKeyNode
  | TypedRenderNode
  | TypedStringNode
  | TypedTypeDefinitionNode
  | TypedTypePropertyNode
  | TypedUnaryExpressionNode
  | TypedUseAbsoluteNode
  | TypedUsePackageNode
  | TypedUseRelativeNode
  | TypedUseSelectorNode
  | TypedVariableAccessNode
  | TypedVariableDeclarationNode
  | TypedWhileNode
