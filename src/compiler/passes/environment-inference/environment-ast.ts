import {
  AnonymousTypeNode,
  ArgumentListNode,
  AssignmentNode,
  BinaryExpressionNode,
  BlockNode,
  BooleanNode,
  ElseNode,
  FloatingPointNode,
  FunctionCallNode,
  FunctionExpressionNode,
  HTMLNode,
  IdentifierNode,
  IfNode,
  IntegerNode,
  JsAsmNode,
  MethodBodyNode,
  MethodDefinitionNode,
  NamedTypeNode,
  ObjectLiteralNode,
  ObjectPropertyNode,
  ObjectTypeNode,
  ParameterListNode,
  ParameterNode,
  ProgramNode,
  PropertyAccessNode,
  PropertyKeyNode,
  RenderNode,
  StringNode,
  TypeDefinitionNode,
  TypePropertyNode,
  UnaryExpressionNode,
  UseAbsoluteNode,
  UsePackageNode,
  UseRelativeNode,
  UseSelectorNode,
  VariableAccessNode,
  VariableAttributeListNode,
  VariableDeclarationNode,
  WhileNode,
} from '../../../parser/ast'

export type EnvironmentType = 'backend' | 'frontend' | 'isomorphic' | 'unknown'

export interface AssignmentNodeWithEnvironment extends AssignmentNode {
  $environment: EnvironmentType
}
export interface BinaryExpressionNodeWithEnvironment
  extends BinaryExpressionNode {
  $environment: EnvironmentType
}
export interface BooleanNodeWithEnvironment extends BooleanNode {
  $environment: EnvironmentType
}
export interface FloatingPointNodeWithEnvironment extends FloatingPointNode {
  $environment: EnvironmentType
}
export interface FunctionCallNodeWithEnvironment extends FunctionCallNode {
  $environment: EnvironmentType
}
export interface FunctionExpressionNodeWithEnvironment
  extends FunctionExpressionNode {
  $environment: EnvironmentType
}
export interface HTMLNodeWithEnvironment extends HTMLNode {
  $environment: EnvironmentType
}
export interface IfNodeWithEnvironment extends IfNode {
  $environment: EnvironmentType
}
export interface IntegerNodeWithEnvironment extends IntegerNode {
  $environment: EnvironmentType
}
export interface JsAsmNodeWithEnvironment extends JsAsmNode {
  $environment: EnvironmentType
}
export interface ObjectLiteralNodeWithEnvironment extends ObjectLiteralNode {
  $environment: EnvironmentType
}
export interface PropertyAccessNodeWithEnvironment extends PropertyAccessNode {
  $environment: EnvironmentType
}
export interface StringNodeWithEnvironment extends StringNode {
  $environment: EnvironmentType
}
export interface UnaryExpressionNodeWithEnvironment
  extends UnaryExpressionNode {
  $environment: EnvironmentType
}
export interface VariableAccessNodeWithEnvironment extends VariableAccessNode {
  $environment: EnvironmentType
}
export interface VariableDeclarationNodeWithEnvironment
  extends VariableDeclarationNode {
  $environment: EnvironmentType
}

export type AstNodeWithEnvironment =
  | AnonymousTypeNode
  | ArgumentListNode
  | AssignmentNodeWithEnvironment
  | BinaryExpressionNodeWithEnvironment
  | BlockNode
  | BooleanNodeWithEnvironment
  | ElseNode
  | FloatingPointNodeWithEnvironment
  | FunctionCallNodeWithEnvironment
  | FunctionExpressionNodeWithEnvironment
  | HTMLNodeWithEnvironment
  | IdentifierNode
  | IfNodeWithEnvironment
  | IntegerNodeWithEnvironment
  | JsAsmNodeWithEnvironment
  | MethodBodyNode
  | MethodDefinitionNode
  | NamedTypeNode
  | ObjectLiteralNodeWithEnvironment
  | ObjectPropertyNode
  | ObjectTypeNode
  | ParameterListNode
  | ParameterNode
  | ProgramNode
  | PropertyAccessNodeWithEnvironment
  | PropertyKeyNode
  | RenderNode
  | StringNodeWithEnvironment
  | TypeDefinitionNode
  | TypePropertyNode
  | UnaryExpressionNodeWithEnvironment
  | UseAbsoluteNode
  | UsePackageNode
  | UseRelativeNode
  | UseSelectorNode
  | VariableAccessNodeWithEnvironment
  | VariableAttributeListNode
  | VariableDeclarationNodeWithEnvironment
  | WhileNode
