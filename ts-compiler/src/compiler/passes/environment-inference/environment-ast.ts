import {
  AssignmentNode,
  AstNode,
  BinaryExpressionNode,
  BooleanNode,
  ExpressionNode,
  FloatingPointNode,
  FunctionCallNode,
  FunctionExpressionNode,
  HTMLNode,
  IfNode,
  IntegerNode,
  JsAsmNode,
  MergeNodeTypes,
  ObjectLiteralNode,
  PropertyAccessNode,
  StringNode,
  UnaryExpressionNode,
  VariableAccessNode,
  VariableDeclarationNode,
} from '../../../parser/ast'

export type EnvironmentType = 'backend' | 'frontend' | 'isomorphic' | 'unknown'

export interface AssignmentNodeWithEnvironment extends AssignmentNode {
  $environment?: EnvironmentType
}
export interface BinaryExpressionNodeWithEnvironment
  extends BinaryExpressionNode {
  $environment?: EnvironmentType
}
export interface BooleanNodeWithEnvironment extends BooleanNode {
  $environment?: EnvironmentType
}
export interface FloatingPointNodeWithEnvironment extends FloatingPointNode {
  $environment?: EnvironmentType
}
export interface FunctionCallNodeWithEnvironment extends FunctionCallNode {
  $environment?: EnvironmentType
}
export interface FunctionExpressionNodeWithEnvironment
  extends FunctionExpressionNode {
  $environment?: EnvironmentType
}
export interface HTMLNodeWithEnvironment extends HTMLNode {
  $environment?: EnvironmentType
}
export interface IfNodeWithEnvironment extends IfNode {
  $environment?: EnvironmentType
}
export interface IntegerNodeWithEnvironment extends IntegerNode {
  $environment?: EnvironmentType
}
export interface JsAsmNodeWithEnvironment extends JsAsmNode {
  $environment?: EnvironmentType
}
export interface ObjectLiteralNodeWithEnvironment extends ObjectLiteralNode {
  $environment?: EnvironmentType
}
export interface PropertyAccessNodeWithEnvironment extends PropertyAccessNode {
  $environment?: EnvironmentType
}
export interface StringNodeWithEnvironment extends StringNode {
  $environment?: EnvironmentType
}
export interface UnaryExpressionNodeWithEnvironment
  extends UnaryExpressionNode {
  $environment?: EnvironmentType
}
export interface VariableAccessNodeWithEnvironment extends VariableAccessNode {
  $environment?: EnvironmentType
}
export interface VariableDeclarationNodeWithEnvironment
  extends VariableDeclarationNode {
  $environment?: EnvironmentType
  initializer: ExpressionNode & { $environment?: EnvironmentType }
}

export const isAnEnvironmentNode = <T extends AstNode>(
  node: T
): node is T & { $environment: EnvironmentType } =>
  '$environment' in node && typeof node['$environment'] === 'string'

export type AstNodeWithEnvironment = MergeNodeTypes<
  AstNode,
  | AssignmentNodeWithEnvironment
  | BinaryExpressionNodeWithEnvironment
  | BooleanNodeWithEnvironment
  | FloatingPointNodeWithEnvironment
  | FunctionCallNodeWithEnvironment
  | FunctionExpressionNodeWithEnvironment
  | HTMLNodeWithEnvironment
  | IfNodeWithEnvironment
  | IntegerNodeWithEnvironment
  | JsAsmNodeWithEnvironment
  | ObjectLiteralNodeWithEnvironment
  | PropertyAccessNodeWithEnvironment
  | StringNodeWithEnvironment
  | UnaryExpressionNodeWithEnvironment
  | VariableAccessNodeWithEnvironment
  | VariableDeclarationNodeWithEnvironment
>
