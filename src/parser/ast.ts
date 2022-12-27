export interface TypeDefinitionNode {
  __type: 'TypeDefinition'
  name: IdentifierNode
  type: TypeNode
}
export const createTypeDefinitionNode = (
  name: IdentifierNode,
  type: TypeNode
): TypeDefinitionNode => ({
  __type: 'TypeDefinition',
  name,
  type,
})

export interface TypePropertyNode {
  __type: 'TypeProperty'
  name: PropertyKeyNode
  type: TypeNode
}
export const createTypePropertyNode = (
  name: PropertyKeyNode,
  type: TypeNode
): TypePropertyNode => ({ __type: 'TypeProperty', name, type })

export interface PropertyKeyNode {
  __type: 'PropertyKey'
  value: IdentifierNode
}
export const createPropertyKeyNode = (
  value: IdentifierNode
): PropertyKeyNode => ({
  __type: 'PropertyKey',
  value,
})

export interface NamedTypeNode {
  __type: 'NamedType'
  name: string
  genericArguments: TypeNode[]
  isArray: boolean
}
export const createNamedTypeNode = (
  name: string,
  genericArguments: TypeNode[],
  isArray: boolean
): NamedTypeNode => ({
  __type: 'NamedType',
  name: name,
  genericArguments,
  isArray,
})

export interface ObjectTypeNode {
  __type: 'ObjectType'
  properties: TypePropertyNode[]
}
export const createObjectTypeNode = (
  properties: TypePropertyNode[]
): ObjectTypeNode => ({ __type: 'ObjectType', properties })

export interface AnonymousTypeNode {
  __type: 'AnonymousType'
  type: ObjectTypeNode
  isArray: boolean
}
export const createAnonymousTypeNode = (
  type: ObjectTypeNode,
  isArray: boolean
): AnonymousTypeNode => ({ __type: 'AnonymousType', type, isArray })

export type TypeNode = NamedTypeNode | AnonymousTypeNode | ObjectTypeNode

export interface IdentifierNode {
  __type: 'Identifier'
  value: string
}
export const createIdentifierNode = (value: string): IdentifierNode => ({
  __type: 'Identifier',
  value,
})

export interface MethodDefinitionNode {
  __type: 'MethodDefinition'
  name: IdentifierNode
  parameterList: ParameterListNode
  returnType: TypeNode | null
  body: MethodBodyNode
}
export const createMethodDefinitionNode = (
  name: IdentifierNode,
  parameterList: ParameterListNode,
  returnType: TypeNode | null,
  body: MethodBodyNode
): MethodDefinitionNode => ({
  __type: 'MethodDefinition',
  name,
  parameterList,
  returnType,
  body,
})

export interface ParameterListNode {
  __type: 'ParameterList'
  parameters: ParameterNode[]
}
export const createParameterListNode = (
  parameters: ParameterNode[]
): ParameterListNode => ({ __type: 'ParameterList', parameters })

export interface ParameterNode {
  __type: 'Parameter'
  name: IdentifierNode
  type: TypeNode | null
}
export const createParameterNode = (
  name: IdentifierNode,
  type: TypeNode | null
): ParameterNode => ({
  __type: 'Parameter',
  name,
  type,
})

export type MethodBodyNode = BlockNode

export type Statement = ExpressionNode | VariableDeclarationNode | WhileNode

export interface UseSelectorNode {
  __type: 'UseSelector'
  name: IdentifierNode | '*'
  alias: IdentifierNode | null
}
export const createUseSelectorNode = (
  name: IdentifierNode | '*',
  alias: IdentifierNode | null
): UseSelectorNode => ({
  __type: 'UseSelector',
  name,
  alias,
})

export interface UsePackageNode {
  __type: 'Use'
  selectors: UseSelectorNode[]
  type: 'Package'
  scope: string
  package: string
  path: string
}

export interface UseAbsoluteNode {
  __type: 'Use'
  selectors: UseSelectorNode[]
  type: 'Absolute'
  path: string
}

export interface UseRelativeNode {
  __type: 'Use'
  selectors: UseSelectorNode[]
  type: 'Relative'
  path: string
}

export type UseNode = UsePackageNode | UseAbsoluteNode | UseRelativeNode
export const createUsePackageNode = (
  scope: string,
  package_: string,
  path: string,
  selectors: UseSelectorNode[]
): UseNode & { type: 'Package' } => ({
  __type: 'Use',
  type: 'Package',
  scope,
  package: package_,
  path,
  selectors,
})
export const createUseAbsoluteNode = (
  path: string,
  selectors: UseSelectorNode[]
): UseNode & { type: 'Absolute' } => ({
  __type: 'Use',
  type: 'Absolute',
  path,
  selectors,
})
export const createUseRelativeNode = (
  path: string,
  selectors: UseSelectorNode[]
): UseNode & { type: 'Relative' } => ({
  __type: 'Use',
  type: 'Relative',
  path,
  selectors,
})

export interface BlockNode {
  __type: 'Block'
  statements: Statement[]
}
export const createBlockNode = (statements: Statement[]): BlockNode => ({
  __type: 'Block',
  statements,
})

export interface BooleanNode {
  __type: 'Boolean'
  value: boolean
}
export const createBooleanNode = (value: boolean): BooleanNode => ({
  __type: 'Boolean',
  value,
})

export interface IntegerNode {
  __type: 'Integer'
  value: number
}
export const createIntegerNode = (value: number): IntegerNode => ({
  __type: 'Integer',
  value,
})

export interface FloatingPointNode {
  __type: 'FloatingPoint'
  value: number
}
export const createFloatingPointNode = (value: number): FloatingPointNode => ({
  __type: 'FloatingPoint',
  value,
})

export interface StringNode {
  __type: 'String'
  value: string
}
export const createStringNode = (value: string): StringNode => ({
  __type: 'String',
  value,
})

export interface HTMLNode {
  __type: 'HTML'
  tag: string
  children: ExpressionNode[]
}
export const createHTMLNode = (
  tag: string,
  children: ExpressionNode[]
): HTMLNode => ({ __type: 'HTML', tag, children })

export interface VariableAccessNode {
  __type: 'VariableAccess'
  name: IdentifierNode
}
export const createVariableAccessNode = (
  name: IdentifierNode
): VariableAccessNode => ({ __type: 'VariableAccess', name })

export interface PropertyAccessNode {
  __type: 'PropertyAccess'
  left: ExpressionNode
  rights: IdentifierNode[]
}
export const createPropertyAccessNode = (
  left: ExpressionNode,
  rights: IdentifierNode[]
): PropertyAccessNode => ({
  __type: 'PropertyAccess',
  left,
  rights,
})

export interface AssignmentNode {
  __type: 'Assignment'
  left: IdentifierNode
  right: ExpressionNode
}
export const createAssignmentNode = (
  left: IdentifierNode,
  right: ExpressionNode
): AssignmentNode => ({
  __type: 'Assignment',
  left,
  right,
})

export type UnaryOperator = '-' | '!'

export interface UnaryExpressionNode {
  __type: 'UnaryExpression'
  operator: UnaryOperator
  expression: ExpressionNode
}
export const createUnaryExpressionNode = (
  operator: UnaryOperator,
  expression: ExpressionNode
): UnaryExpressionNode => ({
  __type: 'UnaryExpression',
  operator,
  expression,
})

export type BinaryOperator =
  | '+'
  | '-'
  | '*'
  | '/'
  | '=='
  | '!='
  | '<'
  | '>'
  | '<='
  | '>='

export interface BinaryExpressionNode {
  __type: 'BinaryExpression'
  left: ExpressionNode
  operator: BinaryOperator
  right: ExpressionNode
}
export const createBinaryExpressionNode = (
  left: ExpressionNode,
  operator: BinaryOperator,
  right: ExpressionNode
): BinaryExpressionNode => ({
  __type: 'BinaryExpression',
  left,
  operator,
  right,
})

export interface FunctionCallNode {
  __type: 'FunctionCall'
  callee: ExpressionNode
  argumentList: ArgumentListNode
}
export const createFunctionCallNode = (
  callee: ExpressionNode,
  argumentList: ArgumentListNode
): FunctionCallNode => ({ __type: 'FunctionCall', callee, argumentList })

export interface ArgumentListNode {
  __type: 'ArgumentList'
  arguments: ExpressionNode[]
}
export const createArgumentListNode = (
  args: ExpressionNode[]
): ArgumentListNode => ({ __type: 'ArgumentList', arguments: args })

export interface ObjectLiteralNode {
  __type: 'ObjectLiteral'
  properties: ObjectPropertyNode[]
}
export const createObjectLiteralNode = (
  properties: ObjectPropertyNode[]
): ObjectLiteralNode => ({ __type: 'ObjectLiteral', properties })

export interface ObjectPropertyNode {
  __type: 'ObjectProperty'
  key: IdentifierNode
  value: ExpressionNode
}
export const createObjectPropertyNode = (
  key: IdentifierNode,
  value: ExpressionNode
): ObjectPropertyNode => ({ __type: 'ObjectProperty', key, value })

export interface JsAsmNode {
  __type: 'JsAsm'
  code: string
}
export const createJsAsmNode = (code: string): JsAsmNode => ({
  __type: 'JsAsm',
  code,
})

export interface FunctionExpressionNode {
  __type: 'FunctionExpression'
  parameterList: ParameterListNode
  body: BlockNode
}
export const createFunctionExpressionNode = (
  parameterList: ParameterListNode,
  body: BlockNode
): FunctionExpressionNode => ({
  __type: 'FunctionExpression',
  parameterList,
  body,
})

export interface IfNode {
  __type: 'If'
  condition: ExpressionNode
  body: BlockNode
  else_: ElseNode | null
}
export const createIfNode = (
  condition: ExpressionNode,
  body: BlockNode,
  else_: ElseNode | null
): IfNode => ({ __type: 'If', condition, body, else_ })

export interface ElseNode {
  __type: 'Else'
  body: BlockNode
}
export const createElseNode = (body: BlockNode): ElseNode => ({
  __type: 'Else',
  body,
})

export type ExpressionNode =
  | AssignmentNode
  | BinaryExpressionNode
  | BooleanNode
  | FloatingPointNode
  | FunctionCallNode
  | FunctionExpressionNode
  | HTMLNode
  | IfNode
  | IntegerNode
  | JsAsmNode
  | ObjectLiteralNode
  | PropertyAccessNode
  | StringNode
  | UnaryExpressionNode
  | VariableAccessNode

export const isAnExpressionNode = (value: AstNode): value is ExpressionNode => {
  const expressionTypes: Record<ExpressionNode['__type'], boolean> = {
    Assignment: true,
    BinaryExpression: true,
    Boolean: true,
    FloatingPoint: true,
    FunctionCall: true,
    FunctionExpression: true,
    HTML: true,
    If: true,
    Integer: true,
    JsAsm: true,
    ObjectLiteral: true,
    PropertyAccess: true,
    String: true,
    UnaryExpression: true,
    VariableAccess: true,
  }
  return value.__type in expressionTypes
}

export interface WhileNode {
  __type: 'While'
  condition: ExpressionNode
  body: BlockNode
}
export const createWhileNode = (
  condition: ExpressionNode,
  body: BlockNode
): WhileNode => ({
  __type: 'While',
  condition,
  body,
})

export interface VariableAttributeListNode {
  __type: 'VariableAttributeList'
  attributes: IdentifierNode[]
}
export const createVariableAttributeListNode = (
  attributes: IdentifierNode[]
): VariableAttributeListNode => ({
  __type: 'VariableAttributeList',
  attributes,
})

export interface VariableDeclarationNode {
  __type: 'VariableDeclaration'
  attributeLists: VariableAttributeListNode[]
  identifier: IdentifierNode
  mutable: boolean
  type: TypeNode | null
  initializer: ExpressionNode
}
export const createVariableDeclarationNode = (
  attributeLists: VariableAttributeListNode[],
  identifier: IdentifierNode,
  mutable: boolean,
  type: TypeNode | null,
  initializer: ExpressionNode
): VariableDeclarationNode => ({
  __type: 'VariableDeclaration',
  identifier,
  attributeLists,
  mutable,
  type,
  initializer,
})

export interface RenderNode {
  __type: 'Render'
  body: BlockNode
}
export const createRenderNode = (body: BlockNode): RenderNode => ({
  __type: 'Render',
  body,
})

export interface ProgramNode {
  __type: 'Program'
  useStatements: UseNode[]
  statements: (TypeDefinitionNode | Statement)[]
  render: RenderNode | null
}
export const createProgramNode = (
  useStatements: UseNode[],
  statements: (TypeDefinitionNode | Statement)[],
  render: RenderNode | null
): ProgramNode => ({
  __type: 'Program',
  useStatements,
  statements,
  render: render,
})

export type AstNode =
  | AnonymousTypeNode
  | ArgumentListNode
  | BlockNode
  | ElseNode
  | ExpressionNode
  | IdentifierNode
  | MethodBodyNode
  | MethodDefinitionNode
  | NamedTypeNode
  | ObjectPropertyNode
  | ObjectTypeNode
  | ParameterListNode
  | ParameterNode
  | ProgramNode
  | PropertyKeyNode
  | RenderNode
  | TypeDefinitionNode
  | TypePropertyNode
  | UseNode
  | UseSelectorNode
  | VariableAccessNode
  | VariableAttributeListNode
  | VariableDeclarationNode
  | WhileNode

export type NodeType = AstNode['__type']

export const isNodeType =
  <T extends AstNode['__type']>(type: T) =>
  (value: unknown): value is AstNode & { __type: T } => {
    if (value === undefined || value === null) {
      return false
    }

    const node = value as AstNode
    return node.__type === type
  }

// Utility type to merge decorated AST nodes
export type MergeNodeTypes<
  TBaseAstNode extends AstNode,
  TDecoratedAstNode extends AstNode
> = {
  [TAstType in TBaseAstNode['__type']]: TAstType extends TDecoratedAstNode['__type']
    ? TDecoratedAstNode & { __type: TAstType }
    : TBaseAstNode & { __type: TAstType }
}[TBaseAstNode['__type']]
