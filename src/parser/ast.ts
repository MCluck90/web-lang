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

export interface RemoteDefinitionNode {
  __type: 'RemoteDefinition'
  name: IdentifierNode
  type: RemoteType
  url: RemoteUrlNode
  properties: TypePropertyNode[]
  methods: MethodDefinitionNode[]
}
export const createRemoteDefinitionNode = (
  name: IdentifierNode,
  type: RemoteType,
  url: RemoteUrlNode,
  properties: TypePropertyNode[],
  methods: MethodDefinitionNode[]
): RemoteDefinitionNode => ({
  __type: 'RemoteDefinition',
  name,
  type,
  url,
  properties,
  methods,
})

export type RemoteType = 'GET' | 'POST' | 'PUT' | 'PATCH' | 'DELETE'

export interface RemoteUrlNode {
  __type: 'RemoteUrl'
  path: string
  parameters: RemoteParameterNode[]
}
export const createRemoteUrlNode = (
  path: string,
  parameters: RemoteParameterNode[]
): RemoteUrlNode => ({ __type: 'RemoteUrl', path, parameters })

export interface RemoteParameterNode {
  __type: 'RemoteParameter'
  name: IdentifierNode
}
export const createRemoteParameterNode = (
  name: IdentifierNode
): RemoteParameterNode => ({
  __type: 'RemoteParameter',
  name,
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

// TODO: Add support for shorthand methods
// Ex: Add(x: int, y: int) = x + y
export type MethodBodyNode = BlockNode

export interface BlockNode {
  __type: 'Block'
  expressions: ExpressionNode[]
}
export const createBlockNode = (expressions: ExpressionNode[]): BlockNode => ({
  __type: 'Block',
  expressions,
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

export type UnaryOperator = '-'

export interface UnaryExpressionNode {
  __type: 'UnaryExpression'
  operator: UnaryOperator
  expression: ExpressionNode
}
export const createUnaryExpression = (
  operator: UnaryOperator,
  expression: ExpressionNode
): UnaryExpressionNode => ({
  __type: 'UnaryExpression',
  operator,
  expression,
})

export type BinaryOperator = '+' | '-' | '*' | '/'

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

export type ExpressionNode =
  | BinaryExpressionNode
  | FloatingPointNode
  | IntegerNode
  | PropertyAccessNode
  | UnaryExpressionNode
  | VariableAccessNode

export interface ProgramNode {
  __type: 'Program'
  typeDefinitions: TypeDefinitionNode[]
  remoteDefinitions: RemoteDefinitionNode[]
}
export const createProgramNode = (
  typeDefinitions: TypeDefinitionNode[],
  remoteDefinitions: RemoteDefinitionNode[]
): ProgramNode => ({ __type: 'Program', typeDefinitions, remoteDefinitions })

export type Node =
  | AnonymousTypeNode
  | BlockNode
  | ExpressionNode
  | IdentifierNode
  | MethodBodyNode
  | MethodDefinitionNode
  | NamedTypeNode
  | ObjectTypeNode
  | ParameterListNode
  | ParameterNode
  | ProgramNode
  | PropertyKeyNode
  | RemoteDefinitionNode
  | RemoteParameterNode
  | RemoteUrlNode
  | TypeDefinitionNode
  | TypePropertyNode

export const isNodeType =
  <T extends Node['__type']>(type: T) =>
  (value: unknown): value is Node & { __type: T } => {
    if (value === undefined || value === null) {
      return false
    }

    const node = value as Node
    return node.__type === type
  }
