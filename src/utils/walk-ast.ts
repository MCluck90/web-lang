import {
  AnonymousTypeNode,
  ArgumentListNode,
  BinaryExpressionNode,
  BlockNode,
  FloatingPointNode,
  FunctionCallNode,
  HTMLNode,
  IdentifierNode,
  IntegerNode,
  isNodeType,
  JsAsmNode,
  MethodDefinitionNode,
  NamedTypeNode,
  ASTNode,
  NodeType,
  ObjectLiteralNode,
  ObjectPropertyNode,
  ObjectTypeNode,
  ParameterListNode,
  ParameterNode,
  ProgramNode,
  PropertyAccessNode,
  PropertyKeyNode,
  RemoteDefinitionNode,
  RemoteParameterNode,
  RemoteUrlNode,
  RenderNode,
  StringNode,
  TypeDefinitionNode,
  TypePropertyNode,
  UnaryExpressionNode,
  VariableAccessNode,
  VariableDeclarationNode,
} from '../parser/ast'

type VisitorLookup<TReturn = void> = {
  [K in NodeType]: (
    node: ASTNode & { __type: K },
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ) => TReturn
} & {
  everyNode?: (
    node: ASTNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ) => TReturn
}

const essentialVisitors: VisitorLookup = {
  AnonymousType: function (
    node: AnonymousTypeNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.type, [...path, node], visitors)
  },
  ArgumentList: function (
    node: ArgumentListNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.arguments.forEach((n) => depthFirstVisit(n, [...path, node], visitors))
  },
  Block: function (
    node: BlockNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.statements.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
  },
  BinaryExpression: function (
    node: BinaryExpressionNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    if (isNodeType('BinaryExpression')(node.left)) {
      depthFirstVisit(node.left, [...path, node], visitors)
      depthFirstVisit(node.right, [...path, node], visitors)
    } else {
      depthFirstVisit(node.right, [...path, node], visitors)
      depthFirstVisit(node.left, [...path, node], visitors)
    }
  },
  FloatingPoint: function (
    node: FloatingPointNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {},
  FunctionCall: function (
    node: FunctionCallNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.callee, [...path, node], visitors)
    depthFirstVisit(node.argumentList, [...path, node], visitors)
  },
  HTML: function (
    node: HTMLNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.children.forEach((n) => depthFirstVisit(n, [...path, node], visitors))
  },
  Integer: function (
    node: IntegerNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {},
  JsAsm: function (
    node: JsAsmNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {},
  ObjectLiteral: function (
    node: ObjectLiteralNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.properties.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
  },
  PropertyAccess: function (
    node: PropertyAccessNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.left, [...path, node], visitors)
    node.rights.forEach((n) => depthFirstVisit(n, [...path, node], visitors))
  },
  String: function (
    node: StringNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {},
  UnaryExpression: function (
    node: UnaryExpressionNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.expression, [...path, node], visitors)
  },
  VariableAccess: function (
    node: VariableAccessNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
  },
  Identifier: function (
    node: IdentifierNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {},
  MethodDefinition: function (
    node: MethodDefinitionNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
    depthFirstVisit(node.parameterList, [...path, node], visitors)
    if (node.returnType) {
      depthFirstVisit(node.returnType, [...path, node], visitors)
    }
    depthFirstVisit(node.body, [...path, node], visitors)
  },
  NamedType: function (
    node: NamedTypeNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.genericArguments.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
  },
  ObjectProperty: function (
    node: ObjectPropertyNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.key, [...path, node], visitors)
    depthFirstVisit(node.value, [...path, node], visitors)
  },
  ObjectType: function (
    node: ObjectTypeNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.properties.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
  },
  ParameterList: function (
    node: ParameterListNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.parameters.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
  },
  Parameter: function (
    node: ParameterNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
    if (node.type) {
      depthFirstVisit(node.type, [...path, node], visitors)
    }
  },
  Program: function (
    node: ProgramNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.statements.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
    if (node.render) {
      depthFirstVisit(node.render, [...path, node], visitors)
    }
  },
  PropertyKey: function (
    node: PropertyKeyNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.value, [...path, node], visitors)
  },
  Render: function (
    node: RenderNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.body, [...path, node], visitors)
  },
  RemoteDefinition: function (
    node: RemoteDefinitionNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
    depthFirstVisit(node.url, [...path, node], visitors)
    node.properties.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
    node.methods.forEach((n) => depthFirstVisit(n, [...path, node], visitors))
  },
  RemoteParameter: function (
    node: RemoteParameterNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
  },
  RemoteUrl: function (
    node: RemoteUrlNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    node.parameters.forEach((n) =>
      depthFirstVisit(n, [...path, node], visitors)
    )
  },
  TypeDefinition: function (
    node: TypeDefinitionNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
    depthFirstVisit(node.type, [...path, node], visitors)
  },
  TypeProperty: function (
    node: TypePropertyNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.name, [...path, node], visitors)
    depthFirstVisit(node.type, [...path, node], visitors)
  },
  VariableDeclaration: function (
    node: VariableDeclarationNode,
    path: ASTNode[],
    visitors: Partial<VisitorLookup>
  ): void {
    depthFirstVisit(node.identifier, [...path, node], visitors)
    if (node.type) {
      depthFirstVisit(node.type, [...path, node], visitors)
    }
    depthFirstVisit(node.initializer, [...path, node], visitors)
  },
}

function depthFirstVisit(
  node: ASTNode,
  path: ASTNode[],
  visitors: Partial<VisitorLookup>
) {
  ;(essentialVisitors[node.__type] as any)(node, path, visitors)
  if (node.__type in visitors) {
    ;(visitors[node.__type] as any)(node, path, visitors)
  }
  if (visitors.everyNode) {
    visitors.everyNode(node, path, visitors)
  }

  // The `any` here is needed because the type is too complex for TS to represent
}

export const walkAstDepthFirst = (
  program: ProgramNode,
  visitors: Partial<VisitorLookup>
): void => {
  depthFirstVisit(program, [], visitors)
}
