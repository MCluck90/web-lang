import {
  AnonymousTypeNode,
  ASTNode,
  ArgumentListNode,
  BlockNode,
  BinaryExpressionNode,
  FloatingPointNode,
  FunctionCallNode,
  HTMLNode,
  IntegerNode,
  JsAsmNode,
  ObjectLiteralNode,
  PropertyAccessNode,
  StringNode,
  UnaryExpressionNode,
  VariableAccessNode,
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
  RemoteDefinitionNode,
  RemoteParameterNode,
  RemoteUrlNode,
  TypeDefinitionNode,
  TypePropertyNode,
  VariableDeclarationNode,
  ExpressionNode,
  Statement,
  TypeNode,
  isNodeType,
} from '../parser/ast'

export interface AstVisitor {
  visitNode(node: ASTNode, path: ASTNode[]): ASTNode | void
  visitProgram(node: ProgramNode): ProgramNode | void
  visitAnonymousType(
    node: AnonymousTypeNode,
    path: ASTNode[]
  ): AnonymousTypeNode | void
  visitArgumentList(
    node: ArgumentListNode,
    path: ASTNode[]
  ): ArgumentListNode | void
  visitBlock(node: BlockNode, path: ASTNode[]): BlockNode | void
  visitBinaryExpression(
    node: BinaryExpressionNode,
    path: ASTNode[]
  ): ExpressionNode | void
  visitFloatingPoint(
    node: FloatingPointNode,
    path: ASTNode[]
  ): FloatingPointNode | void
  visitFunctionCall(
    node: FunctionCallNode,
    path: ASTNode[]
  ): FunctionCallNode | void
  visitHTML(node: HTMLNode, path: ASTNode[]): HTMLNode | void
  visitInteger(node: IntegerNode, path: ASTNode[]): IntegerNode | void
  visitJsAsm(node: JsAsmNode, path: ASTNode[]): JsAsmNode | void
  visitObjectLiteral(
    node: ObjectLiteralNode,
    path: ASTNode[]
  ): ObjectLiteralNode | void
  visitPropertyAccess(
    node: PropertyAccessNode,
    path: ASTNode[]
  ): PropertyAccessNode | void
  visitString(node: StringNode, path: ASTNode[]): StringNode | void
  visitUnaryExpression(
    node: UnaryExpressionNode,
    path: ASTNode[]
  ): UnaryExpressionNode | void
  visitVariableAccess(
    node: VariableAccessNode,
    path: ASTNode[]
  ): VariableAccessNode | void
  visitIdentifier(node: IdentifierNode, path: ASTNode[]): IdentifierNode | void
  visitMethodDefinition(
    node: MethodDefinitionNode,
    path: ASTNode[]
  ): MethodDefinitionNode | void
  visitNamedType(node: NamedTypeNode, path: ASTNode[]): NamedTypeNode | void
  visitObjectProperty(
    node: ObjectPropertyNode,
    path: ASTNode[]
  ): ObjectPropertyNode | void
  visitObjectType(node: ObjectTypeNode, path: ASTNode[]): ObjectTypeNode | void
  visitParameterList(
    node: ParameterListNode,
    path: ASTNode[]
  ): ParameterListNode | void
  visitParameter(node: ParameterNode, path: ASTNode[]): ParameterNode | void
  visitPropertyKey(
    node: PropertyKeyNode,
    path: ASTNode[]
  ): PropertyKeyNode | void
  visitRender(node: RenderNode, path: ASTNode[]): RenderNode | void
  visitRemoteDefinition(
    node: RemoteDefinitionNode,
    path: ASTNode[]
  ): RemoteDefinitionNode | void
  visitRemoteParameter(
    node: RemoteParameterNode,
    path: ASTNode[]
  ): RemoteParameterNode | void
  visitRemoteUrl(node: RemoteUrlNode, path: ASTNode[]): RemoteUrlNode | void
  visitTypeDefinition(
    node: TypeDefinitionNode,
    path: ASTNode[]
  ): TypeDefinitionNode | void
  visitTypeProperty(
    node: TypePropertyNode,
    path: ASTNode[]
  ): TypePropertyNode | void
  visitVariableDeclaration(
    node: VariableDeclarationNode,
    path: ASTNode[]
  ): VariableDeclarationNode | void
}

const buildPath = (node: ASTNode, path: ASTNode[]) => [...path, node]

export class DepthFirstVisitor implements AstVisitor {
  constructor(private readonly visitors: Partial<AstVisitor>) {}

  visitNode<T extends ASTNode>(node: T, path: ASTNode[]): T | void {
    const specificResult = (this.visitors[`visit${node.__type}`] as any)?.(
      node as never,
      path
    )
    const genericResult = this.visitors.visitNode?.(node, path)
    return (specificResult ?? genericResult) as T | void
  }

  visitAnonymousType(
    node: AnonymousTypeNode,
    path: ASTNode[]
  ): AnonymousTypeNode | void {
    node.type =
      this.visitObjectType(node.type, buildPath(node, path)) ?? node.type
    return this.visitNode(node, path)
  }

  visitArgumentList(
    node: ArgumentListNode,
    path: ASTNode[]
  ): ArgumentListNode | void {
    let hasModifiedArguments = false
    const args: ExpressionNode[] = []
    for (const arg of node.arguments) {
      const result = this.visitNode(arg, buildPath(node, path))
      if (result) {
        args.push(result)
        hasModifiedArguments = true
      } else {
        args.push(arg)
      }
    }
    if (hasModifiedArguments) {
      node.arguments = args
    }
    return this.visitNode(node, path)
  }

  visitBlock(node: BlockNode, path: ASTNode[]): BlockNode | void {
    let hasModifiedStatements = false
    const statements: Statement[] = []

    for (const statement of statements) {
      const result = this.visitNode(statement, buildPath(node, path))
      if (result) {
        hasModifiedStatements = true
        statements.push(result)
      } else {
        statements.push(statement)
      }
    }

    if (hasModifiedStatements) {
      node.statements = statements
    }
    return this.visitNode(node, path)
  }

  visitBinaryExpression(
    node: BinaryExpressionNode,
    path: ASTNode[]
  ): ExpressionNode | void {
    node.left = this.visitNode(node.left, buildPath(node, path)) ?? node.left
    node.right = this.visitNode(node.right, buildPath(node, path)) ?? node.right
    return this.visitNode(node, path)
  }

  visitFloatingPoint(
    node: FloatingPointNode,
    path: ASTNode[]
  ): FloatingPointNode | void {
    return this.visitNode(node, path)
  }

  visitFunctionCall(
    node: FunctionCallNode,
    path: ASTNode[]
  ): FunctionCallNode | void {
    node.callee =
      this.visitNode(node.callee, buildPath(node, path)) ?? node.callee
    node.argumentList =
      this.visitArgumentList(node.argumentList, buildPath(node, path)) ??
      node.argumentList
    return this.visitNode(node, path)
  }

  visitHTML(node: HTMLNode, path: ASTNode[]): HTMLNode | void {
    let hasModifiedChildren = false
    const children: ExpressionNode[] = []
    for (const child of node.children) {
      const result = this.visitNode(child, buildPath(node, path))
      if (result) {
        hasModifiedChildren = true
        children.push(result)
      } else {
        children.push(child)
      }
    }

    if (hasModifiedChildren) {
      node.children = children
    }
    return this.visitNode(node, path)
  }

  visitInteger(node: IntegerNode, path: ASTNode[]): IntegerNode | void {
    return this.visitNode(node, path)
  }

  visitJsAsm(node: JsAsmNode, path: ASTNode[]): JsAsmNode | void {
    return this.visitNode(node, path)
  }

  visitObjectLiteral(
    node: ObjectLiteralNode,
    path: ASTNode[]
  ): ObjectLiteralNode | void {
    let hasModifiedProperties = false
    const properties: ObjectPropertyNode[] = []
    for (const property of node.properties) {
      const result = this.visitObjectProperty(property, buildPath(node, path))
      if (result) {
        hasModifiedProperties = true
        properties.push(result)
      } else {
        properties.push(property)
      }
    }
    if (hasModifiedProperties) {
      node.properties = properties
    }
    return this.visitNode(node, path)
  }

  visitPropertyAccess(
    node: PropertyAccessNode,
    path: ASTNode[]
  ): PropertyAccessNode | void {
    node.left = this.visitNode(node.left, buildPath(node, path)) ?? node.left

    let hasModifiedRights = false
    const rights: IdentifierNode[] = []
    for (const right of node.rights) {
      const result = this.visitIdentifier(right, buildPath(node, path))
      if (result) {
        hasModifiedRights = true
        rights.push(result)
      } else {
        rights.push(right)
      }
    }

    if (hasModifiedRights) {
      node.rights = rights
    }

    return this.visitNode(node, path)
  }

  visitString(node: StringNode, path: ASTNode[]): StringNode | void {
    return this.visitNode(node, path)
  }

  visitUnaryExpression(
    node: UnaryExpressionNode,
    path: ASTNode[]
  ): UnaryExpressionNode | void {
    node.expression =
      this.visitNode(node.expression, buildPath(node, path)) ?? node.expression
    return this.visitNode(node, path)
  }

  visitVariableAccess(
    node: VariableAccessNode,
    path: ASTNode[]
  ): VariableAccessNode | void {
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    return this.visitNode(node, path)
  }

  visitIdentifier(
    node: IdentifierNode,
    path: ASTNode[]
  ): IdentifierNode | void {
    return this.visitNode(node, path)
  }

  visitMethodDefinition(
    node: MethodDefinitionNode,
    path: ASTNode[]
  ): MethodDefinitionNode | void {
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.parameterList =
      this.visitParameterList(node.parameterList, buildPath(node, path)) ??
      node.parameterList
    node.returnType = node.returnType
      ? this.visitNode(node.returnType, buildPath(node, path)) ??
        node.returnType
      : null
    node.body = this.visitBlock(node.body, buildPath(node, path)) ?? node.body
    return this.visitNode(node, path)
  }

  visitNamedType(node: NamedTypeNode, path: ASTNode[]): NamedTypeNode | void {
    let hasModifiedGenerics = false
    const genericArguments: TypeNode[] = []
    for (const genericArgument of node.genericArguments) {
      const result = this.visitNode(genericArgument, buildPath(node, path))
      if (result) {
        hasModifiedGenerics = true
        genericArguments.push(result)
      } else {
        genericArguments.push(genericArgument)
      }
    }

    if (hasModifiedGenerics) {
      node.genericArguments = genericArguments
    }

    return this.visitNode(node, path)
  }

  visitObjectProperty(
    node: ObjectPropertyNode,
    path: ASTNode[]
  ): ObjectPropertyNode | void {
    node.key =
      this.visitors.visitIdentifier?.(node.key, buildPath(node, path)) ??
      node.key
    node.value = this.visitNode(node.value, buildPath(node, path)) ?? node.value
    return this.visitNode(node, path)
  }

  visitObjectType(
    node: ObjectTypeNode,
    path: ASTNode[]
  ): ObjectTypeNode | void {
    let hasModifiedProperties = false
    const properties: TypePropertyNode[] = []

    for (const property of node.properties) {
      const result = this.visitTypeProperty(property, buildPath(node, path))
      if (result) {
        hasModifiedProperties = true
        properties.push(result)
      } else {
        properties.push(property)
      }
    }

    if (hasModifiedProperties) {
      node.properties = properties
    }

    return this.visitNode(node, path)
  }

  visitParameterList(
    node: ParameterListNode,
    path: ASTNode[]
  ): ParameterListNode | void {
    let hasModifiedParameters = false
    const parameters: ParameterNode[] = []

    for (const parameter of node.parameters) {
      const result = this.visitParameter(parameter, buildPath(node, path))
      if (result) {
        hasModifiedParameters = true
        parameters.push(result)
      } else {
        parameters.push(parameter)
      }
    }

    if (hasModifiedParameters) {
      node.parameters = parameters
    }

    return this.visitNode(node, path)
  }

  visitParameter(node: ParameterNode, path: ASTNode[]): ParameterNode | void {
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.type = node.type
      ? this.visitNode(node.type, buildPath(node, path)) ?? node.type
      : node.type
    return this.visitNode(node, path)
  }

  visitProgram(node: ProgramNode): ProgramNode | void {
    let hasModifiedStatements = false
    const statements: (
      | RemoteDefinitionNode
      | TypeDefinitionNode
      | Statement
    )[] = []

    for (const statement of node.statements) {
      let result: RemoteDefinitionNode | TypeDefinitionNode | Statement | void =
        undefined
      if (isNodeType('RemoteDefinition')(statement)) {
        result = this.visitRemoteDefinition(statement, [node])
      } else if (isNodeType('TypeDefinition')(statement)) {
        result = this.visitTypeDefinition(statement, [node])
      } else {
        result = this.visitNode(statement, [node])
      }
      if (result) {
        hasModifiedStatements = true
        statements.push(result)
      } else {
        statements.push(statement)
      }
    }

    if (hasModifiedStatements) {
      node.statements = statements
    }

    return this.visitNode(node, [])
  }

  visitPropertyKey(
    node: PropertyKeyNode,
    path: ASTNode[]
  ): PropertyKeyNode | void {
    node.value =
      this.visitIdentifier(node.value, buildPath(node, path)) ?? node.value
    return this.visitNode(node, path)
  }

  visitRender(node: RenderNode, path: ASTNode[]): RenderNode | void {
    node.body = this.visitBlock(node.body, buildPath(node, path)) ?? node.body
    return this.visitNode(node, path)
  }

  visitRemoteDefinition(
    node: RemoteDefinitionNode,
    path: ASTNode[]
  ): RemoteDefinitionNode | void {
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.url = this.visitRemoteUrl(node.url, buildPath(node, path)) ?? node.url

    let hasModifiedProperties = false
    const properties: TypePropertyNode[] = []
    for (const property of node.properties) {
      const result = this.visitTypeProperty(property, buildPath(node, path))
      if (result) {
        hasModifiedProperties = true
        properties.push(result)
      } else {
        properties.push(property)
      }
    }

    if (hasModifiedProperties) {
      node.properties = properties
    }

    let hasModifiedMethods = false
    const methods: MethodDefinitionNode[] = []
    for (const method of node.methods) {
      const result = this.visitMethodDefinition(method, buildPath(node, path))
      if (result) {
        hasModifiedMethods = true
        methods.push(result)
      } else {
        methods.push(method)
      }
    }

    if (hasModifiedMethods) {
      node.methods = methods
    }

    return this.visitNode(node, path)
  }

  visitRemoteParameter(
    node: RemoteParameterNode,
    path: ASTNode[]
  ): RemoteParameterNode | void {
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    return this.visitNode(node, path)
  }

  visitRemoteUrl(node: RemoteUrlNode, path: ASTNode[]): RemoteUrlNode | void {
    let hasModifiedParameters = false
    const parameters: RemoteParameterNode[] = []
    for (const parameter of node.parameters) {
      const result = this.visitRemoteParameter(parameter, buildPath(node, path))
      if (result) {
        hasModifiedParameters = true
        parameters.push(result)
      } else {
        parameters.push(parameter)
      }
    }

    if (hasModifiedParameters) {
      node.parameters = parameters
    }

    return this.visitNode(node, path)
  }

  visitTypeDefinition(
    node: TypeDefinitionNode,
    path: ASTNode[]
  ): TypeDefinitionNode | void {
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.type = this.visitNode(node.type, buildPath(node, path)) ?? node.type
    return this.visitNode(node, path)
  }

  visitTypeProperty(
    node: TypePropertyNode,
    path: ASTNode[]
  ): TypePropertyNode | void {
    node.name =
      this.visitPropertyKey(node.name, buildPath(node, path)) ?? node.name
    node.type = this.visitNode(node.type, buildPath(node, path)) ?? node.type
    return this.visitNode(node, path)
  }

  visitVariableDeclaration(
    node: VariableDeclarationNode,
    path: ASTNode[]
  ): VariableDeclarationNode | void {
    node.identifier =
      this.visitIdentifier(node.identifier, buildPath(node, path)) ??
      node.identifier
    node.type = node.type
      ? this.visitNode(node.type, buildPath(node, path)) ?? node.type
      : node.type
    node.initializer =
      this.visitNode(node.initializer, buildPath(node, path)) ??
      node.initializer
    return this.visitNode(node, path)
  }
}

export class InOrderAstVisitor implements AstVisitor {
  constructor(private readonly visitors: Partial<AstVisitor>) {}

  visitNode<T extends ASTNode>(node: T, path: ASTNode[]): T | void {
    const specificResult = (this.visitors[`visit${node.__type}`] as any)?.(
      node as never,
      path
    )
    const genericResult = this.visitors.visitNode?.(node, path)
    return (specificResult ?? genericResult) as T | void
  }

  visitAnonymousType(
    node: AnonymousTypeNode,
    path: ASTNode[]
  ): AnonymousTypeNode | void {
    const result = this.visitNode(node, path)
    node.type =
      this.visitObjectType(node.type, buildPath(node, path)) ?? node.type
    return result
  }

  visitArgumentList(
    node: ArgumentListNode,
    path: ASTNode[]
  ): ArgumentListNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedArguments = false
    const args: ExpressionNode[] = []
    for (const arg of node.arguments) {
      const result = this.visitNode(arg, buildPath(node, path))
      if (result) {
        args.push(result)
        hasModifiedArguments = true
      } else {
        args.push(arg)
      }
    }
    if (hasModifiedArguments) {
      node.arguments = args
    }
    return result
  }

  visitBlock(node: BlockNode, path: ASTNode[]): BlockNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedStatements = false
    const statements: Statement[] = []

    for (const statement of statements) {
      const result = this.visitNode(statement, buildPath(node, path))
      if (result) {
        hasModifiedStatements = true
        statements.push(result)
      } else {
        statements.push(statement)
      }
    }

    if (hasModifiedStatements) {
      node.statements = statements
    }
    return result
  }

  visitBinaryExpression(
    node: BinaryExpressionNode,
    path: ASTNode[]
  ): ExpressionNode | void {
    const result = this.visitNode(node, path)
    node.left = this.visitNode(node.left, buildPath(node, path)) ?? node.left
    node.right = this.visitNode(node.right, buildPath(node, path)) ?? node.right
    return result
  }

  visitFloatingPoint(
    node: FloatingPointNode,
    path: ASTNode[]
  ): FloatingPointNode | void {
    return this.visitNode(node, path)
  }

  visitFunctionCall(
    node: FunctionCallNode,
    path: ASTNode[]
  ): FunctionCallNode | void {
    const result = this.visitNode(node, path)
    node.callee =
      this.visitNode(node.callee, buildPath(node, path)) ?? node.callee
    node.argumentList =
      this.visitArgumentList(node.argumentList, buildPath(node, path)) ??
      node.argumentList
    return result
  }

  visitHTML(node: HTMLNode, path: ASTNode[]): HTMLNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedChildren = false
    const children: ExpressionNode[] = []
    for (const child of node.children) {
      const result = this.visitNode(child, buildPath(node, path))
      if (result) {
        hasModifiedChildren = true
        children.push(result)
      } else {
        children.push(child)
      }
    }

    if (hasModifiedChildren) {
      node.children = children
    }
    return result
  }

  visitInteger(node: IntegerNode, path: ASTNode[]): IntegerNode | void {
    return this.visitNode(node, path)
  }

  visitJsAsm(node: JsAsmNode, path: ASTNode[]): JsAsmNode | void {
    return this.visitNode(node, path)
  }

  visitObjectLiteral(
    node: ObjectLiteralNode,
    path: ASTNode[]
  ): ObjectLiteralNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedProperties = false
    const properties: ObjectPropertyNode[] = []
    for (const property of node.properties) {
      const result = this.visitObjectProperty(property, buildPath(node, path))
      if (result) {
        hasModifiedProperties = true
        properties.push(result)
      } else {
        properties.push(property)
      }
    }
    if (hasModifiedProperties) {
      node.properties = properties
    }
    return result
  }

  visitPropertyAccess(
    node: PropertyAccessNode,
    path: ASTNode[]
  ): PropertyAccessNode | void {
    const result = this.visitNode(node, path)
    node.left = this.visitNode(node.left, buildPath(node, path)) ?? node.left

    let hasModifiedRights = false
    const rights: IdentifierNode[] = []
    for (const right of node.rights) {
      const result = this.visitIdentifier(right, buildPath(node, path))
      if (result) {
        hasModifiedRights = true
        rights.push(result)
      } else {
        rights.push(right)
      }
    }

    if (hasModifiedRights) {
      node.rights = rights
    }

    return result
  }

  visitString(node: StringNode, path: ASTNode[]): StringNode | void {
    return this.visitNode(node, path)
  }

  visitUnaryExpression(
    node: UnaryExpressionNode,
    path: ASTNode[]
  ): UnaryExpressionNode | void {
    const result = this.visitNode(node, path)
    node.expression =
      this.visitNode(node.expression, buildPath(node, path)) ?? node.expression
    return result
  }

  visitVariableAccess(
    node: VariableAccessNode,
    path: ASTNode[]
  ): VariableAccessNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    return result
  }

  visitIdentifier(
    node: IdentifierNode,
    path: ASTNode[]
  ): IdentifierNode | void {
    return this.visitNode(node, path)
  }

  visitMethodDefinition(
    node: MethodDefinitionNode,
    path: ASTNode[]
  ): MethodDefinitionNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.parameterList =
      this.visitParameterList(node.parameterList, buildPath(node, path)) ??
      node.parameterList
    node.returnType = node.returnType
      ? this.visitNode(node.returnType, buildPath(node, path)) ??
        node.returnType
      : null
    node.body = this.visitBlock(node.body, buildPath(node, path)) ?? node.body
    return result
  }

  visitNamedType(node: NamedTypeNode, path: ASTNode[]): NamedTypeNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedGenerics = false
    const genericArguments: TypeNode[] = []
    for (const genericArgument of node.genericArguments) {
      const result = this.visitNode(genericArgument, buildPath(node, path))
      if (result) {
        hasModifiedGenerics = true
        genericArguments.push(result)
      } else {
        genericArguments.push(genericArgument)
      }
    }

    if (hasModifiedGenerics) {
      node.genericArguments = genericArguments
    }

    return result
  }

  visitObjectProperty(
    node: ObjectPropertyNode,
    path: ASTNode[]
  ): ObjectPropertyNode | void {
    const result = this.visitNode(node, path)
    node.key =
      this.visitors.visitIdentifier?.(node.key, buildPath(node, path)) ??
      node.key
    node.value = this.visitNode(node.value, buildPath(node, path)) ?? node.value
    return result
  }

  visitObjectType(
    node: ObjectTypeNode,
    path: ASTNode[]
  ): ObjectTypeNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedProperties = false
    const properties: TypePropertyNode[] = []

    for (const property of node.properties) {
      const result = this.visitTypeProperty(property, buildPath(node, path))
      if (result) {
        hasModifiedProperties = true
        properties.push(result)
      } else {
        properties.push(property)
      }
    }

    if (hasModifiedProperties) {
      node.properties = properties
    }

    return result
  }

  visitParameterList(
    node: ParameterListNode,
    path: ASTNode[]
  ): ParameterListNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedParameters = false
    const parameters: ParameterNode[] = []

    for (const parameter of node.parameters) {
      const result = this.visitParameter(parameter, buildPath(node, path))
      if (result) {
        hasModifiedParameters = true
        parameters.push(result)
      } else {
        parameters.push(parameter)
      }
    }

    if (hasModifiedParameters) {
      node.parameters = parameters
    }

    return result
  }

  visitParameter(node: ParameterNode, path: ASTNode[]): ParameterNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.type = node.type
      ? this.visitNode(node.type, buildPath(node, path)) ?? node.type
      : node.type
    return result
  }

  visitProgram(node: ProgramNode): ProgramNode | void {
    const result = this.visitNode(node, [])
    let hasModifiedStatements = false
    const statements: (
      | RemoteDefinitionNode
      | TypeDefinitionNode
      | Statement
    )[] = []

    for (const statement of node.statements) {
      let result: RemoteDefinitionNode | TypeDefinitionNode | Statement | void =
        undefined
      if (isNodeType('RemoteDefinition')(statement)) {
        result = this.visitRemoteDefinition(statement, [node])
      } else if (isNodeType('TypeDefinition')(statement)) {
        result = this.visitTypeDefinition(statement, [node])
      } else {
        result = this.visitNode(statement, [node])
      }
      if (result) {
        hasModifiedStatements = true
        statements.push(result)
      } else {
        statements.push(statement)
      }
    }

    if (hasModifiedStatements) {
      node.statements = statements
    }

    return result
  }

  visitPropertyKey(
    node: PropertyKeyNode,
    path: ASTNode[]
  ): PropertyKeyNode | void {
    const result = this.visitNode(node, path)
    node.value =
      this.visitIdentifier(node.value, buildPath(node, path)) ?? node.value
    return result
  }

  visitRender(node: RenderNode, path: ASTNode[]): RenderNode | void {
    const result = this.visitNode(node, path)
    node.body = this.visitBlock(node.body, buildPath(node, path)) ?? node.body
    return result
  }

  visitRemoteDefinition(
    node: RemoteDefinitionNode,
    path: ASTNode[]
  ): RemoteDefinitionNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.url = this.visitRemoteUrl(node.url, buildPath(node, path)) ?? node.url

    let hasModifiedProperties = false
    const properties: TypePropertyNode[] = []
    for (const property of node.properties) {
      const result = this.visitTypeProperty(property, buildPath(node, path))
      if (result) {
        hasModifiedProperties = true
        properties.push(result)
      } else {
        properties.push(property)
      }
    }

    if (hasModifiedProperties) {
      node.properties = properties
    }

    let hasModifiedMethods = false
    const methods: MethodDefinitionNode[] = []
    for (const method of node.methods) {
      const result = this.visitMethodDefinition(method, buildPath(node, path))
      if (result) {
        hasModifiedMethods = true
        methods.push(result)
      } else {
        methods.push(method)
      }
    }

    if (hasModifiedMethods) {
      node.methods = methods
    }

    return result
  }

  visitRemoteParameter(
    node: RemoteParameterNode,
    path: ASTNode[]
  ): RemoteParameterNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    return result
  }

  visitRemoteUrl(node: RemoteUrlNode, path: ASTNode[]): RemoteUrlNode | void {
    const result = this.visitNode(node, path)
    let hasModifiedParameters = false
    const parameters: RemoteParameterNode[] = []
    for (const parameter of node.parameters) {
      const result = this.visitRemoteParameter(parameter, buildPath(node, path))
      if (result) {
        hasModifiedParameters = true
        parameters.push(result)
      } else {
        parameters.push(parameter)
      }
    }

    if (hasModifiedParameters) {
      node.parameters = parameters
    }

    return result
  }

  visitTypeDefinition(
    node: TypeDefinitionNode,
    path: ASTNode[]
  ): TypeDefinitionNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitIdentifier(node.name, buildPath(node, path)) ?? node.name
    node.type = this.visitNode(node.type, buildPath(node, path)) ?? node.type
    return result
  }

  visitTypeProperty(
    node: TypePropertyNode,
    path: ASTNode[]
  ): TypePropertyNode | void {
    const result = this.visitNode(node, path)
    node.name =
      this.visitPropertyKey(node.name, buildPath(node, path)) ?? node.name
    node.type = this.visitNode(node.type, buildPath(node, path)) ?? node.type
    return result
  }

  visitVariableDeclaration(
    node: VariableDeclarationNode,
    path: ASTNode[]
  ): VariableDeclarationNode | void {
    const result = this.visitNode(node, path)
    node.identifier =
      this.visitIdentifier(node.identifier, buildPath(node, path)) ??
      node.identifier
    node.type = node.type
      ? this.visitNode(node.type, buildPath(node, path)) ?? node.type
      : node.type
    node.initializer =
      this.visitNode(node.initializer, buildPath(node, path)) ??
      node.initializer
    return result
  }
}
