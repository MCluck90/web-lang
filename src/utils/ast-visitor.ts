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
  TypeDefinitionNode,
  TypePropertyNode,
  VariableDeclarationNode,
  ExpressionNode,
  Statement,
  TypeNode,
  FunctionExpressionNode,
  IfNode,
  ElseNode,
  AssignmentNode,
  UseNode,
  UseSelectorNode,
  isNodeType,
  BooleanNode,
} from '../parser/ast'

export interface AstMapper<T> {
  visitNode(node: ASTNode, path: ASTNode[]): T
  visitProgram(node: ProgramNode): T
  visitAssignment(node: AssignmentNode, path: ASTNode[]): T
  visitAnonymousType(node: AnonymousTypeNode, path: ASTNode[]): T
  visitArgumentList(node: ArgumentListNode, path: ASTNode[]): T
  visitBlock(node: BlockNode, path: ASTNode[]): T
  visitBinaryExpression(node: BinaryExpressionNode, path: ASTNode[]): T
  visitBoolean(node: BooleanNode, path: ASTNode[]): T
  visitElse(node: ElseNode, path: ASTNode[]): T
  visitFloatingPoint(node: FloatingPointNode, path: ASTNode[]): T
  visitFunctionExpression(node: FunctionExpressionNode, path: ASTNode[]): T
  visitFunctionCall(node: FunctionCallNode, path: ASTNode[]): T
  visitHTML(node: HTMLNode, path: ASTNode[]): T
  visitInteger(node: IntegerNode, path: ASTNode[]): T
  visitIf(node: IfNode, path: ASTNode[]): T
  visitJsAsm(node: JsAsmNode, path: ASTNode[]): T
  visitObjectLiteral(node: ObjectLiteralNode, path: ASTNode[]): T
  visitPropertyAccess(node: PropertyAccessNode, path: ASTNode[]): T
  visitString(node: StringNode, path: ASTNode[]): T
  visitUnaryExpression(node: UnaryExpressionNode, path: ASTNode[]): T
  visitVariableAccess(node: VariableAccessNode, path: ASTNode[]): T
  visitIdentifier(node: IdentifierNode, path: ASTNode[]): T
  visitMethodDefinition(node: MethodDefinitionNode, path: ASTNode[]): T
  visitNamedType(node: NamedTypeNode, path: ASTNode[]): T
  visitObjectProperty(node: ObjectPropertyNode, path: ASTNode[]): T
  visitObjectType(node: ObjectTypeNode, path: ASTNode[]): T
  visitParameterList(node: ParameterListNode, path: ASTNode[]): T
  visitParameter(node: ParameterNode, path: ASTNode[]): T
  visitPropertyKey(node: PropertyKeyNode, path: ASTNode[]): T
  visitRender(node: RenderNode, path: ASTNode[]): T
  visitTypeDefinition(node: TypeDefinitionNode, path: ASTNode[]): T
  visitTypeProperty(node: TypePropertyNode, path: ASTNode[]): T
  visitUse(node: UseNode, path: ASTNode[]): T
  visitUseSelector(node: UseSelectorNode, path: ASTNode[]): T
  visitVariableDeclaration(node: VariableDeclarationNode, path: ASTNode[]): T
}

export interface AstVisitor<T = void> {
  visitNode(node: ASTNode, path: ASTNode[]): ASTNode | T
  visitProgram(node: ProgramNode): ProgramNode | T
  visitAssignment(node: AssignmentNode, path: ASTNode[]): AssignmentNode | T
  visitAnonymousType(
    node: AnonymousTypeNode,
    path: ASTNode[]
  ): AnonymousTypeNode | T
  visitArgumentList(
    node: ArgumentListNode,
    path: ASTNode[]
  ): ArgumentListNode | T
  visitBlock(node: BlockNode, path: ASTNode[]): BlockNode | T
  visitBinaryExpression(
    node: BinaryExpressionNode,
    path: ASTNode[]
  ): ExpressionNode | T
  visitBoolean(node: BooleanNode, path: ASTNode[]): BooleanNode | T
  visitElse(node: ElseNode, path: ASTNode[]): ElseNode | T
  visitFloatingPoint(
    node: FloatingPointNode,
    path: ASTNode[]
  ): FloatingPointNode | T
  visitFunctionCall(
    node: FunctionCallNode,
    path: ASTNode[]
  ): FunctionCallNode | T
  visitFunctionExpression(
    node: FunctionExpressionNode,
    path: ASTNode[]
  ): FunctionExpressionNode | T
  visitHTML(node: HTMLNode, path: ASTNode[]): HTMLNode | T
  visitInteger(node: IntegerNode, path: ASTNode[]): IntegerNode | T
  visitIf(node: IfNode, path: ASTNode[]): IfNode | T
  visitJsAsm(node: JsAsmNode, path: ASTNode[]): JsAsmNode | T
  visitObjectLiteral(
    node: ObjectLiteralNode,
    path: ASTNode[]
  ): ObjectLiteralNode | T
  visitPropertyAccess(
    node: PropertyAccessNode,
    path: ASTNode[]
  ): PropertyAccessNode | T
  visitString(node: StringNode, path: ASTNode[]): StringNode | T
  visitUnaryExpression(
    node: UnaryExpressionNode,
    path: ASTNode[]
  ): UnaryExpressionNode | T
  visitVariableAccess(
    node: VariableAccessNode,
    path: ASTNode[]
  ): VariableAccessNode | T
  visitIdentifier(node: IdentifierNode, path: ASTNode[]): IdentifierNode | T
  visitMethodDefinition(
    node: MethodDefinitionNode,
    path: ASTNode[]
  ): MethodDefinitionNode | T
  visitNamedType(node: NamedTypeNode, path: ASTNode[]): NamedTypeNode | T
  visitObjectProperty(
    node: ObjectPropertyNode,
    path: ASTNode[]
  ): ObjectPropertyNode | T
  visitObjectType(node: ObjectTypeNode, path: ASTNode[]): ObjectTypeNode | T
  visitParameterList(
    node: ParameterListNode,
    path: ASTNode[]
  ): ParameterListNode | T
  visitParameter(node: ParameterNode, path: ASTNode[]): ParameterNode | T
  visitPropertyKey(node: PropertyKeyNode, path: ASTNode[]): PropertyKeyNode | T
  visitRender(node: RenderNode, path: ASTNode[]): RenderNode | T
  visitTypeDefinition(
    node: TypeDefinitionNode,
    path: ASTNode[]
  ): TypeDefinitionNode | T
  visitTypeProperty(
    node: TypePropertyNode,
    path: ASTNode[]
  ): TypePropertyNode | T
  visitUse(node: UseNode, path: ASTNode[]): UseNode | T
  visitUseSelector(node: UseSelectorNode, path: ASTNode[]): UseSelectorNode | T
  visitVariableDeclaration(
    node: VariableDeclarationNode,
    path: ASTNode[]
  ): VariableDeclarationNode | T
}

const buildPath = (node: ASTNode, path: ASTNode[]) => [...path, node]

export class DepthFirstVisitor implements AstVisitor {
  constructor(private readonly visitors: Partial<AstVisitor>) {}

  visitNode<T extends ASTNode>(node: T, path: ASTNode[]): T | void {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const specificResult = (this.visitors[`visit${node.__type}`] as any)?.(
      node as never,
      path
    )
    const genericResult = this.visitors.visitNode?.(node, path)
    return (specificResult ?? genericResult) as T | void
  }

  private descendIntoNode<T extends ASTNode>(
    node: T,
    path: ASTNode[]
  ): T | void {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (this[`visit${node.__type}`] as any)(node, path)
  }

  visitAssignment(
    node: AssignmentNode,
    path: ASTNode[]
  ): AssignmentNode | void {
    node.left =
      this.descendIntoNode(node.left, buildPath(node, path)) ?? node.left
    node.right =
      this.descendIntoNode(node.right, buildPath(node, path)) ?? node.right
    return this.visitNode(node, path)
  }

  visitAnonymousType(
    node: AnonymousTypeNode,
    path: ASTNode[]
  ): AnonymousTypeNode | void {
    node.type =
      this.descendIntoNode(node.type, buildPath(node, path)) ?? node.type
    return this.visitNode(node, path)
  }

  visitArgumentList(
    node: ArgumentListNode,
    path: ASTNode[]
  ): ArgumentListNode | void {
    let hasModifiedArguments = false
    const args: ExpressionNode[] = []
    for (const arg of node.arguments) {
      const result = this.descendIntoNode(arg, buildPath(node, path))
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
      const result = this.descendIntoNode(statement, buildPath(node, path))
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
    node.left =
      this.descendIntoNode(node.left, buildPath(node, path)) ?? node.left
    node.right =
      this.descendIntoNode(node.right, buildPath(node, path)) ?? node.right
    return this.visitNode(node, path)
  }

  visitBoolean(node: BooleanNode, path: ASTNode[]): BooleanNode | void {
    return this.visitNode(node, path)
  }

  visitElse(node: ElseNode, path: ASTNode[]): ElseNode | void {
    node.body =
      this.descendIntoNode(node.body, buildPath(node, path)) ?? node.body
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
      this.descendIntoNode(node.callee, buildPath(node, path)) ?? node.callee
    node.argumentList =
      this.descendIntoNode(node.argumentList, buildPath(node, path)) ??
      node.argumentList
    return this.visitNode(node, path)
  }

  visitFunctionExpression(
    node: FunctionExpressionNode,
    path: ASTNode[]
  ): FunctionExpressionNode | void {
    node.parameterList =
      this.descendIntoNode(node.parameterList, buildPath(node, path)) ??
      node.parameterList
    node.body =
      this.descendIntoNode(node.body, buildPath(node, path)) ?? node.body
    return this.visitNode(node, path)
  }

  visitHTML(node: HTMLNode, path: ASTNode[]): HTMLNode | void {
    let hasModifiedChildren = false
    const children: ExpressionNode[] = []
    for (const child of node.children) {
      const result = this.descendIntoNode(child, buildPath(node, path))
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

  visitIf(node: IfNode, path: ASTNode[]): IfNode | void {
    node.condition =
      this.descendIntoNode(node.condition, buildPath(node, path)) ??
      node.condition
    node.body =
      this.descendIntoNode(node.body, buildPath(node, path)) ?? node.body
    node.else_ = node.else_
      ? this.descendIntoNode(node.else_, buildPath(node, path)) ?? node.else_
      : node.else_
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
      const result = this.descendIntoNode(property, buildPath(node, path))
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
    node.left =
      this.descendIntoNode(node.left, buildPath(node, path)) ?? node.left

    let hasModifiedRights = false
    const rights: IdentifierNode[] = []
    for (const right of node.rights) {
      const result = this.descendIntoNode(right, buildPath(node, path))
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
      this.descendIntoNode(node.expression, buildPath(node, path)) ??
      node.expression
    return this.visitNode(node, path)
  }

  visitVariableAccess(
    node: VariableAccessNode,
    path: ASTNode[]
  ): VariableAccessNode | void {
    node.name =
      this.descendIntoNode(node.name, buildPath(node, path)) ?? node.name
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
      this.descendIntoNode(node.name, buildPath(node, path)) ?? node.name
    node.parameterList =
      this.descendIntoNode(node.parameterList, buildPath(node, path)) ??
      node.parameterList
    node.returnType = node.returnType
      ? this.descendIntoNode(node.returnType, buildPath(node, path)) ??
        node.returnType
      : null
    node.body =
      this.descendIntoNode(node.body, buildPath(node, path)) ?? node.body
    return this.visitNode(node, path)
  }

  visitNamedType(node: NamedTypeNode, path: ASTNode[]): NamedTypeNode | void {
    let hasModifiedGenerics = false
    const genericArguments: TypeNode[] = []
    for (const genericArgument of node.genericArguments) {
      const result = this.descendIntoNode(
        genericArgument,
        buildPath(node, path)
      )
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
    node.key = this.descendIntoNode(node.key, buildPath(node, path)) ?? node.key
    node.value =
      this.descendIntoNode(node.value, buildPath(node, path)) ?? node.value
    return this.visitNode(node, path)
  }

  visitObjectType(
    node: ObjectTypeNode,
    path: ASTNode[]
  ): ObjectTypeNode | void {
    let hasModifiedProperties = false
    const properties: TypePropertyNode[] = []

    for (const property of node.properties) {
      const result = this.descendIntoNode(property, buildPath(node, path))
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
      const result = this.descendIntoNode(parameter, buildPath(node, path))
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
      this.descendIntoNode(node.name, buildPath(node, path)) ?? node.name
    node.type = node.type
      ? this.descendIntoNode(node.type, buildPath(node, path)) ?? node.type
      : node.type
    return this.visitNode(node, path)
  }

  visitProgram(node: ProgramNode): ProgramNode | void {
    let hasModifiedStatements = false
    const statements: (TypeDefinitionNode | Statement)[] = []

    for (const statement of node.statements) {
      const result = this.descendIntoNode(statement, [node])
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
      this.descendIntoNode(node.value, buildPath(node, path)) ?? node.value
    return this.visitNode(node, path)
  }

  visitRender(node: RenderNode, path: ASTNode[]): RenderNode | void {
    node.body =
      this.descendIntoNode(node.body, buildPath(node, path)) ?? node.body
    return this.visitNode(node, path)
  }

  visitTypeDefinition(
    node: TypeDefinitionNode,
    path: ASTNode[]
  ): TypeDefinitionNode | void {
    node.name =
      this.descendIntoNode(node.name, buildPath(node, path)) ?? node.name
    node.type =
      this.descendIntoNode(node.type, buildPath(node, path)) ?? node.type
    return this.visitNode(node, path)
  }

  visitTypeProperty(
    node: TypePropertyNode,
    path: ASTNode[]
  ): TypePropertyNode | void {
    node.name =
      this.descendIntoNode(node.name, buildPath(node, path)) ?? node.name
    node.type =
      this.descendIntoNode(node.type, buildPath(node, path)) ?? node.type
    return this.visitNode(node, path)
  }

  visitUse(node: UseNode, path: ASTNode[]): UseNode | void {
    let hasModifiedSelectors = false
    const selectors: UseSelectorNode[] = []
    for (const selector of node.selectors) {
      const result = this.descendIntoNode(selector, buildPath(node, path))
      if (result) {
        hasModifiedSelectors = true
        selectors.push(result)
      } else {
        selectors.push(selector)
      }
    }

    if (hasModifiedSelectors) {
      node.selectors = selectors
    }
    return this.visitNode(node, path)
  }

  visitUseSelector(
    node: UseSelectorNode,
    path: ASTNode[]
  ): UseSelectorNode | void {
    node.name = isNodeType('Identifier')(node.name)
      ? this.descendIntoNode(node.name, buildPath(node, path)) ?? node.name
      : node.name
    node.alias = node.alias
      ? this.descendIntoNode(node.alias, buildPath(node, path)) ?? node.alias
      : node.alias
    return this.visitNode(node, path)
  }

  visitVariableDeclaration(
    node: VariableDeclarationNode,
    path: ASTNode[]
  ): VariableDeclarationNode | void {
    node.identifier =
      this.descendIntoNode(node.identifier, buildPath(node, path)) ??
      node.identifier
    node.type = node.type
      ? this.descendIntoNode(node.type, buildPath(node, path)) ?? node.type
      : node.type
    node.initializer =
      this.descendIntoNode(node.initializer, buildPath(node, path)) ??
      node.initializer
    return this.visitNode(node, path)
  }
}
