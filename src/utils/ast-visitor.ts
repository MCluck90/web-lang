import {
  ASTNode,
  AnonymousTypeNode,
  ArgumentListNode,
  AssignmentNode,
  BinaryExpressionNode,
  BlockNode,
  BooleanNode,
  ElseNode,
  ExpressionNode,
  FloatingPointNode,
  FunctionCallNode,
  FunctionExpressionNode,
  HTMLNode,
  IdentifierNode,
  IfNode,
  IntegerNode,
  JsAsmNode,
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
  Statement,
  StringNode,
  TypeDefinitionNode,
  TypeNode,
  TypePropertyNode,
  UnaryExpressionNode,
  UseNode,
  UseSelectorNode,
  VariableAccessNode,
  VariableAttributeListNode,
  VariableDeclarationNode,
  WhileNode,
  isNodeType,
} from '../parser/ast'

export interface AstMapper<T> {
  visitNode(node: ASTNode, path: ASTNode[]): T
  visitProgram(node: ProgramNode): T
  visitAnonymousType(node: AnonymousTypeNode, path: ASTNode[]): T
  visitArgumentList(node: ArgumentListNode, path: ASTNode[]): T
  visitAssignment(node: AssignmentNode, path: ASTNode[]): T
  visitBinaryExpression(node: BinaryExpressionNode, path: ASTNode[]): T
  visitBlock(node: BlockNode, path: ASTNode[]): T
  visitBoolean(node: BooleanNode, path: ASTNode[]): T
  visitElse(node: ElseNode, path: ASTNode[]): T
  visitFloatingPoint(node: FloatingPointNode, path: ASTNode[]): T
  visitFunctionCall(node: FunctionCallNode, path: ASTNode[]): T
  visitFunctionExpression(node: FunctionExpressionNode, path: ASTNode[]): T
  visitHTML(node: HTMLNode, path: ASTNode[]): T
  visitIdentifier(node: IdentifierNode, path: ASTNode[]): T
  visitIf(node: IfNode, path: ASTNode[]): T
  visitInteger(node: IntegerNode, path: ASTNode[]): T
  visitJsAsm(node: JsAsmNode, path: ASTNode[]): T
  visitMethodDefinition(node: MethodDefinitionNode, path: ASTNode[]): T
  visitNamedType(node: NamedTypeNode, path: ASTNode[]): T
  visitObjectLiteral(node: ObjectLiteralNode, path: ASTNode[]): T
  visitObjectProperty(node: ObjectPropertyNode, path: ASTNode[]): T
  visitObjectType(node: ObjectTypeNode, path: ASTNode[]): T
  visitParameter(node: ParameterNode, path: ASTNode[]): T
  visitParameterList(node: ParameterListNode, path: ASTNode[]): T
  visitPropertyAccess(node: PropertyAccessNode, path: ASTNode[]): T
  visitPropertyKey(node: PropertyKeyNode, path: ASTNode[]): T
  visitRender(node: RenderNode, path: ASTNode[]): T
  visitString(node: StringNode, path: ASTNode[]): T
  visitTypeDefinition(node: TypeDefinitionNode, path: ASTNode[]): T
  visitTypeProperty(node: TypePropertyNode, path: ASTNode[]): T
  visitUnaryExpression(node: UnaryExpressionNode, path: ASTNode[]): T
  visitUse(node: UseNode, path: ASTNode[]): T
  visitUseSelector(node: UseSelectorNode, path: ASTNode[]): T
  visitVariableAccess(node: VariableAccessNode, path: ASTNode[]): T
  visitVariableAttributeList(
    node: VariableAttributeListNode,
    path: ASTNode[]
  ): T
  visitVariableDeclaration(node: VariableDeclarationNode, path: ASTNode[]): T
  visitWhile(node: WhileNode, path: ASTNode[]): T
}

export interface AstVisitor<
  TAstNode extends ASTNode = ASTNode,
  TReturn = void
> {
  visitAnonymousType(
    node: TAstNode & { __type: AnonymousTypeNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: AnonymousTypeNode['__type'] }) | TReturn
  visitArgumentList(
    node: TAstNode & { __type: ArgumentListNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ArgumentListNode['__type'] }) | TReturn
  visitAssignment(
    node: TAstNode & { __type: AssignmentNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: AssignmentNode['__type'] }) | TReturn
  visitBinaryExpression(
    node: TAstNode & { __type: BinaryExpressionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ExpressionNode['__type'] }) | TReturn
  visitBlock(
    node: TAstNode & { __type: BlockNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: BlockNode['__type'] }) | TReturn
  visitBoolean(
    node: TAstNode & { __type: BooleanNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: BooleanNode['__type'] }) | TReturn
  visitElse(
    node: TAstNode & { __type: ElseNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ElseNode['__type'] }) | TReturn
  visitFloatingPoint(
    node: TAstNode & { __type: FloatingPointNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: FloatingPointNode['__type'] }) | TReturn
  visitFunctionCall(
    node: TAstNode & { __type: FunctionCallNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: FunctionCallNode['__type'] }) | TReturn
  visitFunctionExpression(
    node: TAstNode & { __type: FunctionExpressionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: FunctionExpressionNode['__type'] }) | TReturn
  visitHTML(
    node: TAstNode & { __type: HTMLNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: HTMLNode['__type'] }) | TReturn
  visitIdentifier(
    node: TAstNode & { __type: IdentifierNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: IdentifierNode['__type'] }) | TReturn
  visitIf(
    node: TAstNode & { __type: IfNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: IfNode['__type'] }) | TReturn
  visitInteger(
    node: TAstNode & { __type: IntegerNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: IntegerNode['__type'] }) | TReturn
  visitJsAsm(
    node: TAstNode & { __type: JsAsmNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: JsAsmNode['__type'] }) | TReturn
  visitMethodDefinition(
    node: TAstNode & { __type: MethodDefinitionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: MethodDefinitionNode['__type'] }) | TReturn
  visitNamedType(
    node: TAstNode & { __type: NamedTypeNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: NamedTypeNode['__type'] }) | TReturn
  visitNode(
    node: TAstNode & { __type: TAstNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ASTNode['__type'] }) | TReturn
  visitObjectLiteral(
    node: TAstNode & { __type: ObjectLiteralNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ObjectLiteralNode['__type'] }) | TReturn
  visitObjectProperty(
    node: TAstNode & { __type: ObjectPropertyNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ObjectPropertyNode['__type'] }) | TReturn
  visitObjectType(
    node: TAstNode & { __type: ObjectTypeNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ObjectTypeNode['__type'] }) | TReturn
  visitParameter(
    node: TAstNode & { __type: ParameterNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ParameterNode['__type'] }) | TReturn
  visitParameterList(
    node: TAstNode & { __type: ParameterListNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ParameterListNode['__type'] }) | TReturn
  visitProgram(
    node: TAstNode & { __type: ProgramNode['__type'] }
  ): (TAstNode & { __type: ProgramNode['__type'] }) | TReturn
  visitPropertyAccess(
    node: TAstNode & { __type: PropertyAccessNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: PropertyAccessNode['__type'] }) | TReturn
  visitPropertyKey(
    node: TAstNode & { __type: PropertyKeyNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: PropertyKeyNode['__type'] }) | TReturn
  visitRender(
    node: TAstNode & { __type: RenderNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: RenderNode['__type'] }) | TReturn
  visitString(
    node: TAstNode & { __type: StringNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: StringNode['__type'] }) | TReturn
  visitTypeDefinition(
    node: TAstNode & { __type: TypeDefinitionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: TypeDefinitionNode['__type'] }) | TReturn
  visitTypeProperty(
    node: TAstNode & { __type: TypePropertyNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: TypePropertyNode['__type'] }) | TReturn
  visitUnaryExpression(
    node: TAstNode & { __type: UnaryExpressionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: UnaryExpressionNode['__type'] }) | TReturn
  visitUse(
    node: TAstNode & { __type: UseNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: UseNode['__type'] }) | TReturn
  visitUseSelector(
    node: TAstNode & { __type: UseSelectorNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: UseSelectorNode['__type'] }) | TReturn
  visitVariableAccess(
    node: TAstNode & { __type: VariableAccessNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: VariableAccessNode['__type'] }) | TReturn
  visitVariableAttributeList(
    node: TAstNode & { __type: VariableAttributeListNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: VariableAttributeListNode['__type'] }) | TReturn
  visitVariableDeclaration(
    node: TAstNode & { __type: VariableDeclarationNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: VariableDeclarationNode['__type'] }) | TReturn
  visitWhile(
    node: TAstNode & { __type: WhileNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: WhileNode['__type'] }) | TReturn
}

export class DepthFirstVisitor<TAstNode extends ASTNode>
  implements AstVisitor<TAstNode>
{
  constructor(private readonly visitors: Partial<AstVisitor<TAstNode>>) {}

  visitNode<T extends TAstNode>(node: T, path: TAstNode[]) {
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
    path: TAstNode[]
  ): T | void {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (this[`visit${node.__type}`] as any)(node, path)
  }

  visitAssignment(
    node: TAstNode & { __type: 'Assignment' },
    path: TAstNode[]
  ): void | (TAstNode & { __type: 'Assignment' }) {
    node.left = this.descendIntoNode(node.left, [...path, node]) ?? node.left
    node.right = this.descendIntoNode(node.right, [...path, node]) ?? node.right
    return this.visitNode(node, path)
  }

  visitAnonymousType(
    node: TAstNode & { __type: AnonymousTypeNode['__type'] },
    path: TAstNode[]
  ) {
    node.type = this.descendIntoNode(node.type, [...path, node]) ?? node.type
    return this.visitNode(node, path)
  }

  visitArgumentList(
    node: TAstNode & { __type: ArgumentListNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedArguments = false
    const args: ExpressionNode[] = []
    for (const arg of node.arguments) {
      const result = this.descendIntoNode(arg, [...path, node])
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

  visitBlock(
    node: TAstNode & { __type: BlockNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedStatements = false
    const statements: Statement[] = []

    for (const statement of statements) {
      const result = this.descendIntoNode(statement, [...path, node])
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
    node: TAstNode & { __type: BinaryExpressionNode['__type'] },
    path: TAstNode[]
  ) {
    node.left = this.descendIntoNode(node.left, [...path, node]) ?? node.left
    node.right = this.descendIntoNode(node.right, [...path, node]) ?? node.right
    return this.visitNode(node, path)
  }

  visitBoolean(
    node: TAstNode & { __type: BooleanNode['__type'] },
    path: TAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitElse(node: TAstNode & { __type: ElseNode['__type'] }, path: TAstNode[]) {
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitFloatingPoint(
    node: TAstNode & { __type: FloatingPointNode['__type'] },
    path: TAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitFunctionCall(
    node: TAstNode & { __type: FunctionCallNode['__type'] },
    path: TAstNode[]
  ) {
    node.callee =
      this.descendIntoNode(node.callee, [...path, node]) ?? node.callee
    node.argumentList =
      this.descendIntoNode(node.argumentList, [...path, node]) ??
      node.argumentList
    return this.visitNode(node, path)
  }

  visitFunctionExpression(
    node: TAstNode & { __type: FunctionExpressionNode['__type'] },
    path: TAstNode[]
  ) {
    node.parameterList =
      this.descendIntoNode(node.parameterList, [...path, node]) ??
      node.parameterList
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitHTML(node: TAstNode & { __type: HTMLNode['__type'] }, path: TAstNode[]) {
    let hasModifiedChildren = false
    const children: ExpressionNode[] = []
    for (const child of node.children) {
      const result = this.descendIntoNode(child, [...path, node])
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

  visitInteger(
    node: TAstNode & { __type: IntegerNode['__type'] },
    path: TAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitIf(node: TAstNode & { __type: IfNode['__type'] }, path: TAstNode[]) {
    node.condition =
      this.descendIntoNode(node.condition, [...path, node]) ?? node.condition
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    node.else_ = node.else_
      ? this.descendIntoNode(node.else_, [...path, node]) ?? node.else_
      : node.else_
    return this.visitNode(node, path)
  }

  visitJsAsm(
    node: TAstNode & { __type: JsAsmNode['__type'] },
    path: TAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitObjectLiteral(
    node: TAstNode & { __type: ObjectLiteralNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedProperties = false
    const properties: ObjectPropertyNode[] = []
    for (const property of node.properties) {
      const result = this.descendIntoNode(property, [...path, node])
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
    node: TAstNode & { __type: PropertyAccessNode['__type'] },
    path: TAstNode[]
  ) {
    node.left = this.descendIntoNode(node.left, [...path, node]) ?? node.left

    let hasModifiedRights = false
    const rights: IdentifierNode[] = []
    for (const right of node.rights) {
      const result = this.descendIntoNode(right, [...path, node])
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

  visitString(
    node: TAstNode & { __type: StringNode['__type'] },
    path: TAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitUnaryExpression(
    node: TAstNode & { __type: UnaryExpressionNode['__type'] },
    path: TAstNode[]
  ) {
    node.expression =
      this.descendIntoNode(node.expression, [...path, node]) ?? node.expression
    return this.visitNode(node, path)
  }

  visitVariableAccess(
    node: TAstNode & { __type: VariableAccessNode['__type'] },
    path: TAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    return this.visitNode(node, path)
  }

  visitVariableAttributeList(
    node: TAstNode & { __type: VariableAttributeListNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedAttributes = false
    const attributes: IdentifierNode[] = []
    for (const attribute of node.attributes) {
      const result = this.descendIntoNode(attribute, [...path, node])
      if (result) {
        hasModifiedAttributes = true
        attributes.push(result)
      } else {
        attributes.push(attribute)
      }
    }

    if (hasModifiedAttributes) {
      node.attributes = attributes
    }

    return this.visitNode(node, path)
  }

  visitIdentifier(
    node: TAstNode & { __type: IdentifierNode['__type'] },
    path: TAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitMethodDefinition(
    node: TAstNode & { __type: MethodDefinitionNode['__type'] },
    path: TAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.parameterList =
      this.descendIntoNode(node.parameterList, [...path, node]) ??
      node.parameterList
    node.returnType = node.returnType
      ? this.descendIntoNode(node.returnType, [...path, node]) ??
        node.returnType
      : null
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitNamedType(
    node: TAstNode & { __type: NamedTypeNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedGenerics = false
    const genericArguments: TypeNode[] = []
    for (const genericArgument of node.genericArguments) {
      const result = this.descendIntoNode(genericArgument, [...path, node])
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
    node: TAstNode & { __type: ObjectPropertyNode['__type'] },
    path: TAstNode[]
  ) {
    node.key = this.descendIntoNode(node.key, [...path, node]) ?? node.key
    node.value = this.descendIntoNode(node.value, [...path, node]) ?? node.value
    return this.visitNode(node, path)
  }

  visitObjectType(
    node: TAstNode & { __type: ObjectTypeNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedProperties = false
    const properties: TypePropertyNode[] = []

    for (const property of node.properties) {
      const result = this.descendIntoNode(property, [...path, node])
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
    node: TAstNode & { __type: ParameterListNode['__type'] },
    path: TAstNode[]
  ) {
    let hasModifiedParameters = false
    const parameters: ParameterNode[] = []

    for (const parameter of node.parameters) {
      const result = this.descendIntoNode(parameter, [...path, node])
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

  visitParameter(
    node: TAstNode & { __type: ParameterNode['__type'] },
    path: TAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.type = node.type
      ? this.descendIntoNode(node.type, [...path, node]) ?? node.type
      : node.type
    return this.visitNode(node, path)
  }

  visitProgram(node: TAstNode & { __type: ProgramNode['__type'] }) {
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
    node: TAstNode & { __type: PropertyKeyNode['__type'] },
    path: TAstNode[]
  ) {
    node.value = this.descendIntoNode(node.value, [...path, node]) ?? node.value
    return this.visitNode(node, path)
  }

  visitRender(
    node: TAstNode & { __type: RenderNode['__type'] },
    path: TAstNode[]
  ) {
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitTypeDefinition(
    node: TAstNode & { __type: TypeDefinitionNode['__type'] },
    path: TAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.type = this.descendIntoNode(node.type, [...path, node]) ?? node.type
    return this.visitNode(node, path)
  }

  visitTypeProperty(
    node: TAstNode & { __type: TypePropertyNode['__type'] },
    path: TAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.type = this.descendIntoNode(node.type, [...path, node]) ?? node.type
    return this.visitNode(node, path)
  }

  visitUse(node: TAstNode & { __type: UseNode['__type'] }, path: TAstNode[]) {
    let hasModifiedSelectors = false
    const selectors: UseSelectorNode[] = []
    for (const selector of node.selectors) {
      const result = this.descendIntoNode(selector, [...path, node])
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
    node: TAstNode & { __type: UseSelectorNode['__type'] },
    path: TAstNode[]
  ) {
    node.name = isNodeType('Identifier')(node.name)
      ? this.descendIntoNode(node.name, [...path, node]) ?? node.name
      : node.name
    node.alias = node.alias
      ? this.descendIntoNode(node.alias, [...path, node]) ?? node.alias
      : node.alias
    return this.visitNode(node, path)
  }

  visitVariableDeclaration(
    node: TAstNode & { __type: VariableDeclarationNode['__type'] },
    path: TAstNode[]
  ) {
    node.identifier =
      this.descendIntoNode(node.identifier, [...path, node]) ?? node.identifier
    node.type = node.type
      ? this.descendIntoNode(node.type, [...path, node]) ?? node.type
      : node.type
    node.initializer =
      this.descendIntoNode(node.initializer, [...path, node]) ??
      node.initializer
    return this.visitNode(node, path)
  }

  visitWhile(
    node: TAstNode & { __type: WhileNode['__type'] },
    path: TAstNode[]
  ) {
    node.condition =
      this.descendIntoNode(node.condition, [...path, node]) ?? node.condition
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }
}

export class CustomOrderVisitor<TAstNode extends ASTNode>
  implements AstVisitor<TAstNode, void>
{
  constructor(private readonly visitors: Partial<AstVisitor<TAstNode, void>>) {}

  private hasCustomVisitor<T extends TAstNode>(node: T) {
    return this.visitors[`visit${node['__type']}`]
  }

  descendIntoNode<T extends ASTNode>(node: T, path: TAstNode[]): T | void {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (this[`visit${node.__type}`] as any)(node, path)
  }

  visitNode<T extends TAstNode>(node: T, path: TAstNode[]) {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const specificResult = (this.visitors[`visit${node.__type}`] as any)?.(
      node as never,
      path
    )
    const genericResult = this.visitors.visitNode?.(node, path)
    return (specificResult ?? genericResult) as T | void
  }

  visitAnonymousType(
    node: TAstNode & { __type: AnonymousTypeNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: AnonymousTypeNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const result = this.visitNode(node, path)
    const anonymousType = result ?? node
    node.type =
      this.descendIntoNode(anonymousType.type, [...path, node]) ?? node.type
    return result
  }

  visitArgumentList(
    node: TAstNode & { __type: ArgumentListNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ArgumentListNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitAssignment(
    node: TAstNode & { __type: AssignmentNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: AssignmentNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitBinaryExpression(
    node: TAstNode & { __type: BinaryExpressionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ExpressionNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitBlock(
    node: TAstNode & { __type: BlockNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: BlockNode['__type'] }) | void {
    const result = this.visitNode(node, path)
    const block = result ?? node

    let hasModifiedStatements = false
    const statements: Statement[] = []
    for (const statement of block.statements) {
      const statementResult = this.descendIntoNode(statement, [...path, node])
      if (statementResult) {
        hasModifiedStatements = true
        statements.push(statementResult)
      } else {
        statements.push(statement)
      }
    }

    if (hasModifiedStatements) {
      block.statements = statements
    }

    return result
  }

  visitBoolean(
    node: TAstNode & { __type: BooleanNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: BooleanNode['__type'] }) | void {
    return this.visitNode(node, path)
  }

  visitElse(
    node: TAstNode & { __type: ElseNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ElseNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitFloatingPoint(
    node: TAstNode & { __type: FloatingPointNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: FloatingPointNode['__type'] }) | void {
    return this.visitNode(node, path)
  }

  visitFunctionCall(
    node: TAstNode & { __type: FunctionCallNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: FunctionCallNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitFunctionExpression(
    node: TAstNode & { __type: FunctionExpressionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: FunctionExpressionNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitHTML(
    node: TAstNode & { __type: HTMLNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: HTMLNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitIdentifier(
    node: TAstNode & { __type: IdentifierNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: IdentifierNode['__type'] }) | void {
    return this.visitNode(node, path)
  }

  visitIf(
    node: TAstNode & { __type: IfNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: IfNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitInteger(
    node: TAstNode & { __type: IntegerNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: IntegerNode['__type'] }) | void {
    return this.visitNode(node, path)
  }

  visitJsAsm(
    node: TAstNode & { __type: JsAsmNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: JsAsmNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitMethodDefinition(
    node: TAstNode & { __type: MethodDefinitionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: MethodDefinitionNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitNamedType(
    node: TAstNode & { __type: NamedTypeNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: NamedTypeNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitObjectLiteral(
    node: TAstNode & { __type: ObjectLiteralNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ObjectLiteralNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitObjectProperty(
    node: TAstNode & { __type: ObjectPropertyNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ObjectPropertyNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitObjectType(
    node: TAstNode & { __type: ObjectTypeNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ObjectTypeNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const result = this.visitNode(node, path)
    let hasModifiedProperties = false
    const typeProperties: TypePropertyNode[] = []
    for (const property of node.properties) {
      const propertyResult = this.descendIntoNode(property, [...path, node])
      if (propertyResult) {
        hasModifiedProperties = true
        typeProperties.push(propertyResult)
      } else {
        typeProperties.push(property)
      }
    }

    if (hasModifiedProperties) {
      node.properties = typeProperties
    }
    return result
  }

  visitParameter(
    node: TAstNode & { __type: ParameterNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ParameterNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitParameterList(
    node: TAstNode & { __type: ParameterListNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: ParameterListNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitProgram(
    node: TAstNode & { __type: ProgramNode['__type'] }
  ): (TAstNode & { __type: ProgramNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, [])
    }

    const result = this.visitNode(node, [])
    const program = result ?? node
    let hasModifiedUseStatements = false
    const useStatements: UseNode[] = []
    for (const useStatement of program.useStatements) {
      const useStatementResult = this.descendIntoNode(useStatement, [program])
      if (useStatementResult) {
        hasModifiedUseStatements = true
        useStatements.push(useStatementResult)
      } else {
        useStatements.push(useStatement)
      }
    }

    program.useStatements = hasModifiedUseStatements
      ? useStatements
      : program.useStatements

    let hasModifiedStatements = false
    const statements: (Statement | TypeDefinitionNode)[] = []
    for (const statement of program.statements) {
      const statementResult = this.descendIntoNode(statement, [program])
      if (statementResult) {
        hasModifiedStatements = true
        statements.push(statementResult)
      } else {
        statements.push(statement)
      }
    }

    program.statements = hasModifiedStatements ? statements : program.statements

    program.render = program.render
      ? this.descendIntoNode(program.render, [program]) ?? program.render
      : program.render

    return result
  }

  visitPropertyAccess(
    node: TAstNode & { __type: PropertyAccessNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: PropertyAccessNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitPropertyKey(
    node: TAstNode & { __type: PropertyKeyNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: PropertyKeyNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitRender(
    node: TAstNode & { __type: RenderNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: RenderNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const result = this.visitNode(node, path)
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return result
  }

  visitString(
    node: TAstNode & { __type: StringNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: StringNode['__type'] }) | void {
    return this.visitNode(node, path)
  }

  visitTypeDefinition(
    node: TAstNode & { __type: TypeDefinitionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: TypeDefinitionNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitTypeProperty(
    node: TAstNode & { __type: TypePropertyNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: TypePropertyNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitUnaryExpression(
    node: TAstNode & { __type: UnaryExpressionNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: UnaryExpressionNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitUse(
    node: TAstNode & { __type: UseNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: UseNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const result = this.visitNode(node, path)
    const useStatement = result ?? node

    let hasModifiedSelectors = false
    const selectors: UseSelectorNode[] = []
    for (const selector of useStatement.selectors) {
      const selectorResult = this.descendIntoNode(selector, [...path, node])
      if (selectorResult) {
        hasModifiedSelectors = true
        selectors.push(selectorResult)
      } else {
        selectors.push(selector)
      }
    }

    if (hasModifiedSelectors) {
      useStatement.selectors = selectors
    }

    return result
  }

  visitUseSelector(
    node: TAstNode & { __type: UseSelectorNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: UseSelectorNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }

  visitVariableAccess(
    node: TAstNode & { __type: VariableAccessNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: VariableAccessNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const variableAccess = this.visitNode(node, path) ?? node
    variableAccess.name =
      this.descendIntoNode(variableAccess.name, [...path, variableAccess]) ??
      variableAccess.name
    return variableAccess
  }

  visitVariableAttributeList(
    node: TAstNode & { __type: VariableAttributeListNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: VariableAttributeListNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const variableAttributeList = this.visitNode(node, path) ?? node

    let hasModifiedAttributes = false
    const attributes: IdentifierNode[] = []
    for (const attribute of attributes) {
      const result = this.descendIntoNode(attribute, [...path, node])
      if (result) {
        hasModifiedAttributes = true
        attributes.push(result)
      } else {
        attributes.push(attribute)
      }
    }

    if (hasModifiedAttributes) {
      variableAttributeList.attributes = attributes
    }

    return variableAttributeList
  }

  visitVariableDeclaration(
    node: TAstNode & { __type: VariableDeclarationNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: VariableDeclarationNode['__type'] }) | void {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const variableDeclaration = this.visitNode(node, path) ?? node
    let hasModifiedAttributeLists = false
    const attributeLists: VariableAttributeListNode[] = []
    for (const attrList of node.attributeLists) {
      const result = this.descendIntoNode(attrList, [...path, node])
      if (result) {
        hasModifiedAttributeLists = true
        attributeLists.push(result)
      } else {
        attributeLists.push(attrList)
      }
    }

    if (hasModifiedAttributeLists) {
      variableDeclaration.attributeLists = attributeLists
    }

    variableDeclaration.identifier =
      this.descendIntoNode(variableDeclaration.identifier, [...path, node]) ??
      variableDeclaration.identifier
    variableDeclaration.initializer =
      this.descendIntoNode(variableDeclaration.initializer, [
        ...path,
        variableDeclaration,
      ]) ?? variableDeclaration.initializer
    return variableDeclaration
  }

  visitWhile(
    node: TAstNode & { __type: WhileNode['__type'] },
    path: TAstNode[]
  ): (TAstNode & { __type: WhileNode['__type'] }) | void {
    throw new Error('Method not implemented.')
  }
}
