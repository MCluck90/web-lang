import {
  AstNode,
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

export interface AstReducer<TOutput, TAstNode extends AstNode = AstNode> {
  visitNode(node: TAstNode, path: TAstNode[]): TOutput
  visitProgram(node: ProgramNode): TOutput
  visitAnonymousType(node: AnonymousTypeNode, path: TAstNode[]): TOutput
  visitArgumentList(node: ArgumentListNode, path: TAstNode[]): TOutput
  visitAssignment(node: AssignmentNode, path: TAstNode[]): TOutput
  visitBinaryExpression(node: BinaryExpressionNode, path: TAstNode[]): TOutput
  visitBlock(node: BlockNode, path: TAstNode[]): TOutput
  visitBoolean(node: BooleanNode, path: TAstNode[]): TOutput
  visitElse(node: ElseNode, path: TAstNode[]): TOutput
  visitFloatingPoint(node: FloatingPointNode, path: TAstNode[]): TOutput
  visitFunctionCall(node: FunctionCallNode, path: TAstNode[]): TOutput
  visitFunctionExpression(
    node: FunctionExpressionNode,
    path: TAstNode[]
  ): TOutput
  visitHTML(node: HTMLNode, path: TAstNode[]): TOutput
  visitIdentifier(node: IdentifierNode, path: TAstNode[]): TOutput
  visitIf(node: IfNode, path: TAstNode[]): TOutput
  visitInteger(node: IntegerNode, path: TAstNode[]): TOutput
  visitJsAsm(node: JsAsmNode, path: TAstNode[]): TOutput
  visitMethodDefinition(node: MethodDefinitionNode, path: TAstNode[]): TOutput
  visitNamedType(node: NamedTypeNode, path: TAstNode[]): TOutput
  visitObjectLiteral(node: ObjectLiteralNode, path: TAstNode[]): TOutput
  visitObjectProperty(node: ObjectPropertyNode, path: TAstNode[]): TOutput
  visitObjectType(node: ObjectTypeNode, path: TAstNode[]): TOutput
  visitParameter(node: ParameterNode, path: TAstNode[]): TOutput
  visitParameterList(node: ParameterListNode, path: TAstNode[]): TOutput
  visitPropertyAccess(node: PropertyAccessNode, path: TAstNode[]): TOutput
  visitPropertyKey(node: PropertyKeyNode, path: TAstNode[]): TOutput
  visitRender(node: RenderNode, path: TAstNode[]): TOutput
  visitString(node: StringNode, path: TAstNode[]): TOutput
  visitTypeDefinition(node: TypeDefinitionNode, path: TAstNode[]): TOutput
  visitTypeProperty(node: TypePropertyNode, path: TAstNode[]): TOutput
  visitUnaryExpression(node: UnaryExpressionNode, path: TAstNode[]): TOutput
  visitUse(node: UseNode, path: TAstNode[]): TOutput
  visitUseSelector(node: UseSelectorNode, path: TAstNode[]): TOutput
  visitVariableAccess(node: VariableAccessNode, path: TAstNode[]): TOutput
  visitVariableAttributeList(
    node: VariableAttributeListNode,
    path: TAstNode[]
  ): TOutput
  visitVariableDeclaration(
    node: VariableDeclarationNode,
    path: TAstNode[]
  ): TOutput
  visitWhile(node: WhileNode, path: TAstNode[]): TOutput
}

export interface AstVisitor<
  TInputAstNode extends AstNode = AstNode,
  TOutputAstNode extends AstNode | void = TInputAstNode,
  TOutputWithoutVoid = Exclude<TOutputAstNode, void>
> {
  visitAnonymousType(
    node: TInputAstNode & { __type: AnonymousTypeNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: AnonymousTypeNode['__type'] }) | TOutputAstNode
  visitArgumentList(
    node: TInputAstNode & { __type: ArgumentListNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ArgumentListNode['__type'] }) | TOutputAstNode
  visitAssignment(
    node: TInputAstNode & { __type: AssignmentNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: AssignmentNode['__type'] }) | TOutputAstNode
  visitBinaryExpression(
    node: TInputAstNode & { __type: BinaryExpressionNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ExpressionNode['__type'] }) | TOutputAstNode
  visitBlock(
    node: TInputAstNode & { __type: BlockNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: BlockNode['__type'] }) | TOutputAstNode
  visitBoolean(
    node: TInputAstNode & { __type: BooleanNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: BooleanNode['__type'] }) | TOutputAstNode
  visitElse(
    node: TInputAstNode & { __type: ElseNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ElseNode['__type'] }) | TOutputAstNode
  visitFloatingPoint(
    node: TInputAstNode & { __type: FloatingPointNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: FloatingPointNode['__type'] }) | TOutputAstNode
  visitFunctionCall(
    node: TInputAstNode & { __type: FunctionCallNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: FunctionCallNode['__type'] }) | TOutputAstNode
  visitFunctionExpression(
    node: TInputAstNode & { __type: FunctionExpressionNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ):
    | (TInputAstNode & { __type: FunctionExpressionNode['__type'] })
    | TOutputAstNode
  visitHTML(
    node: TInputAstNode & { __type: HTMLNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: HTMLNode['__type'] }) | TOutputAstNode
  visitIdentifier(
    node: TInputAstNode & { __type: IdentifierNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: IdentifierNode['__type'] }) | TOutputAstNode
  visitIf(
    node: TInputAstNode & { __type: IfNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: IfNode['__type'] }) | TOutputAstNode
  visitInteger(
    node: TInputAstNode & { __type: IntegerNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: IntegerNode['__type'] }) | TOutputAstNode
  visitJsAsm(
    node: TInputAstNode & { __type: JsAsmNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: JsAsmNode['__type'] }) | TOutputAstNode
  visitMethodDefinition(
    node: TInputAstNode & { __type: MethodDefinitionNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ):
    | (TInputAstNode & { __type: MethodDefinitionNode['__type'] })
    | TOutputAstNode
  visitNamedType(
    node: TInputAstNode & { __type: NamedTypeNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: NamedTypeNode['__type'] }) | TOutputAstNode
  visitNode(
    node: TInputAstNode & { __type: TInputAstNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: AstNode['__type'] }) | TOutputAstNode
  visitObjectLiteral(
    node: TInputAstNode & { __type: ObjectLiteralNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ObjectLiteralNode['__type'] }) | TOutputAstNode
  visitObjectProperty(
    node: TInputAstNode & { __type: ObjectPropertyNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ObjectPropertyNode['__type'] }) | TOutputAstNode
  visitObjectType(
    node: TInputAstNode & { __type: ObjectTypeNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ObjectTypeNode['__type'] }) | TOutputAstNode
  visitParameter(
    node: TInputAstNode & { __type: ParameterNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ParameterNode['__type'] }) | TOutputAstNode
  visitParameterList(
    node: TInputAstNode & { __type: ParameterListNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: ParameterListNode['__type'] }) | TOutputAstNode
  visitProgram(
    node: TInputAstNode & { __type: ProgramNode['__type'] }
  ): (TInputAstNode & { __type: ProgramNode['__type'] }) | TOutputAstNode
  visitPropertyAccess(
    node: TInputAstNode & { __type: PropertyAccessNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: PropertyAccessNode['__type'] }) | TOutputAstNode
  visitPropertyKey(
    node: TInputAstNode & { __type: PropertyKeyNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: PropertyKeyNode['__type'] }) | TOutputAstNode
  visitRender(
    node: TInputAstNode & { __type: RenderNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: RenderNode['__type'] }) | TOutputAstNode
  visitString(
    node: TInputAstNode & { __type: StringNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: StringNode['__type'] }) | TOutputAstNode
  visitTypeDefinition(
    node: TInputAstNode & { __type: TypeDefinitionNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: TypeDefinitionNode['__type'] }) | TOutputAstNode
  visitTypeProperty(
    node: TInputAstNode & { __type: TypePropertyNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: TypePropertyNode['__type'] }) | TOutputAstNode
  visitUnaryExpression(
    node: TInputAstNode & { __type: UnaryExpressionNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ):
    | (TInputAstNode & { __type: UnaryExpressionNode['__type'] })
    | TOutputAstNode
  visitUse(
    node: TInputAstNode & { __type: UseNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: UseNode['__type'] }) | TOutputAstNode
  visitUseSelector(
    node: TInputAstNode & { __type: UseSelectorNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: UseSelectorNode['__type'] }) | TOutputAstNode
  visitVariableAccess(
    node: TInputAstNode & { __type: VariableAccessNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: VariableAccessNode['__type'] }) | TOutputAstNode
  visitVariableAttributeList(
    node: TInputAstNode & { __type: VariableAttributeListNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ):
    | (TInputAstNode & { __type: VariableAttributeListNode['__type'] })
    | TOutputAstNode
  visitVariableDeclaration(
    node: TInputAstNode & { __type: VariableDeclarationNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ):
    | (TInputAstNode & { __type: VariableDeclarationNode['__type'] })
    | TOutputAstNode
  visitWhile(
    node: TInputAstNode & { __type: WhileNode['__type'] },
    path: (TInputAstNode | TOutputWithoutVoid)[]
  ): (TInputAstNode & { __type: WhileNode['__type'] }) | TOutputAstNode
}

export class DepthFirstVisitor<
  TInputAstNode extends AstNode,
  TOutputAstNode extends AstNode = AstNode
> implements AstVisitor<TInputAstNode, TOutputAstNode>
{
  constructor(private readonly visitors: Partial<AstVisitor<TInputAstNode>>) {}

  visitNode<T extends TInputAstNode>(node: T, path: TInputAstNode[]) {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const specificResult = (this.visitors[`visit${node.__type}`] as any)?.(
      node as never,
      path
    )
    const genericResult = this.visitors.visitNode?.(node, path)
    return specificResult ?? genericResult ?? node
  }

  private descendIntoNode<T extends AstNode>(
    node: T,
    path: TInputAstNode[]
  ): T | void {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (this[`visit${node.__type}`] as any)(node, path)
  }

  visitAssignment(
    node: TInputAstNode & { __type: 'Assignment' },
    path: TInputAstNode[]
  ) {
    node.left = this.descendIntoNode(node.left, [...path, node]) ?? node.left
    node.right = this.descendIntoNode(node.right, [...path, node]) ?? node.right
    return this.visitNode(node, path)
  }

  visitAnonymousType(
    node: TInputAstNode & { __type: AnonymousTypeNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.type = this.descendIntoNode(node.type, [...path, node]) ?? node.type
    return this.visitNode(node, path)
  }

  visitArgumentList(
    node: TInputAstNode & { __type: ArgumentListNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: BlockNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: BinaryExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.left = this.descendIntoNode(node.left, [...path, node]) ?? node.left
    node.right = this.descendIntoNode(node.right, [...path, node]) ?? node.right
    return this.visitNode(node, path)
  }

  visitBoolean(
    node: TInputAstNode & { __type: BooleanNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitElse(
    node: TInputAstNode & { __type: ElseNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitFloatingPoint(
    node: TInputAstNode & { __type: FloatingPointNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitFunctionCall(
    node: TInputAstNode & { __type: FunctionCallNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.callee =
      this.descendIntoNode(node.callee, [...path, node]) ?? node.callee
    node.argumentList =
      this.descendIntoNode(node.argumentList, [...path, node]) ??
      node.argumentList
    return this.visitNode(node, path)
  }

  visitFunctionExpression(
    node: TInputAstNode & { __type: FunctionExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.parameterList =
      this.descendIntoNode(node.parameterList, [...path, node]) ??
      node.parameterList
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitHTML(
    node: TInputAstNode & { __type: HTMLNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: IntegerNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitIf(
    node: TInputAstNode & { __type: IfNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.condition =
      this.descendIntoNode(node.condition, [...path, node]) ?? node.condition
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    node.else_ = node.else_
      ? this.descendIntoNode(node.else_, [...path, node]) ?? node.else_
      : node.else_
    return this.visitNode(node, path)
  }

  visitJsAsm(
    node: TInputAstNode & { __type: JsAsmNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitObjectLiteral(
    node: TInputAstNode & { __type: ObjectLiteralNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: PropertyAccessNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: StringNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitUnaryExpression(
    node: TInputAstNode & { __type: UnaryExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.expression =
      this.descendIntoNode(node.expression, [...path, node]) ?? node.expression
    return this.visitNode(node, path)
  }

  visitVariableAccess(
    node: TInputAstNode & { __type: VariableAccessNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    return this.visitNode(node, path)
  }

  visitVariableAttributeList(
    node: TInputAstNode & { __type: VariableAttributeListNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: IdentifierNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitMethodDefinition(
    node: TInputAstNode & { __type: MethodDefinitionNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: NamedTypeNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: ObjectPropertyNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.key = this.descendIntoNode(node.key, [...path, node]) ?? node.key
    node.value = this.descendIntoNode(node.value, [...path, node]) ?? node.value
    return this.visitNode(node, path)
  }

  visitObjectType(
    node: TInputAstNode & { __type: ObjectTypeNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: ParameterListNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: ParameterNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.type = node.type
      ? this.descendIntoNode(node.type, [...path, node]) ?? node.type
      : node.type
    return this.visitNode(node, path)
  }

  visitProgram(node: TInputAstNode & { __type: ProgramNode['__type'] }) {
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
    node: TInputAstNode & { __type: PropertyKeyNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.value = this.descendIntoNode(node.value, [...path, node]) ?? node.value
    return this.visitNode(node, path)
  }

  visitRender(
    node: TInputAstNode & { __type: RenderNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }

  visitTypeDefinition(
    node: TInputAstNode & { __type: TypeDefinitionNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.type = this.descendIntoNode(node.type, [...path, node]) ?? node.type
    return this.visitNode(node, path)
  }

  visitTypeProperty(
    node: TInputAstNode & { __type: TypePropertyNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.name = this.descendIntoNode(node.name, [...path, node]) ?? node.name
    node.type = this.descendIntoNode(node.type, [...path, node]) ?? node.type
    return this.visitNode(node, path)
  }

  visitUse(
    node: TInputAstNode & { __type: UseNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: UseSelectorNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: VariableDeclarationNode['__type'] },
    path: TInputAstNode[]
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
    node: TInputAstNode & { __type: WhileNode['__type'] },
    path: TInputAstNode[]
  ) {
    node.condition =
      this.descendIntoNode(node.condition, [...path, node]) ?? node.condition
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return this.visitNode(node, path)
  }
}

export class CustomOrderVisitor<
  TInputAstNode extends AstNode,
  TOutputAstNode extends AstNode = TInputAstNode
> implements AstVisitor<TInputAstNode, TOutputAstNode>
{
  constructor(
    private readonly visitors: Partial<
      AstVisitor<TInputAstNode, TOutputAstNode | void>
    >
  ) {}

  private hasCustomVisitor<T extends TInputAstNode>(node: T) {
    return this.visitors[`visit${node['__type']}`]
  }

  descendIntoNode<T extends AstNode>(node: T, path: TInputAstNode[]): T | void {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (this[`visit${node.__type}`] as any)(node, path)
  }

  visitNode<T extends TInputAstNode>(node: T, path: TInputAstNode[]) {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const specificResult = (this.visitors[`visit${node.__type}`] as any)?.(
      node as never,
      path
    )
    const genericResult = this.visitors.visitNode?.(node, path)
    return specificResult ?? genericResult ?? node
  }

  visitAnonymousType(
    node: TInputAstNode & { __type: AnonymousTypeNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const anonymousType = this.visitNode(node, path)
    node.type =
      this.descendIntoNode(anonymousType.type, [...path, node]) ?? node.type
    return anonymousType
  }

  visitArgumentList(
    node: TInputAstNode & { __type: ArgumentListNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitAssignment(
    node: TInputAstNode & { __type: AssignmentNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitBinaryExpression(
    node: TInputAstNode & { __type: BinaryExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitBlock(
    node: TInputAstNode & { __type: BlockNode['__type'] },
    path: TInputAstNode[]
  ) {
    const block = this.visitNode(node, path)

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

    return block
  }

  visitBoolean(
    node: TInputAstNode & { __type: BooleanNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitElse(
    node: TInputAstNode & { __type: ElseNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitFloatingPoint(
    node: TInputAstNode & { __type: FloatingPointNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitFunctionCall(
    node: TInputAstNode & { __type: FunctionCallNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitFunctionExpression(
    node: TInputAstNode & { __type: FunctionExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitHTML(
    node: TInputAstNode & { __type: HTMLNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitIdentifier(
    node: TInputAstNode & { __type: IdentifierNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitIf(
    node: TInputAstNode & { __type: IfNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitInteger(
    node: TInputAstNode & { __type: IntegerNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitJsAsm(
    node: TInputAstNode & { __type: JsAsmNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitMethodDefinition(
    node: TInputAstNode & { __type: MethodDefinitionNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitNamedType(
    node: TInputAstNode & { __type: NamedTypeNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitObjectLiteral(
    node: TInputAstNode & { __type: ObjectLiteralNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitObjectProperty(
    node: TInputAstNode & { __type: ObjectPropertyNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitObjectType(
    node: TInputAstNode & { __type: ObjectTypeNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: ParameterNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitParameterList(
    node: TInputAstNode & { __type: ParameterListNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitProgram(node: TInputAstNode & { __type: ProgramNode['__type'] }) {
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
    node: TInputAstNode & { __type: PropertyAccessNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitPropertyKey(
    node: TInputAstNode & { __type: PropertyKeyNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitRender(
    node: TInputAstNode & { __type: RenderNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const result = this.visitNode(node, path)
    node.body = this.descendIntoNode(node.body, [...path, node]) ?? node.body
    return result
  }

  visitString(
    node: TInputAstNode & { __type: StringNode['__type'] },
    path: TInputAstNode[]
  ) {
    return this.visitNode(node, path)
  }

  visitTypeDefinition(
    node: TInputAstNode & { __type: TypeDefinitionNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitTypeProperty(
    node: TInputAstNode & { __type: TypePropertyNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitUnaryExpression(
    node: TInputAstNode & { __type: UnaryExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitUse(
    node: TInputAstNode & { __type: UseNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: UseSelectorNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }

  visitVariableAccess(
    node: TInputAstNode & { __type: VariableAccessNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: VariableAttributeListNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: VariableDeclarationNode['__type'] },
    path: TInputAstNode[]
  ) {
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
    node: TInputAstNode & { __type: WhileNode['__type'] },
    path: TInputAstNode[]
  ) {
    throw new Error('Method not implemented.')
    return node
  }
}
