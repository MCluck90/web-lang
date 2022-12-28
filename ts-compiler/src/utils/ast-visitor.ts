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

  descendIntoNode<T extends AstNode>(node: T, path: TInputAstNode[]): T {
    // This is safe. TypeScript just doesn't understand it
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (this[`visit${node.__type}`] as any)(node, path) ?? node
  }

  visitNode<T extends TInputAstNode>(node: T, path: TInputAstNode[]): T {
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
    node.type = this.descendIntoNode(anonymousType.type, [...path, node])
    return anonymousType
  }

  visitArgumentList(
    node: TInputAstNode & { __type: ArgumentListNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const argumentList = this.visitNode(node, path)
    argumentList.arguments = argumentList.arguments.map((arg) =>
      this.descendIntoNode(arg, [...path, argumentList])
    )
    return argumentList
  }

  visitAssignment(
    node: TInputAstNode & { __type: AssignmentNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const assignment = this.visitNode(node, path)
    assignment.left = this.descendIntoNode(assignment.left, [
      ...path,
      assignment,
    ])
    assignment.right = this.descendIntoNode(assignment.right, [
      ...path,
      assignment,
    ])
    return assignment
  }

  visitBinaryExpression(
    node: TInputAstNode & { __type: BinaryExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const binaryExpression = this.visitNode(node, path)
    binaryExpression.left = this.descendIntoNode(binaryExpression.left, [
      ...path,
      binaryExpression,
    ])
    binaryExpression.right = this.descendIntoNode(binaryExpression.right, [
      ...path,
      binaryExpression,
    ])
    return binaryExpression
  }

  visitBlock(
    node: TInputAstNode & { __type: BlockNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const block = this.visitNode(node, path)
    block.statements = block.statements.map((s) =>
      this.descendIntoNode(s, [...path, block])
    )
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
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const else_ = this.visitNode(node, path)
    else_.body = this.descendIntoNode(else_.body, [...path, else_])
    return else_
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
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const functionCall = this.visitNode(node, path)
    functionCall.callee = this.descendIntoNode(functionCall.callee, [
      ...path,
      functionCall,
    ])
    functionCall.argumentList = this.descendIntoNode(
      functionCall.argumentList,
      [...path, functionCall]
    )
    return functionCall
  }

  visitFunctionExpression(
    node: TInputAstNode & { __type: FunctionExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const functionExpression = this.visitNode(node, path)
    functionExpression.parameterList = this.descendIntoNode(
      functionExpression.parameterList,
      [...path, functionExpression]
    )
    functionExpression.body = this.descendIntoNode(functionExpression.body, [
      ...path,
      functionExpression,
    ])
    return functionExpression
  }

  visitHTML(
    node: TInputAstNode & { __type: HTMLNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const html = this.visitNode(node, path)
    html.children = html.children.map((c) =>
      this.descendIntoNode(c, [...path, html])
    )
    return html
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
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const if_ = this.visitNode(node, path)
    if_.condition = this.descendIntoNode(if_.condition, [...path, node])
    if_.body = this.descendIntoNode(if_.body, [...path, node])
    return if_
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
    return this.visitNode(node, path)
  }

  visitMethodDefinition(
    node: TInputAstNode & { __type: MethodDefinitionNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const methodDefinition = this.visitNode(node, path)
    methodDefinition.name = this.descendIntoNode(methodDefinition.name, [
      ...path,
      node,
    ])
    methodDefinition.parameterList = this.descendIntoNode(
      methodDefinition.parameterList,
      [...path, node]
    )
    methodDefinition.returnType = methodDefinition.returnType
      ? this.descendIntoNode(methodDefinition.returnType, [...path, node])
      : null
    methodDefinition.body = this.descendIntoNode(methodDefinition.body, [
      ...path,
      node,
    ])
    return methodDefinition
  }

  visitNamedType(
    node: TInputAstNode & { __type: NamedTypeNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const namedType = this.visitNode(node, path)
    namedType.genericArguments = namedType.genericArguments.map((g) =>
      this.descendIntoNode(g, [...path, namedType])
    )
    return namedType
  }

  visitObjectLiteral(
    node: TInputAstNode & { __type: ObjectLiteralNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const objectLiteral = this.visitNode(node, path)
    objectLiteral.properties = objectLiteral.properties.map((prop) =>
      this.descendIntoNode(prop, [...path, objectLiteral])
    )
    return objectLiteral
  }

  visitObjectProperty(
    node: TInputAstNode & { __type: ObjectPropertyNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const objectProperty = this.visitNode(node, path)
    objectProperty.key = this.descendIntoNode(objectProperty.key, [
      ...path,
      objectProperty,
    ])
    objectProperty.value = this.descendIntoNode(objectProperty.value, [
      ...path,
      objectProperty,
    ])
    return objectProperty
  }

  visitObjectType(
    node: TInputAstNode & { __type: ObjectTypeNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const objectType = this.visitNode(node, path)
    objectType.properties = objectType.properties.map((p) =>
      this.descendIntoNode(p, [...path, objectType])
    )
    return objectType
  }

  visitParameter(
    node: TInputAstNode & { __type: ParameterNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const parameter = this.visitNode(node, path)
    parameter.name = this.descendIntoNode(parameter.name, [...path, parameter])
    parameter.type = parameter.type
      ? this.descendIntoNode(parameter.type, [...path, parameter])
      : null
    return parameter
  }

  visitParameterList(
    node: TInputAstNode & { __type: ParameterListNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const parameterList = this.visitNode(node, path)
    parameterList.parameters = parameterList.parameters.map((p) =>
      this.descendIntoNode(p, [...path, parameterList])
    )
    return parameterList
  }

  visitProgram(node: TInputAstNode & { __type: ProgramNode['__type'] }) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, [])
    }

    const program = this.visitNode(node, [])
    program.useStatements = program.useStatements.map((s) =>
      this.descendIntoNode(s, [program])
    )
    program.statements = program.statements.map((s) =>
      this.descendIntoNode(s, [program])
    )
    program.render = program.render
      ? this.descendIntoNode(program.render, [program]) ?? program.render
      : program.render

    return program
  }

  visitPropertyAccess(
    node: TInputAstNode & { __type: PropertyAccessNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const propertyAccess = this.visitNode(node, path)
    propertyAccess.left = this.descendIntoNode(propertyAccess.left, [
      ...path,
      propertyAccess,
    ])
    propertyAccess.rights = propertyAccess.rights.map((r) =>
      this.descendIntoNode(r, [...path, propertyAccess])
    )
    return node
  }

  visitPropertyKey(
    node: TInputAstNode & { __type: PropertyKeyNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const propertyKey = this.visitNode(node, path)
    propertyKey.value = this.descendIntoNode(propertyKey.value, [
      ...path,
      propertyKey,
    ])
    return propertyKey
  }

  visitRender(
    node: TInputAstNode & { __type: RenderNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const render = this.visitNode(node, path)
    render.body = this.descendIntoNode(render.body, [...path, render])
    return render
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
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const typeDef = this.visitNode(node, path)
    typeDef.name = this.descendIntoNode(typeDef.name, [...path, typeDef])
    typeDef.type = this.descendIntoNode(typeDef.type, [...path, typeDef])
    return typeDef
  }

  visitTypeProperty(
    node: TInputAstNode & { __type: TypePropertyNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const typeProperty = this.visitNode(node, path)
    typeProperty.name = this.descendIntoNode(typeProperty.name, [
      ...path,
      typeProperty,
    ])
    typeProperty.type = this.descendIntoNode(typeProperty.type, [
      ...path,
      typeProperty,
    ])
    return typeProperty
  }

  visitUnaryExpression(
    node: TInputAstNode & { __type: UnaryExpressionNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const unaryExpression = this.visitNode(node, path)
    unaryExpression.expression = this.descendIntoNode(
      unaryExpression.expression,
      [...path, unaryExpression]
    )
    return unaryExpression
  }

  visitUse(
    node: TInputAstNode & { __type: UseNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const useStatement = this.visitNode(node, path)
    useStatement.selectors = useStatement.selectors.map((s) =>
      this.descendIntoNode(s, [...path, useStatement])
    )
    return useStatement
  }

  visitUseSelector(
    node: TInputAstNode & { __type: UseSelectorNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const useSelector = this.visitNode(node, path)
    useSelector.name =
      typeof useSelector.name !== 'string'
        ? this.descendIntoNode(useSelector.name, [...path, useSelector])
        : useSelector.name
    useSelector.alias =
      useSelector.alias !== null
        ? this.descendIntoNode(useSelector.alias, [...path, useSelector])
        : useSelector.alias
    return useSelector
  }

  visitVariableAccess(
    node: TInputAstNode & { __type: VariableAccessNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const variableAccess = this.visitNode(node, path)
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

    const variableAttributeList = this.visitNode(node, path)
    variableAttributeList.attributes = variableAttributeList.attributes.map(
      (attr) => this.descendIntoNode(attr, [...path, variableAttributeList])
    )
    return variableAttributeList
  }

  visitVariableDeclaration(
    node: TInputAstNode & { __type: VariableDeclarationNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const variableDeclaration = this.visitNode(node, path)
    variableDeclaration.attributeLists = variableDeclaration.attributeLists.map(
      (a) => this.descendIntoNode(a, [...path, variableDeclaration])
    )
    variableDeclaration.identifier = this.descendIntoNode(
      variableDeclaration.identifier,
      [...path, node]
    )
    variableDeclaration.initializer = this.descendIntoNode(
      variableDeclaration.initializer,
      [...path, variableDeclaration]
    )
    return variableDeclaration
  }

  visitWhile(
    node: TInputAstNode & { __type: WhileNode['__type'] },
    path: TInputAstNode[]
  ) {
    if (this.hasCustomVisitor(node)) {
      return this.visitNode(node, path)
    }

    const while_ = this.visitNode(node, path)
    while_.condition = this.descendIntoNode(while_.condition, [...path, while_])
    while_.body = this.descendIntoNode(while_.body, [...path, while_])
    return while_
  }
}
