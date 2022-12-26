import {
  AnonymousTypeNode,
  ArgumentListNode,
  AssignmentNode,
  ASTNode,
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
  UseNode,
  UseSelectorNode,
  VariableAccessNode,
  VariableAttributeListNode,
  VariableDeclarationNode,
  WhileNode,
} from '../../../parser/ast'
import {
  AstNodeWithEnvironment,
  BinaryExpressionNodeWithEnvironment,
  BooleanNodeWithEnvironment,
  EnvironmentType,
  FloatingPointNodeWithEnvironment,
  FunctionCallNodeWithEnvironment,
  FunctionExpressionNodeWithEnvironment,
  HTMLNodeWithEnvironment,
  IfNodeWithEnvironment,
  IntegerNodeWithEnvironment,
  JsAsmNodeWithEnvironment,
  ObjectLiteralNodeWithEnvironment,
  PropertyAccessNodeWithEnvironment,
  StringNodeWithEnvironment,
  UnaryExpressionNodeWithEnvironment,
  VariableAccessNodeWithEnvironment,
  VariableDeclarationNodeWithEnvironment,
} from './environment-ast'

class Scope {
  private parent: Scope | null
  private variables: Record<string, EnvironmentType | undefined>

  constructor(parent: Scope | null) {
    this.parent = parent
    this.variables = {}
  }

  setVariable(name: string, type: EnvironmentType) {
    this.variables[name] = type
  }

  getVariable(name: string): EnvironmentType | null {
    const type = this.variables[name] ?? null
    if (!type) {
      if (this.parent) {
        return this.parent.getVariable(name)
      }
      throw new Error(
        `Attempted to evaluate environment for undeclared variable \`${name}\``
      )
    }
    return type
  }
}

export const inferEnvironment = <T extends ASTNode>(
  node: T
): T & AstNodeWithEnvironment => {
  const scopeStack: Scope[] = [new Scope(null)]
  const currentScope = () => scopeStack[scopeStack.length - 1]

  const inferenceVisitor = {
    visitNode<T extends ASTNode>(node: T): T & AstNodeWithEnvironment {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      return (inferenceVisitor[`visit${node.__type}`] as any)(node as never)
    },
    visitAnonymousType(node: AnonymousTypeNode): AnonymousTypeNode {
      throw new Error('Not yet implemented')
    },
    visitArgumentList(node: ArgumentListNode): ArgumentListNode {
      throw new Error('Not yet implemented')
    },
    visitAssignment(node: AssignmentNode[]) {
      throw new Error('Not yet implemented')
    },
    visitBinaryExpression(
      node: BinaryExpressionNode
    ): BinaryExpressionNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitBlock(node: BlockNode): BlockNode {
      throw new Error('Not yet implemented')
    },
    visitBoolean(node: BooleanNode): BooleanNodeWithEnvironment {
      return { ...node, $environment: 'isomorphic' }
    },
    visitElse(node: ElseNode): ElseNode {
      throw new Error('Not yet implemented')
    },
    visitFloatingPoint(
      node: FloatingPointNode
    ): FloatingPointNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitFunctionCall(node: FunctionCallNode): FunctionCallNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitFunctionExpression(
      node: FunctionExpressionNode
    ): FunctionExpressionNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitHTML(node: HTMLNode): HTMLNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitIdentifier(node: IdentifierNode): IdentifierNode {
      throw new Error('Not yet implemented')
    },
    visitIf(node: IfNode): IfNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitInteger(node: IntegerNode): IntegerNodeWithEnvironment {
      return { ...node, $environment: 'isomorphic' }
    },
    visitJsAsm(node: JsAsmNode): JsAsmNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitMethodDefinition(node: MethodDefinitionNode): MethodDefinitionNode {
      throw new Error('Not yet implemented')
    },
    visitNamedType(node: NamedTypeNode): NamedTypeNode {
      throw new Error('Not yet implemented')
    },
    visitObjectLiteral(
      node: ObjectLiteralNode
    ): ObjectLiteralNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitObjectProperty(node: ObjectPropertyNode): ObjectPropertyNode {
      throw new Error('Not yet implemented')
    },
    visitObjectType(node: ObjectTypeNode): ObjectTypeNode {
      throw new Error('Not yet implemented')
    },
    visitParameter(node: ParameterNode): ParameterNode {
      throw new Error('Not yet implemented')
    },
    visitParameterList(node: ParameterListNode): ParameterListNode {
      throw new Error('Not yet implemented')
    },
    visitProgram(node: ProgramNode): ProgramNode {
      node.useStatements = node.useStatements.map(inferenceVisitor.visitNode)
      node.statements = node.statements.map(inferenceVisitor.visitNode)
      node.render = node.render ? inferenceVisitor.visitNode(node.render) : null
      return node
    },
    visitPropertyAccess(
      node: PropertyAccessNode
    ): PropertyAccessNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitPropertyKey(node: PropertyKeyNode): PropertyKeyNode {
      throw new Error('Not yet implemented')
    },
    visitRender(node: RenderNode): RenderNode {
      throw new Error('Not yet implemented')
    },
    visitString(node: StringNode): StringNodeWithEnvironment {
      return { ...node, $environment: 'isomorphic' }
    },
    visitTypeDefinition(node: TypeDefinitionNode): TypeDefinitionNode {
      throw new Error('Not yet implemented')
    },
    visitTypeProperty(node: TypePropertyNode): TypePropertyNode {
      throw new Error('Not yet implemented')
    },
    visitUnaryExpression(
      node: UnaryExpressionNode
    ): UnaryExpressionNodeWithEnvironment {
      throw new Error('Not yet implemented')
    },
    visitUse(node: UseNode): UseNode {
      throw new Error('Not yet implemented')
    },
    visitUseSelector(node: UseSelectorNode): UseSelectorNode {
      throw new Error('Not yet implemented')
    },
    visitVariableAccess(
      node: VariableAccessNode
    ): VariableAccessNodeWithEnvironment {
      return {
        ...node,
        $environment: currentScope().getVariable(node.name.value) ?? 'unknown',
      }
    },
    visitVariableAttributeList(
      node: VariableAttributeListNode
    ): VariableAttributeListNode {
      throw new Error('Not yet implemented')
    },
    visitVariableDeclaration(
      node: VariableDeclarationNode
    ): VariableDeclarationNodeWithEnvironment {
      const initializer = inferenceVisitor.visitNode(node.initializer)
      const isMarkedAsFrontend = node.attributeLists.find((attr) =>
        attr.attributes.map((i) => i.value).includes('Frontend')
      )
      const isMarkedAsBackend = node.attributeLists.find((attr) =>
        attr.attributes.map((i) => i.value).includes('Backend')
      )
      const isMarkedAsIsomorphic = isMarkedAsFrontend && isMarkedAsBackend
      const markedEnvironment: EnvironmentType = isMarkedAsIsomorphic
        ? 'isomorphic'
        : isMarkedAsFrontend
        ? 'frontend'
        : isMarkedAsBackend
        ? 'backend'
        : 'unknown'
      const initializerEnvironment = initializer.$environment
      let foundEnvironment: EnvironmentType = 'unknown'
      if (markedEnvironment === 'unknown') {
        foundEnvironment = initializerEnvironment
      } else if (
        initializerEnvironment === 'isomorphic' ||
        initializerEnvironment === 'unknown'
      ) {
        foundEnvironment = markedEnvironment
      } else if (initializerEnvironment !== markedEnvironment) {
        throw new Error(
          `Environment conflict. Variable declared as ${markedEnvironment} but initializer has environment of ${initializerEnvironment}`
        )
      } else {
        foundEnvironment = initializerEnvironment
      }
      currentScope().setVariable(node.identifier.value, foundEnvironment)
      return { ...node, $environment: foundEnvironment }
    },
    visitWhile(node: WhileNode): WhileNode {
      throw new Error('Not yet implemented')
    },
  } satisfies Record<`visit${ASTNode['__type']}` | 'visitNode', unknown>
  return inferenceVisitor.visitNode(node)
}
