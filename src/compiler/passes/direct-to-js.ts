import {
  AnonymousTypeNode,
  ArgumentListNode,
  ASTNode,
  BinaryExpressionNode,
  BlockNode,
  FloatingPointNode,
  FunctionCallNode,
  FunctionExpressionNode,
  HTMLNode,
  IdentifierNode,
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
} from '../../parser/ast'
import { AstMapper } from '../../utils/ast-visitor'
import { JSModule } from '../index.types'

const buildJsVisitor: AstMapper<string> = {
  visitNode<T extends ASTNode>(node: T, path: ASTNode[]) {
    return (buildJsVisitor[`visit${node.__type}`] as any)(node as never, path)
  },
  visitProgram(node: ProgramNode) {
    // TODO: Handle `render` section
    return node.statements.reduce(
      (acc, statement) => acc + buildJsVisitor.visitNode(statement, [node]),
      ''
    )
  },
  visitAnonymousType(node: AnonymousTypeNode, path: ASTNode[]) {
    // Types do not emit code
    return ''
  },
  visitArgumentList(node: ArgumentListNode, path: ASTNode[]) {
    return `(${node.arguments.map((n) => this.visitNode(n, path)).join(', ')})`
  },
  visitBlock(node: BlockNode, path: ASTNode[]) {
    if (node.statements.length === 0) {
      return ''
    }

    const nonReturnedStatements = node.statements.slice(
      0,
      node.statements.length - 1
    )
    const lastStatement = node.statements[node.statements.length - 1]
    return `{\n${nonReturnedStatements
      .map((n) => `${this.visitNode(n, path)};`)
      .join('\n')}\nreturn ${this.visitNode(lastStatement, path)};\n}`
  },
  visitBinaryExpression(node: BinaryExpressionNode, path: ASTNode[]) {
    return `${this.visitNode(node.left, path)} ${
      node.operator
    } ${this.visitNode(node.right, path)}`
  },
  visitElse(node, path) {
    return `else ${this.visitNode(node.body, path)}`
  },
  visitFloatingPoint(node: FloatingPointNode, path: ASTNode[]) {
    return node.value.toString()
  },
  visitFunctionCall(node: FunctionCallNode, path: ASTNode[]) {
    return `${this.visitNode(node.callee, path)}${this.visitNode(
      node.argumentList,
      path
    )}`
  },
  visitFunctionExpression(node: FunctionExpressionNode, path: ASTNode[]) {
    return `${this.visitNode(node.parameterList, path)} => ${this.visitNode(
      node.body,
      path
    )}`
  },
  visitHTML(node: HTMLNode, path: ASTNode[]) {
    throw new Error('HTML not yet implemented.')
  },
  visitIf(node, path) {
    return `(function() {if (${this.visitNode(
      node.condition,
      path
    )}) ${this.visitNode(node.body, path)} ${
      node.else_ ? this.visitNode(node.else_, path) : ''
    }})();`
  },
  visitInteger(node: IntegerNode, path: ASTNode[]) {
    return node.value.toString()
  },
  visitJsAsm(node: JsAsmNode, path: ASTNode[]) {
    return node.code
  },
  visitObjectLiteral(node: ObjectLiteralNode, path: ASTNode[]) {
    return `{ ${node.properties
      .map((n) => this.visitNode(n, path))
      .join(', ')} }`
  },
  visitPropertyAccess(node: PropertyAccessNode, path: ASTNode[]) {
    return `${this.visitNode(node.left, path)}.${node.rights
      .map((n) => this.visitNode(n, path))
      .join(',')}`
  },
  visitString(node: StringNode, path: ASTNode[]) {
    return `"${node.value.replace(/"/g, '\\"')}"`
  },
  visitUnaryExpression(node: UnaryExpressionNode, path: ASTNode[]) {
    return `${node.operator}${this.visitNode(node.expression, path)}`
  },
  visitVariableAccess(node: VariableAccessNode, path: ASTNode[]) {
    return this.visitNode(node.name, path)
  },
  visitIdentifier(node: IdentifierNode, path: ASTNode[]) {
    return node.value
  },
  visitMethodDefinition(node: MethodDefinitionNode, path: ASTNode[]) {
    return `${node.name}${this.visitNode(
      node.parameterList,
      path
    )} ${this.visitNode(node.body, path)}`
  },
  visitNamedType(node: NamedTypeNode, path: ASTNode[]) {
    // Types are not emitted
    return ''
  },
  visitObjectProperty(node: ObjectPropertyNode, path: ASTNode[]) {
    return `${this.visitNode(node.key, path)}: ${this.visitNode(
      node.value,
      path
    )},`
  },
  visitObjectType(node: ObjectTypeNode, path: ASTNode[]) {
    // Types are not emitted
    return ''
  },
  visitParameterList(node: ParameterListNode, path: ASTNode[]) {
    return `(${node.parameters.map((n) => this.visitNode(n, path)).join(', ')})`
  },
  visitParameter(node: ParameterNode, path: ASTNode[]) {
    return this.visitNode(node.name, path)
  },
  visitPropertyKey(node: PropertyKeyNode, path: ASTNode[]) {
    return this.visitNode(node.value, path)
  },
  visitRender(node: RenderNode, path: ASTNode[]) {
    throw new Error('Render not yet implemented.')
  },
  visitRemoteDefinition(node: RemoteDefinitionNode, path: ASTNode[]) {
    throw new Error('RemoteDefinition not yet implemented.')
  },
  visitRemoteParameter(node: RemoteParameterNode, path: ASTNode[]) {
    throw new Error('RemoteParameter not yet implemented.')
  },
  visitRemoteUrl(node: RemoteUrlNode, path: ASTNode[]) {
    throw new Error('RemoteUrl not yet implemented.')
  },
  visitTypeDefinition(node: TypeDefinitionNode, path: ASTNode[]) {
    // Types are not emitted
    return ''
  },
  visitTypeProperty(node: TypePropertyNode, path: ASTNode[]) {
    // Types are not emitted
    return ''
  },
  visitVariableDeclaration(node: VariableDeclarationNode, path: ASTNode[]) {
    return `let ${this.visitNode(node.identifier, path)} = ${this.visitNode(
      node.initializer,
      path
    )};`
  },
}

export const directToJs = (program: ProgramNode): JSModule | null => {
  const contents = buildJsVisitor.visitProgram(program)
  if (typeof contents !== 'string' || contents.length === 0) {
    return null
  }
  return new JSModule('main', contents)
}
