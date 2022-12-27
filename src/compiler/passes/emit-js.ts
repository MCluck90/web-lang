import { AstNode } from '../../parser/ast'
import { AstReducer } from '../../utils/ast-visitor'
import { JSModule } from '../index.types'
import { AstNodeWithEnvironment } from './environment-inference/environment-ast'
import { TypedAstNode } from './type-inference/typed-ast'

const buildPath = (node: AstNode, path: AstNode[]) => [...path, node]

type FinalAst = TypedAstNode & AstNodeWithEnvironment

const jsEmitter: AstReducer<string> = {
  visitNode<T extends AstNode>(node: T, path: AstNode[]) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    return (jsEmitter[`visit${node.__type}`] as any)(node as never, path)
  },
  visitProgram(node) {
    // TODO: Handle `render` section
    const useStatements = node.useStatements.reduce(
      (acc, statement) => acc + `${jsEmitter.visitNode(statement, [node])};\n`,
      ''
    )
    return (
      useStatements +
      node.statements.reduce(
        (acc, statement) =>
          acc + `${jsEmitter.visitNode(statement, [node])};\n`,
        ''
      )
    )
  },
  visitAssignment(node, path) {
    return `${this.visitNode(
      node.left,
      buildPath(node, path)
    )} = ${this.visitNode(node.right, path)}`
  },
  visitAnonymousType(_node) {
    // Types do not emit code
    return ''
  },
  visitArgumentList(node, path) {
    return `(${node.arguments
      .map((n) => this.visitNode(n, buildPath(node, path)))
      .join(', ')})`
  },
  visitBlock(node, path) {
    if (node.statements.length === 0) {
      return ''
    }

    if (path[path.length - 1].__type === 'While') {
      return `{\n${node.statements
        .map((n) => `${this.visitNode(n, buildPath(node, path))};`)
        .join('\n')}\n}`
    }

    const nonReturnedStatements = node.statements.slice(
      0,
      node.statements.length - 1
    )
    const lastStatement = node.statements[node.statements.length - 1]
    return `{\n${nonReturnedStatements
      .map((n) => `${this.visitNode(n, buildPath(node, path))};`)
      .join('\n')}\nreturn ${this.visitNode(
      lastStatement,
      buildPath(node, path)
    )}\n}`
  },
  visitBinaryExpression(node, path) {
    return `${this.visitNode(node.left, buildPath(node, path))} ${
      node.operator === '=='
        ? '==='
        : node.operator === '!='
        ? '!=='
        : node.operator
    } ${this.visitNode(node.right, buildPath(node, path))}`
  },
  visitBoolean(node, path) {
    return ` ${node.value} `
  },
  visitElse(node, path) {
    return `else ${this.visitNode(node.body, buildPath(node, path))}`
  },
  visitFloatingPoint(node) {
    return node.value.toString()
  },
  visitFunctionCall(node, path) {
    return `${this.visitNode(
      node.callee,
      buildPath(node, path)
    )}${this.visitNode(node.argumentList, path)}`
  },
  visitFunctionExpression(node, path) {
    return `${this.visitNode(
      node.parameterList,
      buildPath(node, path)
    )} => ${this.visitNode(node.body, path)}`
  },
  visitHTML() {
    throw new Error('HTML not yet implemented.')
  },
  visitIf(node, path) {
    return `(() => {if (${this.visitNode(
      node.condition,
      path
    )}) ${this.visitNode(node.body, buildPath(node, path))} ${
      node.else_ ? this.visitNode(node.else_, path) : ''
    }})();`
  },
  visitInteger(node) {
    return node.value.toString()
  },
  visitJsAsm(node) {
    return node.code
  },
  visitObjectLiteral(node, path) {
    return `{ ${node.properties
      .map((n) => this.visitNode(n, buildPath(node, path)))
      .join(', ')} }`
  },
  visitPropertyAccess(node, path) {
    return `${this.visitNode(node.left, buildPath(node, path))}.${node.rights
      .map((n) => this.visitNode(n, buildPath(node, path)))
      .join(',')}`
  },
  visitString(node) {
    const result = node.value
      .replace(/"/g, '\\"')
      .replace(/\r/g, '\\r')
      .replace(/\n/g, '\\n')
      .replace(/\t/g, '\\t')
      .replace(/\0/g, '\\0')
    return `"${result}"`
  },
  visitUnaryExpression(node, path) {
    return `${node.operator}${this.visitNode(
      node.expression,
      buildPath(node, path)
    )}`
  },
  visitVariableAccess(node, path) {
    return this.visitNode(node.name, buildPath(node, path))
  },
  visitVariableAttributeList() {
    // Attributes are not emitted
    return ''
  },
  visitIdentifier(node) {
    return node.value
  },
  visitMethodDefinition(node, path) {
    return `${node.name}${this.visitNode(
      node.parameterList,
      path
    )} ${this.visitNode(node.body, buildPath(node, path))}`
  },
  visitNamedType() {
    // Types are not emitted
    return ''
  },
  visitObjectProperty(node, path) {
    return `${this.visitNode(
      node.key,
      buildPath(node, path)
    )}: ${this.visitNode(node.value, path)},`
  },
  visitObjectType() {
    // Types are not emitted
    return ''
  },
  visitParameterList(node, path) {
    return `(${node.parameters
      .map((n) => this.visitNode(n, buildPath(node, path)))
      .join(', ')})`
  },
  visitParameter(node, path) {
    return this.visitNode(node.name, buildPath(node, path))
  },
  visitPropertyKey(node, path) {
    return this.visitNode(node.value, buildPath(node, path))
  },
  visitRender() {
    throw new Error('Render not yet implemented.')
  },
  visitTypeDefinition() {
    // Types are not emitted
    return ''
  },
  visitTypeProperty() {
    // Types are not emitted
    return ''
  },
  visitVariableDeclaration(node, path) {
    return `let ${this.visitNode(
      node.identifier,
      buildPath(node, path)
    )} = ${this.visitNode(node.initializer, path)}`
  },
  visitUse(node, path) {
    const selectors = `{ ${node.selectors
      .map((s) => this.visitNode(s, buildPath(node, path)))
      .join(',\n')} }`
    switch (node.type) {
      case 'Absolute':
        throw new Error('Absolute importing is not yet implemented')

      case 'Relative':
        return `import ${selectors} from './${node.path.replace(/'/g, "\\'")}'`

      case 'Package':
        if (node.scope === 'std') {
          return `import ${selectors} from '../../std/${node.package}${
            node.path.length > 1 ? node.path : '/index.mjs'
          }'`
        } else {
          return `import ${selectors} from '${node.scope}/${node.package}${
            node.path.length > 1 ? node.path.replace(/'/g, "\\'") : ''
          }'`
        }
    }
  },
  visitUseSelector(node, path) {
    const alias = node.alias
      ? this.visitNode(node.alias, buildPath(node, path))
      : null
    if (alias) {
      return `${node.name} as ${alias}`
    }
    if (typeof node.name === 'string') {
      return node.name
    }
    return this.visitNode(node.name, buildPath(node, path))
  },
  visitWhile(node, path) {
    return `while (${this.visitNode(
      node.condition,
      buildPath(node, path)
    )}) ${this.visitNode(node.body, buildPath(node, path))}\n`
  },
}

export const emitJs = (node: FinalAst): JSModule | null => {
  // This is safe. TypeScript just can't prove it.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const contents: string = (jsEmitter[`visit${node.__type}`] as any)(
    node as never,
    []
  )
  if (typeof contents !== 'string' || contents.length === 0) {
    return null
  }
  return new JSModule('main', contents)
}
