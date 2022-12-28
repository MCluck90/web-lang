import { AstNode } from '../../parser/ast'
import { AstReducer } from '../../utils/ast-visitor'
import { JSModule } from '../index.types'
import {
  AstNodeWithEnvironment,
  EnvironmentType,
  isAnEnvironmentNode,
} from './environment-inference/environment-ast'

const buildPath = <T, U>(node: T, path: U[]) => [...path, node]

export const emitJs = (
  node: AstNodeWithEnvironment
): {
  backend: JSModule | null
  frontend: JSModule | null
  isomorphic: JSModule | null
} => {
  const contents: Record<EnvironmentType, string> = {
    backend: '',
    frontend: '',
    isomorphic: '',
    unknown: '',
  }
  const environmentStack: EnvironmentType[] = ['isomorphic']
  const getEnv = () => environmentStack[environmentStack.length - 1]
  const appendToEnv = (source: string) => {
    contents[getEnv()] += source
  }

  const visitor: AstReducer<string, AstNodeWithEnvironment> = {
    visitNode<T extends AstNode>(node: T, path: AstNode[]) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      return (visitor[`visit${node.__type}`] as any)(node as never, path)
    },
    visitProgram(node) {
      // TODO: Handle `render` section
      const useStatements = node.useStatements.reduce(
        (acc, statement) => acc + `${visitor.visitNode(statement, [node])};\n`,
        ''
      )
      return (
        useStatements +
        node.statements.reduce(
          (acc, statement) =>
            acc + `${visitor.visitNode(statement, [node])};\n`,
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
        return `{${node.statements
          .map((n) => `${this.visitNode(n, buildPath(node, path))};`)
          .join('')}}`
      }

      const nonReturnedStatements = node.statements.slice(
        0,
        node.statements.length - 1
      )
      const lastStatement = node.statements[node.statements.length - 1]
      return `{\n${nonReturnedStatements
        .map((n) => `${this.visitNode(n, buildPath(node, path))}`)
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
      if (isAnEnvironmentNode(node) && node.$environment !== 'unknown') {
        environmentStack.push(node.$environment)
      }
      const contents = `let ${this.visitNode(
        node.identifier,
        buildPath(node, path)
      )} = ${this.visitNode(node.initializer, path)};\n`
      if (isAnEnvironmentNode(node) && node.$environment !== 'unknown') {
        appendToEnv(contents)

        if (node.$environment === 'backend') {
          appendToEnv(`app.post('/${node.identifier.value}', (req, res) => {
            console.log(req.body)
            res.send(${node.identifier.value}(...req.body))
          })`)

          environmentStack.push('frontend')
          appendToEnv(
            `let ${node.identifier.value} = (...args) => fetch('/${node.identifier.value}', {method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(args)});\n`
          )
          environmentStack.pop()
        }
      }
      if (isAnEnvironmentNode(node) && node.$environment !== 'unknown') {
        environmentStack.pop()
      }
      return contents
    },
    visitUse(node, path) {
      const selectors = `{ ${node.selectors
        .map((s) => this.visitNode(s, buildPath(node, path)))
        .join(',\n')} }`
      switch (node.type) {
        case 'Absolute':
          throw new Error('Absolute importing is not yet implemented')

        case 'Relative':
          return `import ${selectors} from './${node.path.replace(
            /'/g,
            "\\'"
          )}'`

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
  visitor.visitNode(node, [])

  if (contents.unknown) {
    throw new Error(`Unknown contents: ${contents.unknown}`)
  }

  if (contents.frontend) {
    contents.frontend += `\n\nif (frontend_main) { frontend_main() }`
  }
  if (contents.backend) {
    contents.backend = `
import express from 'express'
import path from 'path'
import { fileURLToPath } from 'url'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const app = express()
app.use(express.json())
app.use(express.static(path.join(__dirname, 'public')))
app.use(
  'frontend-js',
  express.static(path.join(__dirname, 'public', 'frontend-js'))
)
app.use(
  'isomorphic-js',
  express.static(path.join(__dirname, 'public', 'isomorphic-js'))
)

${contents.backend}

app.listen(3000, () => {
  console.log('Server listening on http://localhost:3000')
})
`
  }

  return {
    backend: contents.backend ? new JSModule('main', contents.backend) : null,
    frontend: contents.frontend
      ? new JSModule('main', contents.frontend)
      : null,
    isomorphic: contents.isomorphic
      ? new JSModule('main', contents.isomorphic)
      : null,
  }
}
