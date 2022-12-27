import { AstNode } from '../../../parser/ast'
import { CustomOrderVisitor } from '../../../utils/ast-visitor'
import { AstNodeWithEnvironment, EnvironmentType } from './environment-ast'

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

export const inferEnvironment = <T extends AstNode>(
  node: T
): AstNodeWithEnvironment & { __type: T['__type'] } => {
  const scopeStack: Scope[] = [new Scope(null)]
  const currentScope = () => scopeStack[scopeStack.length - 1]

  const visitor: CustomOrderVisitor<
    AstNodeWithEnvironment | (AstNode & { $environment?: EnvironmentType })
  > = new CustomOrderVisitor<AstNodeWithEnvironment>({
    visitBoolean(node) {
      return { ...node, $environment: 'isomorphic' }
    },
    visitInteger(node) {
      return { ...node, $environment: 'isomorphic' }
    },
    visitString(node) {
      return { ...node, $environment: 'isomorphic' }
    },
    visitVariableDeclaration(node, path) {
      const initializer =
        visitor.descendIntoNode(node.initializer, [...path, node]) ??
        node.initializer
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
    visitVariableAccess(node) {
      return {
        ...node,
        $environment: currentScope().getVariable(node.name.value) ?? 'unknown',
      }
    },
  })
  return visitor.descendIntoNode(node, []) as T & AstNodeWithEnvironment
}
