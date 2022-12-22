import {
  ExpressionNode,
  isAnExpressionNode,
  isNodeType,
} from '../../parser/ast'
import { HTMLModule, JSModule } from '../index.types'
import { Input, Output } from './render-static-entry-html.types'

const toStaticHtml = (expression: ExpressionNode): string => {
  switch (expression.__type) {
    case 'FloatingPoint':
    case 'Integer':
    case 'String':
      return expression.value.toString()

    case 'HTML': {
      let html = `<${expression.tag}`
      if (expression.children.length > 0) {
        const firstExpression = expression.children[0]
        if (isNodeType('ObjectLiteral')(firstExpression)) {
          html +=
            firstExpression.properties
              .map((prop) => ` ${prop.key.value}="${toStaticHtml(prop.value)}"`)
              .join('') + '>'
          html += expression.children.slice(1).map(toStaticHtml).join('')
        } else {
          html += '>'
          html += expression.children.map(toStaticHtml).join('')
        }
      }
      return `${html}</${expression.tag}>`
    }
  }

  throw new Error(`Unhandled expression type: ${expression.__type}`)
}

const generateHtmlDocument = (body: string, jsModules: JSModule[]) =>
  `
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
  ${body}
  ${jsModules.map(
    (mod) => `<script src="frontend-js/${mod.moduleName}.js"></script>`
  )}
</body>
</html>
    `.trim()

export const renderStaticEntryHtmlPass = ({
  program,
  startupJsModules: jsModules,
}: Input): Output => {
  const renderBlock = program.render
  if (renderBlock) {
    const statements = renderBlock.body.statements
    if (statements.length === 0) {
      return new HTMLModule('index', '')
    }

    const lastStatement = statements[statements.length - 1]
    if (!isAnExpressionNode(lastStatement)) {
      return new HTMLModule('index', generateHtmlDocument('', jsModules))
    }

    return new HTMLModule(
      'index',
      generateHtmlDocument(toStaticHtml(lastStatement), jsModules)
    )
  }

  return new HTMLModule('index', generateHtmlDocument('', jsModules))
}
