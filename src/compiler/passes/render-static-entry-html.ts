import {
  ExpressionNode,
  isAnExpressionNode,
  isNodeType,
} from '../../parser/ast'
import { HTMLModule } from '../index.types'
import { Input, Output } from './render-static-entry-html.types'

const toStaticHtml = (expression: ExpressionNode): string => {
  switch (expression.__type) {
    case 'FloatingPoint':
    case 'Integer':
    case 'String':
      return expression.value.toString()

    case 'HTML':
      return `<${expression.tag}>${expression.children
        .map(toStaticHtml)
        .join('')}</${expression.tag}>`
  }

  throw new Error(`Unhandled expression type: ${expression.__type}`)
}

const generateHtmlDocument = (body: string) =>
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
</body>
</html>
    `.trim()

export const renderStaticEntryHtmlPass = (program: Input): Output => {
  const renderMethod = program.main.methods.find(
    (method) => method.name.value === 'render'
  )
  if (!renderMethod) {
    console.info(
      'Pass: render-static-entry-html:',
      '`main` must have a render method'
    )
    return null
  }

  const statements = renderMethod.body.statements
  if (statements.length === 0) {
    return new HTMLModule('index', '')
  }

  const lastStatement = statements[statements.length - 1]
  if (!isAnExpressionNode(lastStatement)) {
    return new HTMLModule('index', '')
  }

  console.log(lastStatement)
  return new HTMLModule(
    'index',
    generateHtmlDocument(toStaticHtml(lastStatement))
  )
}
