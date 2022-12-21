import { isNodeType } from '../../parser/ast'
import { HTMLModule } from '../index.types'
import { Input, Output } from './render-static-entry-html.types'

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

  const mainExpressions = renderMethod.body.statements
  const lastExpression = mainExpressions[mainExpressions.length - 1]
  if (!isNodeType('String')(lastExpression)) {
    console.info(
      'Pass: render-static-entry-html:',
      'Only string literals are implemented'
    )
    return null
  }

  return new HTMLModule('index', generateHtmlDocument(lastExpression.value))
}
