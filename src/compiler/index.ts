import { isStringNode, ProgramNode } from '../parser/ast'

export const compileProgram = (program: ProgramNode) => {
  const renderMethod = program.main.methods.find(
    (method) => method.name.value === 'render'
  )
  if (!renderMethod) {
    throw new Error('`main` must have a render method')
  }

  const mainExpressions = renderMethod.body.expressions
  const lastExpression = mainExpressions[mainExpressions.length - 1]
  if (!isStringNode(lastExpression)) {
    throw new Error('Only string literals are implemented')
  }
  return {
    html: `
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
  ${lastExpression.value}
</body>
</html>
    `.trim(),
  }
}
