import { error, lazy, list, maybe, pair, Parser, zeroOrMore } from 'parsnip-ts'
import { separatedFloatingPoint, separatedInteger } from 'parsnip-ts/numbers'
import { seq } from 'parsnip-ts/seq'
import { singleQuoteString, doubleQuoteString } from 'parsnip-ts/strings'
import {
  BinaryOperator,
  createArgumentListNode,
  createBinaryExpressionNode,
  createFloatingPointNode,
  createFunctionCallNode,
  createHTMLNode,
  createIntegerNode,
  createObjectLiteralNode,
  createObjectPropertyNode,
  createPropertyAccessNode,
  createStringNode,
  createUnaryExpression,
  createVariableAccessNode,
  ExpressionNode,
} from './ast'
import {
  between,
  token,
  trailingCommaList,
  _braces,
  _colon,
  _parens,
} from './common'
import { _identifier } from './identifier'

export let _expression: Parser<ExpressionNode> = error('Not yet implemented')

const _additionOperator = token(/\+/y) as Parser<'+'>
const _subtractionOperator = token(/\-/y) as Parser<'-'>
const _multiplicationOperator = token(/\*/y) as Parser<'*'>
const _divisionOperator = token(/\//y) as Parser<'/'>
const _propertyAccessOperator = token(/\./y) as Parser<'.'>
const _integer = separatedInteger.map(createIntegerNode)
const _floatingPoint = separatedFloatingPoint.map(createFloatingPointNode)
const _string = singleQuoteString.or(doubleQuoteString).map(createStringNode)
const _html = seq([
  token(/[a-z][a-z0-9\-]*/y),
  token(/#/y),
  between(_parens, trailingCommaList(lazy(() => _expression))),
]).map(([tag, _, children]) => createHTMLNode(tag, children))

const foldBinaryExpression = ([left, rights]: [
  ExpressionNode,
  [BinaryOperator, ExpressionNode][]
]): ExpressionNode =>
  rights.reduce(
    (l, [op, r]) => createBinaryExpressionNode(l, op, r),
    left as ExpressionNode
  )

const _objectProperty = pair(
  _identifier,
  _colon.and(lazy(() => _expression))
).map(([key, value]) => createObjectPropertyNode(key, value))
const _objectLiteral = between(_braces, trailingCommaList(_objectProperty)).map(
  createObjectLiteralNode
)

const _literalValue = _floatingPoint.or(_integer).or(_string).or(_objectLiteral)

const _primaryExpression = between(
  _parens,
  lazy(() => _expression)
)
  .or(_literalValue)
  .or(_html)
  .or(_identifier.map(createVariableAccessNode))

const _propertyAccess = seq([
  _primaryExpression,
  maybe(
    _propertyAccessOperator.and(list(_identifier, _propertyAccessOperator))
  ),
]).map(([primary, maybeAccessChain]) =>
  maybeAccessChain === null
    ? primary
    : createPropertyAccessNode(primary, maybeAccessChain)
)

const _functionCall = seq([
  _propertyAccess,
  maybe(between(_parens, trailingCommaList(lazy(() => _expression)))),
]).map(([left, right]) =>
  right === null
    ? left
    : createFunctionCallNode(left, createArgumentListNode(right))
)

const _unary = seq([maybe(_subtractionOperator), _functionCall]).map(
  ([op, expression]) =>
    op !== null ? createUnaryExpression(op, expression) : expression
)

const _factor = seq([
  _functionCall.or(_unary),
  zeroOrMore(pair(_multiplicationOperator.or(_divisionOperator), _unary)),
]).map(foldBinaryExpression)

const _term = seq([
  _factor,
  zeroOrMore(pair(_additionOperator.or(_subtractionOperator), _factor)),
]).map(foldBinaryExpression)

_expression = _term
