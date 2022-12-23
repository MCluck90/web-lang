import { error, lazy, list, maybe, pair, Parser, zeroOrMore } from 'parsnip-ts'
import { separatedFloatingPoint, separatedInteger } from 'parsnip-ts/numbers'
import { seq } from 'parsnip-ts/seq'
import { singleQuoteString, doubleQuoteString } from 'parsnip-ts/strings'
import { ws } from 'parsnip-ts/whitespace'
import {
  BinaryOperator,
  createArgumentListNode,
  createBinaryExpressionNode,
  createBlockNode,
  createElseNode,
  createFloatingPointNode,
  createFunctionCallNode,
  createFunctionExpressionNode,
  createHTMLNode,
  createIfNode,
  createIntegerNode,
  createObjectLiteralNode,
  createObjectPropertyNode,
  createParameterListNode,
  createParameterNode,
  createPropertyAccessNode,
  createStringNode,
  createUnaryExpression,
  createVariableAccessNode,
  createVariableDeclarationNode,
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
import { _jsAsm } from './js-asm'
import {
  _elseKeyword,
  _fnKeyword,
  _ifKeyword,
  _letKeyword,
  _mutKeyword,
} from './keywords'
import { _type } from './types'

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

export const _variableDeclaration = _letKeyword
  .and(
    seq([
      maybe(_mutKeyword),
      _identifier,
      maybe(_colon.and(_type)),
      token(/=/y),
      lazy(() => _expression),
    ])
  )
  .map(([mut, identifier, type, , initializer]) =>
    createVariableDeclarationNode(identifier, mut !== null, type, initializer)
  )

export const _statement = _variableDeclaration
  .or(lazy(() => _expression))
  .or(_jsAsm)
  .or(token(/;/y).map(() => null))

const _objectProperty = pair(
  _identifier,
  _colon.and(lazy(() => _expression))
).map(([key, value]) => createObjectPropertyNode(key, value))
const _objectLiteral = between(_braces, trailingCommaList(_objectProperty)).map(
  createObjectLiteralNode
)

export const _block = between(
  _braces,
  zeroOrMore(
    between(
      [ws, ws],
      lazy(() => _statement)
    )
  ).map((statements) =>
    statements.filter((s): s is Exclude<typeof s, null> => s !== null)
  )
).map(createBlockNode)

const _parameter = seq([_identifier, maybe(_colon.and(_type))]).map(
  ([name, type]) => createParameterNode(name, type)
)
const _parameterList = between(_parens, trailingCommaList(_parameter)).map(
  createParameterListNode
)
const _functionExpression = _fnKeyword
  .and(pair(_parameterList, _block))
  .map(([parameterList, body]) =>
    createFunctionExpressionNode(parameterList, body)
  )

const _ifExpression = _ifKeyword
  .and(
    seq([
      lazy(() => _expression),
      _block,
      maybe(_elseKeyword.and(_block).map(createElseNode)),
    ])
  )
  .map((args) => createIfNode(...args))

const _literalValue = _floatingPoint
  .or(_integer)
  .or(_string)
  .or(_functionExpression)
  .or(_jsAsm)
  .or(_objectLiteral)

const _primaryExpression = between(
  _parens,
  lazy(() => _expression)
)
  .or(_literalValue)
  .or(_ifExpression)
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
