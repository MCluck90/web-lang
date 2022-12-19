import { error, list, maybe, pair, Parser, zeroOrMore } from 'parsnip-ts'
import { separatedFloatingPoint, separatedInteger } from 'parsnip-ts/numbers'
import { seq } from 'parsnip-ts/seq'
import {
  BinaryOperator,
  createBinaryExpressionNode,
  createFloatingPointNode,
  createIntegerNode,
  createPropertyAccessNode,
  createUnaryExpression,
  createVariableAccessNode,
  ExpressionNode,
  PropertyAccessNode,
} from './ast'
import { token } from './combinators'
import { _identifier } from './common'

export let _expression: Parser<ExpressionNode> = error('Not yet implemented')

const _additionOperator = token(/\+/y) as Parser<'+'>
const _subtractionOperator = token(/\-/y) as Parser<'-'>
const _multiplicationOperator = token(/\*/y) as Parser<'*'>
const _divisionOperator = token(/\//y) as Parser<'/'>
const _propertyAccessOperator = token(/\./y) as Parser<'.'>
const _integer = separatedInteger.map(createIntegerNode)
const _floatingPoint = separatedFloatingPoint.map(createFloatingPointNode)

const foldBinaryExpression = ([left, rights]: [
  ExpressionNode,
  [BinaryOperator, ExpressionNode][]
]): ExpressionNode =>
  rights.reduce(
    (l, [op, r]) => createBinaryExpressionNode(l, op, r),
    left as ExpressionNode
  )

const _literalValue = _floatingPoint.or(_integer)

const _primaryExpression = _literalValue.or(
  _identifier.map(createVariableAccessNode)
)

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

const _unary = seq([maybe(_subtractionOperator), _propertyAccess]).map(
  ([op, expression]) =>
    op !== null ? createUnaryExpression(op, expression) : expression
)

const _factor = seq([
  _propertyAccess.or(_unary),
  zeroOrMore(pair(_multiplicationOperator.or(_divisionOperator), _unary)),
]).map(foldBinaryExpression)

const _term = seq([
  _factor,
  zeroOrMore(pair(_additionOperator.or(_subtractionOperator), _factor)),
]).map(foldBinaryExpression)

_expression = _term