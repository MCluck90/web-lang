import {
  constant,
  error,
  lazy,
  list,
  maybe,
  pair,
  Parser,
  zeroOrMore,
} from 'parsnip-ts'
import { separatedFloatingPoint, separatedInteger } from 'parsnip-ts/numbers'
import { seq } from 'parsnip-ts/seq'
import { singleQuoteString, doubleQuoteString } from 'parsnip-ts/strings'
import { ws } from 'parsnip-ts/whitespace'
import {
  BinaryOperator,
  BlockNode,
  createArgumentListNode,
  createAssignmentNode,
  createBinaryExpressionNode,
  createBlockNode,
  createBooleanNode,
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
  createUnaryExpressionNode,
  createVariableAccessNode,
  createVariableDeclarationNode,
  createWhileNode,
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
  _falseKeyword,
  _fnKeyword,
  _ifKeyword,
  _letKeyword,
  _mutKeyword,
  _trueKeyword,
  _whileKeyword,
} from './keywords'
import { _type } from './types'

export let _expression: Parser<ExpressionNode> = error('Not yet implemented')

const _additionOperator = token(/\+/y) as Parser<'+'>
const _subtractionOperator = token(/-/y) as Parser<'-'>
const _multiplicationOperator = token(/\*/y) as Parser<'*'>
const _divisionOperator = token(/\//y) as Parser<'/'>
const _propertyAccessOperator = token(/\./y) as Parser<'.'>
const _equalityOperator = token(/==/y) as Parser<'=='>
const _lessThanOperator = token(/</y) as Parser<'<'>
const _lessThanOrEqualOperator = token(/<=/y) as Parser<'<='>
const _greaterThanOperator = token(/>/y) as Parser<'>'>
const _greaterThanOrEqualOperator = token(/>=/y) as Parser<'>='>
const _notEqualOperator = token(/!=/y) as Parser<'!='>
const _assignmentOperator = token(/=/y) as Parser<'='>
const _notOperator = token(/!/y) as Parser<'!'>

const _integer = separatedInteger.map(createIntegerNode)
const _floatingPoint = separatedFloatingPoint.map(createFloatingPointNode)
const _boolean = _trueKeyword
  .and(constant(true))
  .or(_falseKeyword.and(constant(false)))
  .map(createBooleanNode)
const _string = singleQuoteString.or(doubleQuoteString).map(createStringNode)
const _html = seq([
  token(/[a-z][a-z0-9-]*/y),
  token(/#/y),
  between(_parens, trailingCommaList(lazy(() => _expression))),
]).map(([tag, , children]) => createHTMLNode(tag, children))

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

export let _block: Parser<BlockNode> = error('Not yet implemented')

const _whileStatement = _whileKeyword
  .and(seq([lazy(() => _expression), lazy(() => _block)]))
  .map(([condition, block]) => createWhileNode(condition, block))

export const _statement = _variableDeclaration
  .or(_whileStatement)
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

_block = between(
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
  .or(_boolean)
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
  zeroOrMore(between(_parens, trailingCommaList(lazy(() => _expression)))),
]).map(([left, rights]) =>
  rights.length === 0
    ? left
    : rights.reduce(
        (l, r) => createFunctionCallNode(l, createArgumentListNode(r)),
        left as ExpressionNode
      )
)

const _unary = seq([
  maybe(_subtractionOperator.or(_notOperator)),
  _functionCall,
]).map(([op, expression]) =>
  op !== null ? createUnaryExpressionNode(op, expression) : expression
)

const _factor = seq([
  _functionCall.or(_unary),
  zeroOrMore(pair(_multiplicationOperator.or(_divisionOperator), _unary)),
]).map(foldBinaryExpression)

const _term = seq([
  _factor,
  zeroOrMore(pair(_additionOperator.or(_subtractionOperator), _factor)),
]).map(foldBinaryExpression)

const _comparison = seq([
  _term,
  zeroOrMore(
    pair(
      _lessThanOrEqualOperator
        .or(_greaterThanOrEqualOperator)
        .or(_lessThanOperator)
        .or(_greaterThanOperator),
      _term
    )
  ),
]).map(foldBinaryExpression)

const _equality = seq([
  _comparison,
  zeroOrMore(pair(_equalityOperator.or(_notEqualOperator), _comparison)),
]).map(foldBinaryExpression)

const _assignment = seq([
  _identifier,
  _assignmentOperator,
  lazy(() => _expression),
])
  .map(([left, , right]) => createAssignmentNode(left, right))
  .or(_equality)

_expression = _assignment
