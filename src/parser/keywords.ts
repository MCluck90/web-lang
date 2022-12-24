import { token } from './common'

export const _boolKeyword = token(/bool/y)
export const _fnKeyword = token(/fn/y)
export const _ifKeyword = token(/if/y)
export const _elseKeyword = token(/else/y)
export const _intKeyword = token(/int/y)
export const _jsAsmKeyword = token(/%js-asm%/y)
export const _letKeyword = token(/let/y)
export const _mutKeyword = token(/mut/y)
export const _renderKeyword = token(/render/y)
export const _stringKeyword = token(/string/y)
export const _typeKeyword = token(/type/y)
export const _keyword = _boolKeyword
  .or(_boolKeyword)
  .or(_fnKeyword)
  .or(_intKeyword)
  .or(_jsAsmKeyword)
  .or(_letKeyword)
  .or(_mutKeyword)
  .or(_renderKeyword)
  .or(_stringKeyword)
  .or(_typeKeyword)

export const _builtInTypeKeyword = _intKeyword
  .or(_stringKeyword)
  .or(_boolKeyword)
