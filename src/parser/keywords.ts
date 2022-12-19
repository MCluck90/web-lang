import { token } from './common'

export const _mainKeyword = token(/main/y)
export const _typeKeyword = token(/type/y)
export const _intKeyword = token(/int/y)
export const _stringKeyword = token(/string/y)
export const _boolKeyword = token(/bool/y)
export const _remoteKeyword = token(/remote/y)
export const _keyword = _mainKeyword
  .or(_typeKeyword)
  .or(_intKeyword)
  .or(_stringKeyword)
  .or(_boolKeyword)
  .or(_remoteKeyword)

export const _builtInTypeKeyword = _intKeyword
  .or(_stringKeyword)
  .or(_boolKeyword)
