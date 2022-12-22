import { token } from './common'

export const _renderKeyword = token(/render/y)
export const _letKeyword = token(/let/y)
export const _mutKeyword = token(/mut/y)
export const _typeKeyword = token(/type/y)
export const _intKeyword = token(/int/y)
export const _stringKeyword = token(/string/y)
export const _boolKeyword = token(/bool/y)
export const _remoteKeyword = token(/remote/y)
export const _keyword = _renderKeyword
  .or(_typeKeyword)
  .or(_intKeyword)
  .or(_stringKeyword)
  .or(_boolKeyword)
  .or(_remoteKeyword)

export const _builtInTypeKeyword = _intKeyword
  .or(_stringKeyword)
  .or(_boolKeyword)
