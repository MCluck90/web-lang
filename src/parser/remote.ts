import { list, pair, Parser, regexp, text, zeroOrMore } from 'parsnip-ts'
import {
  createRemoteParameterNode,
  createRemoteDefinitionNode,
  createRemoteUrlNode,
  isNodeType,
  RemoteType,
} from './ast'
import { between, token } from './combinators'
import { _identifier } from './common'
import { _remoteKeyword } from './keywords'
import { _methodDefinition } from './methods'
import { _typeProperty } from './types'

export const _remoteType = token(
  /GET|POST|PUT|PATCH|DELETE/y
) as Parser<RemoteType>

const _pathParameter = text(':').and(_identifier).map(createRemoteParameterNode)
const _segment = regexp(/[a-z0-9\-_]+/y)
export const _remoteUrl = token(/\//y)
  .and(list(_pathParameter.or(_segment), text('/')))
  .map((parametersAndSegments) => {
    const path = parametersAndSegments.reduce(
      (acc: string, s) =>
        acc + '/' + (typeof s === 'string' ? s : `:${s.name.value}`),
      ''
    )
    const parameters = parametersAndSegments.filter(
      isNodeType('RemoteParameter')
    )
    return createRemoteUrlNode(path, parameters)
  })

export const _remoteDefinition = _remoteKeyword.and(_identifier).bind((name) =>
  between(
    [token(/{/y), token(/}/y)],
    pair(_remoteType, _remoteUrl).bind(([type, url]) =>
      zeroOrMore(_typeProperty.or(_methodDefinition)).map(
        (propertiesOrMethods) => {
          const properties = propertiesOrMethods.filter(
            isNodeType('TypeProperty')
          )
          const methods = propertiesOrMethods.filter(
            isNodeType('MethodDefinition')
          )
          return createRemoteDefinitionNode(
            name,
            type,
            url,
            properties,
            methods
          )
        }
      )
    )
  )
)
