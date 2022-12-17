import { assertNodeType, assertSuccessfulParse } from '../test/parser-utils'
import {
  createIdentifierNode,
  createPropertyKeyNode,
  createTypePropertyNode,
  createNamedTypeNode,
  createObjectTypeNode,
} from './ast'
import { _remoteDefinition } from './remote'

describe('_remoteDefinition', () => {
  test('can parse an empty GET remote', () => {
    const source = 'remote GetThings { GET /things }'
    const remote = _remoteDefinition.parseToEnd(source)
    assertSuccessfulParse(remote)
    assertNodeType(remote, 'RemoteDefinition')
    expect(remote.type).toBe('GET')
    expect(remote.url.path).toBe('/things')
  })

  test('can parse parameters from URL', () => {
    const source = `
      remote GetTodo {
        GET /todo/:id/and/:isDone
      }
    `
    const remote = _remoteDefinition.parseToEnd(source)
    assertSuccessfulParse(remote)
    assertNodeType(remote, 'RemoteDefinition')
    expect(remote.url.path).toBe('/todo/:id/and/:isDone')
    expect(remote.url.parameters).toHaveLength(2)
    expect(remote.url.parameters[0].name.value).toBe('id')
    expect(remote.url.parameters[1].name.value).toBe('isDone')
  })

  test('can parse a GET remote with type properties', () => {
    const source = `
      remote GetThings {
        GET /things

        response: {
          success: bool,
          data: Thing[]
        }
        output: Thing[]
      }
    `
    const remote = _remoteDefinition.parseToEnd(source)
    assertSuccessfulParse(remote)
    assertNodeType(remote, 'RemoteDefinition')
    expect(remote.type).toBe('GET')
    expect(remote.url.path).toBe('/things')
    expect(remote.properties[0]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('response')),
        createObjectTypeNode([
          createTypePropertyNode(
            createPropertyKeyNode(createIdentifierNode('success')),
            createNamedTypeNode('bool', [], false)
          ),
          createTypePropertyNode(
            createPropertyKeyNode(createIdentifierNode('data')),
            createNamedTypeNode('Thing', [], true)
          ),
        ])
      )
    )
    expect(remote.properties[1]).toEqual(
      createTypePropertyNode(
        createPropertyKeyNode(createIdentifierNode('output')),
        createNamedTypeNode('Thing', [], true)
      )
    )
  })
})
