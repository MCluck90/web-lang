import { assertFailedParse, assertSuccessfulParse } from '../test/parser-utils'
import {
  createIdentifierNode,
  createUsePackageNode,
  createUseSelectorNode,
} from './ast'
import { _use } from './program'

describe('Use Statements', () => {
  describe('Package Imports', () => {
    test.each([
      [
        'use @scope:package/{}',
        createUsePackageNode('scope', 'package', '', []),
      ],
      ['use @std:io/{}', createUsePackageNode('std', 'io', '', [])],
      [
        'use @scope:package/level1/level2/level3/{}',
        createUsePackageNode('scope', 'package', '/level1/level2/level3', []),
      ],
    ])('can import scoped packages', (source, expected) => {
      const useStatement = _use.parseToEnd(source)
      assertSuccessfulParse(useStatement)
      expect(useStatement).toEqual(expected)
    })

    test('fails when no selectors are given', () => {
      const source = 'use @scope:package/'
      assertFailedParse(_use.parseToEnd(source))
    })

    test.each([
      [
        'use @scope:package/{itemA, itemB, itemC}',
        createUsePackageNode('scope', 'package', '', [
          createUseSelectorNode(createIdentifierNode('itemA'), null),
          createUseSelectorNode(createIdentifierNode('itemB'), null),
          createUseSelectorNode(createIdentifierNode('itemC'), null),
        ]),
      ],
      [
        // Support trailing commas
        `use @scope:package/{
           itemA,
           itemB,
           itemC,
         }`,
        createUsePackageNode('scope', 'package', '', [
          createUseSelectorNode(createIdentifierNode('itemA'), null),
          createUseSelectorNode(createIdentifierNode('itemB'), null),
          createUseSelectorNode(createIdentifierNode('itemC'), null),
        ]),
      ],
    ])('can select a list of items', (source, expected) => {
      const useStatement = _use.parseToEnd(source)
      assertSuccessfulParse(useStatement)
      expect(useStatement).toEqual(expected)
    })

    test('supports wildcard imports', () => {
      const source = 'use @scope:package/{ * as allItems }'
      const useStatement = _use.parseToEnd(source)
      assertSuccessfulParse(useStatement)
      expect(useStatement.selectors[0].name).toBe('*')
      expect(useStatement.selectors[0].alias?.value).toBe('allItems')
    })

    test('wildcard imports must have an alias', () => {
      const source = 'use @scope:package/{ * }'
      assertFailedParse(_use.parseToEnd(source))
    })
  })
})
