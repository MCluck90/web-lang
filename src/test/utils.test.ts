import { assertNotNull } from './utils'

describe('assertNotNull', () => {
  it('should succeed when given a non-null value', () => {
    assertNotNull(1)
  })

  it('should throw when given a null value', () => {
    expect(() => assertNotNull(null)).toThrow()
  })
})
