export function assertNotNull<T>(value: T): asserts value is Exclude<T, null> {
  if (value === null) {
    throw new Error('Expected non-null value')
  }
}
