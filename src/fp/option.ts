export class Option<T> {
  private _isSome: boolean
  private _value: T

  private constructor(isSome: boolean, value: T) {
    this._isSome = isSome
    this._value = value
  }

  static Some<T>(value?: T): Option<T> {
    return new Option<T>(true, value as T)
  }

  static None = new Option(false, null as any)

  isSome() {
    return this._isSome
  }

  isNone() {
    return !this._isSome
  }

  [Symbol.iterator](): this {
    return this
  }

  next(): IteratorResult<T> {
    if (this.isSome()) {
      return {
        done: true,
        value: this._value,
      }
    }
    return {
      done: true,
      value: undefined,
    }
  }

  /**
   * Maps an `Option<T>` to `Option<U>` by applying a function to a contained `Some` value.
   * Does nothing for `None`.
   */
  map<U>(f: (value: T) => U): Option<U> {
    if (this.isSome()) {
      return Option.Some(f(this._value))
    } else {
      return Option.None
    }
  }
}
