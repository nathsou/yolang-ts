
export class Maybe<T> {
  private value: T | null;
  public static readonly None = new Maybe(null);

  private constructor(value: T | null) {
    this.value = value;
  }

  static Some<T>(val: T): Maybe<T> {
    return new Maybe(val);
  }

  isSome(): boolean {
    return this.value !== null;
  }

  isNone(): boolean {
    return this.value === null;
  }

  map<U>(f: (val: T) => U): Maybe<U> {
    if (this.isSome()) {
      return Some(f(this.value));
    }

    return None;
  }

  flatMap<U>(f: (val: T) => Maybe<U>): Maybe<U> {
    if (this.isSome()) {
      return f(this.value);
    }

    return None;
  }
}

export const { None, Some } = Maybe;