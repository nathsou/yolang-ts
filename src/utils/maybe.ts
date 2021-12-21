import { panic } from "./misc";

type M<T> = { type: 'some', data: T } | { type: 'none' };

export class Maybe<T> {
  private raw: M<T>;
  public static readonly none = new Maybe<any>({ type: 'none' });

  private constructor(value: M<T>) {
    this.raw = value;
  }

  static some<T>(val: T): Maybe<T> {
    return new Maybe({ type: 'some', data: val });
  }

  isSome(): boolean {
    return this.raw.type === 'some';
  }

  isNone(): boolean {
    return this.raw.type === 'none';
  }

  map<U>(f: (val: T) => U): Maybe<U> {
    if (this.raw.type === 'some') {
      return some(f(this.raw.data));
    }

    return none;
  }

  do(f: (val: T) => void): void {
    if (this.raw.type === 'some') {
      f(this.raw.data);
    }
  }

  mapWithDefault<U>(f: (val: T) => U, defaultValue: U): U {
    if (this.raw.type === 'some') {
      return f(this.raw.data);
    }

    return defaultValue;
  }

  flatMap<U>(f: (val: T) => Maybe<U>): Maybe<U> {
    if (this.raw.type === 'some') {
      return f(this.raw.data);
    }

    return none;
  }

  or(other: Maybe<T>): Maybe<T> {
    if (this.raw.type === 'some') {
      return this;
    }

    return other;
  }

  orDefault(defaultValue: T): T {
    if (this.raw.type === 'some') {
      return this.raw.data;
    }

    return defaultValue;
  }

  match<U>(actions: { Some: (data: T) => U, None: () => U }): U {
    if (this.raw.type === 'some') {
      return actions.Some(this.raw.data);
    }

    return actions.None();
  }

  unwrap(): T {
    if (this.raw.type === 'some') {
      return this.raw.data;
    }

    return panic('Tried to unwrap a None value');
  }

  show(showData: (data: T) => string = JSON.stringify): string {
    if (this.raw.type === 'some') {
      return `Some(${showData(this.raw.data)})`;
    }

    return 'None';
  }
}

export const { none, some } = Maybe;