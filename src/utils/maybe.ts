import { panic } from "./misc";

type M<T> = { type: 'some', data: T } | { type: 'none' };

export class Maybe<T> {
  private raw: M<T>;
  public static readonly None = new Maybe<any>({ type: 'none' });

  private constructor(value: M<T>) {
    this.raw = value;
  }

  static Some<T>(val: T): Maybe<T> {
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
      return Some(f(this.raw.data));
    }

    return None;
  }

  flatMap<U>(f: (val: T) => Maybe<U>): Maybe<U> {
    if (this.raw.type === 'some') {
      return f(this.raw.data);
    }

    return None;
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

    panic('Tried to unwrap a None value');
  }

  show(showData: (data: T) => string = JSON.stringify): string {
    if (this.raw.type === 'some') {
      return `Some(${showData(this.raw.data)})`;
    }

    return 'None';
  }
}

export const { None, Some } = Maybe;