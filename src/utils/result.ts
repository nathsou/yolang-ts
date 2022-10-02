import { panic } from './misc';

type Ok<T> = { type: 'ok', data: T };
type Err<E> = { type: 'error', data: E };
type Res<T, E> = Ok<T> | Err<E>;

export class Result<T, E> {
  public raw: Res<T, E>;

  private constructor(res: Res<T, E>) {
    this.raw = res;
  }

  static ok<T, E>(data: T): Result<T, E> {
    return new Result<T, E>({ type: 'ok', data });
  }

  static error<T, E>(data: E): Result<T, E> {
    return new Result<T, E>({ type: 'error', data });
  }

  static wrap<T, E>([data, errors]: [T, E[]]): Result<T, E[]> {
    return errors.length === 0 ? Result.ok(data) : Result.error(errors);
  }

  isOk(): boolean {
    return this.raw.type === 'ok';
  }

  isError(): boolean {
    return this.raw.type === 'error';
  }

  map<U>(f: (data: T) => U): Result<U, E> {
    if (this.raw.type === 'ok') {
      return Result.ok(f(this.raw.data));
    }

    return Result.error(this.raw.data);
  }

  flatMap<U>(f: (data: T) => Result<U, E>): Result<U, E> {
    if (this.raw.type === 'ok') {
      return f(this.raw.data);
    }

    return Result.error(this.raw.data);
  }

  match<U>(actions: { Ok: (data: T) => U, Error: (error: E) => U }): U {
    if (this.raw.type === 'ok') {
      return actions.Ok(this.raw.data);
    }

    return actions.Error(this.raw.data);
  }

  unwrap(): T {
    if (this.raw.type === 'ok') {
      return this.raw.data;
    }

    return panic(`Tried to unwrap an error result: ${this.show()}`);
  }

  unwrapError(): E {
    if (this.raw.type === 'error') {
      return this.raw.data;
    }

    return panic(`Tried to unwrapError an ok result: ${this.show()}`);
  }

  show(
    showData: (data: T) => string = JSON.stringify,
    showError: (error: E) => string = JSON.stringify
  ): string {
    if (this.raw.type === 'ok') {
      return `Ok(${showData(this.raw.data)})`;
    }

    return `Error(${showError(this.raw.data)})`;
  }

  static firstOkBy<T, E, U>(elems: T[], f: (elem: T) => Result<U, E>, err: E): Result<U, E> {
    for (const elem of elems) {
      const mapped = f(elem);
      if (mapped.isOk()) {
        return mapped;
      }
    }

    return error(err);
  }
}

export const { ok, error } = Result;