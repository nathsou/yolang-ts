import { panic } from './misc';

type Ok<T> = { type: 'ok', data: T };
type Err<E> = { type: 'error', data: E };
type Res<T, E> = Ok<T> | Err<E>;

export class Result<T, E> {
  private res: Res<T, E>;

  private constructor(res: Res<T, E>) {
    this.res = res;
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
    return this.res.type === 'ok';
  }

  isError(): boolean {
    return this.res.type === 'error';
  }

  map<U>(f: (data: T) => U): Result<U, E> {
    if (this.res.type === 'ok') {
      return Result.ok(f(this.res.data));
    }

    return Result.error(this.res.data);
  }

  flatMap<U>(f: (data: T) => Result<U, E>): Result<U, E> {
    if (this.res.type === 'ok') {
      return f(this.res.data);
    }

    return Result.error(this.res.data);
  }

  match<U>(actions: { Ok: (data: T) => U, Error: (error: E) => U }): U {
    if (this.res.type === 'ok') {
      return actions.Ok(this.res.data);
    }

    return actions.Error(this.res.data);
  }

  unwrap(): T {
    if (this.res.type === 'ok') {
      return this.res.data;
    }

    return panic(`Tried to unwrap an error result: ${this.show()}`);
  }

  show(
    showData: (data: T) => string = JSON.stringify,
    showError: (error: E) => string = JSON.stringify
  ): string {
    if (this.res.type === 'ok') {
      return `Ok(${showData(this.res.data)})`;
    }

    return `Error(${showError(this.res.data)})`;
  }
}

export const { ok, error } = Result;