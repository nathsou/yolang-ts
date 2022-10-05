import { panic } from "./misc";

type Raw<L, R> = { type: 'left', value: L } | { type: 'right', value: R };

export class Either<L, R> {
  private raw: Raw<L, R>;

  private constructor(raw: Raw<L, R>) {
    this.raw = raw;
  }

  static left<L, R>(value: L): Either<L, R> {
    return new Either<L, R>({ type: 'left', value });
  }

  static right<L, R>(value: R): Either<L, R> {
    return new Either<L, R>({ type: 'right', value });
  }

  public match<T>(handler: { Left: (value: L) => T, Right: (value: R) => T }): T {
    if (this.raw.type === 'left') {
      return handler.Left(this.raw.value);
    }

    return handler.Right(this.raw.value);
  }

  public map<A, B>(mapper: { left: (value: L) => A, right: (value: R) => B }): Either<A, B> {
    return this.match({
      Left: value => Either.left(mapper.left(value)),
      Right: value => Either.right(mapper.right(value)),
    });
  }

  public isLeft(): boolean {
    return this.raw.type === 'left';
  }

  public isRight(): boolean {
    return this.raw.type === 'right';
  }

  public unwrapLeft(message?: string): L {
    if (this.raw.type === 'left') {
      return this.raw.value;
    }

    return panic(`Called unwrapLeft on a right either: ${message}`);
  }

  public unwrapRight(message?: string): R {
    if (this.raw.type === 'right') {
      return this.raw.value;
    }

    return panic(`Called unwrapRight on a left either: ${message}`);
  }

  public value(): Readonly<L | R> {
    return this.raw.value;
  }
}