
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

  public match<T>(handler: { left: (value: L) => T, right: (value: R) => T }): T {
    if (this.raw.type === 'left') {
      return handler.left(this.raw.value);
    }

    return handler.right(this.raw.value);
  }

  public map<A, B>(mapper: { left: (value: L) => A, right: (value: R) => B }): Either<A, B> {
    return this.match({
      left: value => Either.left(mapper.left(value)),
      right: value => Either.right(mapper.right(value)),
    });
  }

  public isLeft(): boolean {
    return this.raw.type === 'left';
  }

  public isRight(): boolean {
    return this.raw.type === 'right';
  }
}