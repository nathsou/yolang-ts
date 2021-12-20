import { Slice } from "../utils/slice";
import { Parser } from "./combinators";
import { Symbol, Token } from "./token";

export type RecoveryStrategy = (tokens: Slice<Token>) => Slice<Token>;

export const RecoveryStrategy = {
  stay: (tokens: Slice<Token>) => tokens,
  skip: (p: Parser<any>): RecoveryStrategy => tokens => {
    const [_, rem] = p.ref(tokens);
    return rem;
  },
  reachNearest: (token: Token): RecoveryStrategy => {
    return tokens => {
      const slice = Slice.clone(tokens);

      while (!Slice.isEmpty(slice)) {
        const head = Slice.head(slice).unwrap();
        if (Token.eq(head, token)) {
          return slice;
        }

        Slice.stepMut(slice);
      }

      return slice;
    };
  },
  skipNested: (left: Symbol, right: Symbol): RecoveryStrategy => tokens => {
    let depth = 1;
    const slice = Slice.clone(tokens);

    while (depth > 0 && !Slice.isEmpty(slice)) {
      const token = Slice.head(slice).unwrap();
      if (token.variant === 'Symbol' && token.value === left) {
        depth += 1;
      } else if (token.variant === 'Symbol' && token.value === right) {
        depth -= 1;
      }

      Slice.stepMut(slice);
    }

    return slice;
  },
};