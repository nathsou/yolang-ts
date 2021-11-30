import { match } from 'ts-pattern';
import { Error, Ok, Result } from '../utils/result';
import { Slice } from "../utils/slice";
import { alphaNum, alt, digit, letter, many, map, not, some, spaces, str, then } from "./lexerCombinators";
import { Const, Keyword, Space, Spaces, Symbol, Token, TokenPos, TokenWithPos, withPos } from "./token";

export const symbol = map(alt(...Symbol.values.map(str)), s => Token.symbol(s as Symbol));

export const keyword = map(
  then(
    alt(...Keyword.values.map(str)),
    not(alphaNum)
  ),
  ([kw, _]) => Token.keyword(kw as Keyword)
);

const digits = map(some(digit), digits => parseInt(digits.join(''), 10));

export const u32 = map(digits, n => Token.const(Const.u32(n)));
export const bool = map(alt(str('true'), str('false')), b => Token.const(Const.bool(b === 'true')));
export const unit = map(str('()'), () => Token.const(Const.unit()));
export const ident = map(then(letter, many(alphaNum)), ([h, tl]) => Token.identifier([h, ...tl].join('')));

export const token = alt(u32, bool, unit, keyword, symbol, ident);

export const lex = (input: string): Result<TokenWithPos[], string> => {
  const tokens: TokenWithPos[] = [];
  const pos: TokenPos = { line: 1, column: 1 };
  const slice = Slice.from((input + ' ').split(''));

  const skipSpaces = () => {
    spaces(slice).match({
      Some: ([spaces, rem]) => {
        spaces.forEach(space => {
          match(space as Space)
            .with(Spaces.enum.space, () => {
              pos.column++;
            })
            .with(Spaces.enum.newline, () => {
              pos.line++;
              pos.column = 1;
            })
            .with(Spaces.enum.tab, () => {
              pos.column += 4;
            })
            .with(Spaces.enum.carriageReturn, () => {
              pos.column = 1;
            })
            .exhaustive();
        });

        slice.start = rem.start;
      },
      None: () => { },
    });
  };

  skipSpaces();

  while (!Slice.isEmpty(slice)) {
    const res = token(slice);
    if (res.isNone()) {
      const context = slice.elems.slice(slice.start, slice.start + 10).join('');
      return Error(`Unrecognized token near '${context}' at ${pos.line}:${pos.column}`);
    } else {
      const [tok, rem] = res.unwrap();
      tokens.push(withPos(tok, pos));
      pos.column += rem.start - slice.start;
      slice.start = rem.start;
    }

    skipSpaces();
  }

  return Ok(tokens);
};