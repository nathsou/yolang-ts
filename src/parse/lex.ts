import { error, ok, Result } from '../utils/result';
import { Slice } from "../utils/slice";
import { alphaNum, alt, digit, letter, many, map, not, oneOrMore, spaces, str, then, trie } from "./lexerCombinators";
import { Const, Keyword, Space, Spaces, Symbol, Token, Position, TokenWithPos, withPos } from "./token";

export const symbol = map(trie(Symbol.values), s => Token.symbol(s as Symbol));

export const keyword = map(
  then(
    trie(Keyword.values),
    not(alphaNum)
  ),
  ([kw, _]) => Token.keyword(kw as Keyword)
);

const digits = map(oneOrMore(digit), digits => parseInt(digits.join(''), 10));

export const u32 = map(digits, n => Token.const(Const.u32(n)));
export const bool = map(alt(str('true'), str('false')), b => Token.const(Const.bool(b === 'true')));
export const ident = map(then(letter, many(alphaNum)), ([h, tl]) => Token.identifier(h + tl.join('')));

export const token = alt(u32, bool, keyword, symbol, ident);

export const lex = (input: string): Result<TokenWithPos[], string> => {
  const tokens: TokenWithPos[] = [];
  const pos: Position = { line: 1, column: 1 };
  const slice = Slice.from((input + ' ').split(''));

  const spaceActionMap: { [S in Space]: (pos: Position) => void } = {
    [Spaces.enum.space]: pos => {
      pos.column += 1;
    },
    [Spaces.enum.newline]: pos => {
      pos.line += 1;
      pos.column = 1;
    },
    [Spaces.enum.tab]: pos => {
      pos.column += 4;
    },
    [Spaces.enum.carriageReturn]: pos => {
      pos.column = 1;
    },
  };

  const skipSpaces = () => {
    spaces(slice).match({
      Some: ([spaces, rem]) => {
        spaces.forEach(space => {
          spaceActionMap[space as Space](pos);
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
      return error(`Unrecognized token near '${context}' at ${pos.line}:${pos.column}`);
    } else {
      const [tok, rem] = res.unwrap();
      tokens.push(withPos(tok, pos));
      pos.column += rem.start - slice.start;
      slice.start = rem.start;
    }

    skipSpaces();
  }

  return ok(tokens);
};