import { Slice } from "../utils/slice";
import { alphaNum, alt, digit, letter, many, map, not, oneOrMore, spaces, str, then, trie } from "./lexerCombinators";
import { Const, Keyword, Position, Space, Spaces, Symbol, Token, TokenWithPos, withPos } from "./token";

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

const invalid = map(oneOrMore(not(token)), chars => Token.invalid(chars.join('').trim()));

export const lex = (input: string): TokenWithPos[] => {
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
    const [tok, rem] = alt(token, invalid)(slice).unwrap();
    tokens.push(withPos(tok, pos));
    pos.column += rem.start - slice.start;
    slice.start = rem.start;

    skipSpaces();
  }

  return [...tokens, withPos(Token.eof(), pos)];
};