import { Slice } from "../utils/slice";
import { alphaNum, alphaNumUnderscore, alt, digit, letter, many, map, not, oneOrMore, spaces, str, then, trie } from "./lexerCombinators";
import { Const, Keyword, Position, Space, Spaces, Symbol, Token, TokenWithPos, withPos } from "./token";

export const symbol = map(trie(Symbol.values), s => Token.Symbol(s as Symbol));

export const keyword = map(
  then(
    trie(Keyword.values),
    not(alphaNum, false)
  ),
  ([kw, _]) => Token.Keyword(kw as Keyword)
);

const digits = map(oneOrMore(digit), digits => parseInt(digits.join(''), 10));

const intKind = trie(['u32', 'u64', 'i32', 'i64', '']);
export const int = map(then(digits, intKind), ([n, kind]) => Token.Const(Const[kind === '' ? 'i32' : kind](n)));
export const bool = map(alt(str('true'), str('false')), b => Token.Const(Const.bool(b === 'true')));
export const ident = map(then(letter, many(alphaNumUnderscore)), ([h, tl]) => Token.Identifier(h + tl.join('')));

export const token = alt(int, bool, keyword, symbol, ident);

const invalid = map(oneOrMore(not(token)), chars => Token.Invalid(chars.join('').trim()));

const removeComments = (input: string): string => {
  return input.split('\n').map(line => line.split('//')[0]).join('\n');
};

export const lex = (input: string): TokenWithPos[] => {
  const tokens: TokenWithPos[] = [];
  const pos: Position = { line: 1, column: 1 };
  const slice = Slice.from((removeComments(input) + ' ').split(''));

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
    spaces(slice).do(([spaces, rem]) => {
      spaces.forEach(space => {
        spaceActionMap[space as Space](pos);
      });

      slice.start = rem.start;
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

  tokens.push(withPos(Token.EOF(), pos));

  return tokens;
};