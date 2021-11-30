import { Slice } from "../utils/slice";
import { Maybe, None, Some } from "../utils/maybe";
import { firstSomeBy } from "../utils/array";
import { Spaces } from "./token";
import { Trie, Node } from "../utils/trie";

export type Char = string;

export type Lexer<T> = (input: Slice<Char>) => Maybe<[T, Slice<Char>]>;

export const satisfy = (predicate: (char: Char) => boolean): Lexer<Char> => {
  return (input: Slice<Char>) =>
    Slice.head(input)
      .flatMap(char => predicate(char) ? Some([char, Slice.tail(input)]) : None);
};

const isLowerCaseLetter = (c: Char) => c >= 'a' && c <= 'z';
const isUpperCaseLetter = (c: Char) => c >= 'A' && c <= 'Z';
const isAlpha = (c: Char) => isLowerCaseLetter(c) || isUpperCaseLetter(c);
const isDigit = (c: Char) => c >= '0' && c <= '9';
const isAlphaNum = (c: Char) => isAlpha(c) || isDigit(c);
const isSpace = (char: Char) => Spaces.set.has(char);

export const char = (c: Char) => satisfy(ch => ch === c);
export const lowerLetter = satisfy(isLowerCaseLetter);
export const upperLetter = satisfy(isUpperCaseLetter);
export const letter = satisfy(isAlpha);
export const digit = satisfy(isDigit);
export const alphaNum = satisfy(isAlphaNum);
export const space = satisfy(isSpace);

export const map = <T, U>(lexer: Lexer<T>, f: (t: T) => U): Lexer<U> => {
  return (input: Slice<Char>) => lexer(input).map(([t, rem]) => [f(t), rem]);
};

export const many = <T>(lexer: Lexer<T>, acc: T[] = []): Lexer<T[]> => {
  return (input: Slice<Char>) => {
    if (!Slice.isEmpty(input)) {
      return lexer(input).match({
        Some: ([t, rem]) => many(lexer, [...acc, t])(rem),
        None: () => Some([acc, input]),
      });
    } else {
      return Some([acc, input]);
    }
  };
};

export const spaces = many(space);

export const then = <A, B>(la: Lexer<A>, lb: Lexer<B>): Lexer<[A, B]> => {
  return (input: Slice<Char>) =>
    la(input).flatMap(([a, rem1]) => lb(rem1).map(([b, rem2]) => [[a, b], rem2]));
};

export const some = <T>(lexer: Lexer<T>): Lexer<T[]> =>
  map(then(lexer, many(lexer)), ([head, tail]) => [head, ...tail]);

export const seq3 = <A, B, C>(la: Lexer<A>, lb: Lexer<B>, lc: Lexer<C>): Lexer<[A, B, C]> => {
  return (input: Slice<Char>) =>
    la(input).flatMap(([a, rem1]) =>
      lb(rem1).flatMap(([b, rem2]) =>
        lc(rem2).map(([c, rem3]) => [[a, b, c], rem3])));
};

export const seq = <T>(lexers: Lexer<T>[]): Lexer<T[]> => {
  return (input: Slice<Char>) => {
    if (lexers.length === 0) {
      return Some([[], input]);
    }

    const [head, ...tail] = lexers;
    return head(input).flatMap(([head, rem1]) =>
      seq(tail)(rem1).map(([tail, rem2]) => [[head, ...tail], rem2]));
  };
};

export const alt = <T>(...lexers: Lexer<T>[]): Lexer<T> => {
  return (input: Slice<Char>) => firstSomeBy(lexers, lexer => lexer(input));
};

export const str = (str: string): Lexer<string> => {
  return map(seq(str.split('').map(char)), () => str);
};

export const not = <T>(lexer: Lexer<T>): Lexer<T> => {
  return (input: Slice<Char>) =>
    lexer(input).match({
      Some: () => None,
      None: () => Slice.head(input).map(h => [h, Slice.tail(input)]),
    });
};

export const trie = (strings: readonly string[]): Lexer<string> => {
  const trie = Trie.fromStrings(strings);
  const root = trie.getRoot();

  return (input: Slice<Char>) => {
    let inp = { ...input };
    let next = Some(root);
    let prev: Maybe<Node<string>> = None;
    while (!Slice.isEmpty(inp) && next.isSome()) {
      const char = Slice.head(inp).unwrap();
      inp.start += 1;
      prev = next;
      next = Trie.step(prev.unwrap(), char);
    }

    return prev.flatMap(node => node.value.map(value => [value, Slice.step(inp, -1)]));
  };
};