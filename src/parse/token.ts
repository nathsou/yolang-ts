import { DataType, genConstructors, VariantOf } from "itsamatch";
import { match } from 'ts-pattern';

export type Token = DataType<{
  Symbol: { value: Symbol },
  Keyword: { value: Keyword },
  Const: { value: Const },
  Identifier: { name: string },
  EOF: {}
}>;

export const Token = {
  symbol: (value: Symbol): Token => ({ variant: 'Symbol', value }),
  keyword: (value: Keyword): Token => ({ variant: 'Keyword', value }),
  const: (value: Const): Token => ({ variant: 'Const', value }),
  identifier: (name: string): Token => ({ variant: 'Identifier', name }),
  eof: (): Token => ({ variant: 'EOF' }),
  show: (token: Token) => match(token)
    .with({ variant: 'Symbol' }, ({ value }) => value)
    .with({ variant: 'Keyword' }, ({ value }) => value)
    .with({ variant: 'Const' }, ({ value }) => Const.show(value))
    .with({ variant: 'Identifier' }, ({ name }) => name)
    .with({ variant: 'EOF' }, () => 'EOF')
    .exhaustive(),
  eq: (a: Token, b: Token) => match<[Token, Token]>([a, b])
    .with([{ variant: 'Symbol' }, { variant: 'Symbol' }], ([a, b]) => Symbol.eq(a.value, b.value))
    .with([{ variant: 'Keyword' }, { variant: 'Keyword' }], ([a, b]) => Keyword.eq(a.value, b.value))
    .with([{ variant: 'Const' }, { variant: 'Const' }], ([a, b]) => Const.eq(a.value, b.value))
    .with([{ variant: 'Identifier' }, { variant: 'Identifier' }], ([a, b]) => a.name === b.name)
    .otherwise(() => false),
};

export type Position = {
  line: number,
  column: number,
};

export type TokenWithPos = DataType<{
  [Variant in Token['variant']]: VariantOf<Token, Variant> & { pos: Position }
}>;

export const withPos = (token: Token, pos: Position): TokenWithPos => ({
  ...token,
  pos: { ...pos },
});

const symbols = [
  '->', '==', '!=', '&&', '||', '+=', '-=', '*=', '/=', '%=',
  '+', '-', '*', '/', '%', '<=', '>=', '<', '>', '(', ')', ',',
  ';', '=', '{', '}', '[', ']', ':', '!', '.', '&', '|', '\'', '"',
] as const;

export type Symbol = (typeof symbols)[number];

export const Symbol = {
  values: symbols,
  eq: (a: Symbol, b: Symbol) => a === b,
};

const keywords = [
  'let', 'mut', 'in', 'if', 'else', 'fn', 'while',
  'return', 'as', 'unsafe', 'struct', 'impl', 'extern',
] as const;

export type Keyword = (typeof keywords)[number];

export const Keyword = {
  values: keywords,
  eq: (a: Keyword, b: Keyword) => a === b,
};

export type Const = DataType<{
  u32: { value: number },
  bool: { value: boolean },
}>;

const { u32, bool } = genConstructors<Const>()('u32', 'bool');

export const Const = {
  u32: (value: number) => u32({ value }),
  bool: (value: boolean) => bool({ value }),
  show: (c: Const) => match(c)
    .with({ variant: 'u32' }, ({ value }) => `${value}`)
    .with({ variant: 'bool' }, ({ value }) => `${value}`)
    .exhaustive(),
  eq: (a: Const, b: Const) => a.variant === b.variant && a.value === b.value,
};

const spaces = {
  space: ' ',
  newline: '\n',
  tab: '\t',
  carriageReturn: '\r',
} as const;

export type Space = (typeof spaces)[keyof typeof spaces];

export const Spaces = {
  enum: spaces,
  set: new Set<string>(Object.values(spaces)),
};