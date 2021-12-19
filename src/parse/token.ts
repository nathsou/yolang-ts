import { DataType, genConstructors, VariantOf, match as matchVariant } from "itsamatch";
import { match, __ } from 'ts-pattern';
import { MonoTy } from "../infer/types";

export type Token = DataType<{
  Symbol: { value: Symbol },
  Keyword: { value: Keyword },
  Const: { value: Const },
  Identifier: { name: string },
  Invalid: { lexeme: string },
}>;

export const Token = {
  Symbol: (value: Symbol): Token => ({ variant: 'Symbol', value }),
  Keyword: (value: Keyword): Token => ({ variant: 'Keyword', value }),
  Const: (value: Const): Token => ({ variant: 'Const', value }),
  Identifier: (name: string): Token => ({ variant: 'Identifier', name }),
  Invalid: (lexeme: string): Token => ({ variant: 'Invalid', lexeme }),
  show: (token: Token) => matchVariant(token, {
    Symbol: ({ value }) => value,
    Keyword: ({ value }) => value,
    Const: ({ value }) => Const.show(value),
    Identifier: ({ name }) => name,
    Invalid: ({ lexeme: message }) => `Invalid token: '${message}'`,
  }),
  eq: (a: Token, b: Token) => match<[Token, Token]>([a, b])
    .with([{ variant: 'Symbol' }, { variant: 'Symbol' }], ([a, b]) => Symbol.eq(a.value, b.value))
    .with([{ variant: 'Keyword' }, { variant: 'Keyword' }], ([a, b]) => Keyword.eq(a.value, b.value))
    .with([{ variant: 'Const' }, { variant: 'Const' }], ([a, b]) => Const.eq(a.value, b.value))
    .with([{ variant: 'Identifier' }, { variant: 'Identifier' }], ([a, b]) => a.name === b.name)
    .with([{ variant: 'Invalid' }, { variant: 'Invalid' }], ([a, b]) => a.lexeme === b.lexeme)
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
  '->', '=>', '==', '!=', '&&=', '||=', '&&', '||', '+=', '-=', '*=', '/=', '%=',
  '+', '-', '*', '/', '%', '<=', '>=', '<', '>', '(', ')', ',',
  ';', '=', '{', '}', '[', ']', ':', '!', '.', '&', '|', '\'', '"',
  '_',
] as const;

export type Symbol = (typeof symbols)[number];

export const Symbol = {
  values: symbols,
  eq: (a: Symbol, b: Symbol) => a === b,
};

const keywords = [
  'let', 'mut', 'in', 'if', 'else', 'fn', 'while',
  'return', 'as', 'unsafe', 'impl', 'extern',
  'module', 'match', 'type',
] as const;

export type Keyword = (typeof keywords)[number];

export const Keyword = {
  values: keywords,
  eq: (a: Keyword, b: Keyword) => a === b,
};

export type Const = DataType<{
  u32: { value: number },
  bool: { value: boolean },
  unit: {},
}>;

const { u32, bool, unit } = genConstructors<Const>()('u32', 'bool', 'unit');

export const Const = {
  u32: (value: number) => u32({ value }),
  bool: (value: boolean) => bool({ value }),
  unit: () => unit({}),
  show: (c: Const) => matchVariant(c, {
    u32: ({ value }) => `${value}`,
    bool: ({ value }) => `${value}`,
    unit: () => '()',
  }),
  eq: (a: Const, b: Const) =>
    match<[Const, Const]>([a, b])
      .with([{ variant: 'u32' }, { variant: 'u32' }], ([a, b]) => a.value === b.value)
      .with([{ variant: 'bool' }, { variant: 'bool' }], ([a, b]) => a.value === b.value)
      .with([{ variant: 'unit' }, { variant: 'unit' }], () => true)
      .otherwise(() => false),
  type: (c: Const) => matchVariant(c, {
    u32: MonoTy.u32,
    bool: MonoTy.bool,
    unit: MonoTy.unit,
  }),
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