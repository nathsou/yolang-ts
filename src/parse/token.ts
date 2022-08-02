import { DataType, genConstructors, VariantOf, match, matchMany } from "itsamatch";
import { MonoTy } from "../infer/types";

export type Token = DataType<{
  Symbol: { value: Symbol },
  Keyword: { value: Keyword },
  Const: { value: Const },
  Identifier: { name: string },
  Invalid: { lexeme: string },
  EOF: {},
}>;

export const Token = {
  Symbol: (value: Symbol): Token => ({ variant: 'Symbol', value }),
  Keyword: (value: Keyword): Token => ({ variant: 'Keyword', value }),
  Const: (value: Const): Token => ({ variant: 'Const', value }),
  Identifier: (name: string): Token => ({ variant: 'Identifier', name }),
  Invalid: (lexeme: string): Token => ({ variant: 'Invalid', lexeme }),
  EOF: (): Token => ({ variant: 'EOF' }),
  show: (token: Token) => match(token, {
    Symbol: ({ value }) => value,
    Keyword: ({ value }) => value,
    Const: ({ value }) => Const.show(value),
    Identifier: ({ name }) => name,
    Invalid: ({ lexeme: message }) => `Invalid token: '${message}'`,
    EOF: () => '<EOF>',
  }),
  eq: (a: Token, b: Token) => matchMany([a, b], {
    'Symbol Symbol': (a, b) => Symbol.eq(a.value, b.value),
    'Keyword Keyword': (a, b) => Keyword.eq(a.value, b.value),
    'Const Const': (a, b) => Const.eq(a.value, b.value),
    'Identifier Identifier': (a, b) => a.name === b.name,
    'Invalid Invalid': (a, b) => a.lexeme === b.lexeme,
    'EOF EOF': () => true,
    _: () => false,
  }),
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
  '_', '...',
] as const;

export type Symbol = (typeof symbols)[number];

export const Symbol = {
  values: symbols,
  eq: (a: Symbol, b: Symbol) => a === b,
};

const keywords = [
  'let', 'mut', 'in', 'if', 'else', 'fun', 'while',
  'return', 'as', 'unsafe', 'impl', 'extern',
  'module', 'match', 'type', 'trait', 'for',
  'wasm', 'use', 'sugar',
] as const;

export type Keyword = (typeof keywords)[number];

export const Keyword = {
  values: keywords,
  valuesSet: new Set(keywords),
  eq: (a: Keyword, b: Keyword) => a === b,
  is: (ident: string): ident is Keyword => Keyword.valuesSet.has(ident as any),
};

export type Const = DataType<{
  u32: { value: number },
  bool: { value: boolean },
  unit: {},
}>;

const { u32, bool, unit } = genConstructors<Const>(['u32', 'bool', 'unit']);

export const Const = {
  u32: (value: number) => u32({ value }),
  bool: (value: boolean) => bool({ value }),
  unit: () => unit({}),
  show: (c: Const) => match(c, {
    u32: ({ value }) => `${value}`,
    bool: ({ value }) => `${value}`,
    unit: () => '()',
  }),
  eq: (a: Const, b: Const) => matchMany([a, b], {
    'u32 u32': (a, b) => a.value === b.value,
    'bool bool': (a, b) => a.value === b.value,
    'unit unit': () => true,
    _: () => false,
  }),
  type: (c: Const) => match(c, {
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