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

export const Position = {
  show: ({ line, column }: Position): string => `${line}:${column}`,
};

export type TokenWithPos = DataType<{
  [Variant in Token['variant']]: VariantOf<Token, Variant> & { pos: Position }
}>;

export const withPos = (token: Token, pos: Position): TokenWithPos => ({
  ...token,
  pos: { ...pos },
});

const symbols = [
  '->', '=>', '(', ')', ',', ';', '{', '}',
  '[', ']', ':', '.', '\'', '"', '_', '..', '...', '#',
] as const;

export const operators = new Set([
  '=', '+', '-', '*', '/', '==', '!=', 'not', 'mod', 'and', 'or', 'nand',
  'nor', 'xor', 'xnor', '<', '>', '<=', '>=',
  '+=', '-=', '*=', '/=', 'not=', 'mod=', 'and=', 'or=',
  'nand=', 'nor=', 'xor=', 'xnor=',
]);

export type Symbol = (typeof symbols)[number];

export const Symbol = {
  values: symbols,
  eq: (a: Symbol, b: Symbol) => a === b,
};

const keywords = [
  'let', 'mut', 'in', 'if', 'else', 'fun', 'while',
  'return', 'as', 'unsafe', 'impl',
  'module', 'match', 'type', 'trait', 'for',
  'import', 'export', 'sugar', 'pub', 'void',
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
  i32: { value: number },
  u64: { value: number },
  i64: { value: number },
  i8: { value: number },
  u8: { value: number },
  bool: { value: boolean },
  str: { value: string },
}>;

const { u32, i32, u64, i64, i8, u8, bool, str } = genConstructors<Const>([
  'u32', 'i32', 'u64', 'i64', 'i8', 'u8', 'bool', 'str',
]);

export const Const = {
  u32: (value: number) => u32({ value }),
  u64: (value: number) => u64({ value }),
  i32: (value: number) => i32({ value }),
  i64: (value: number) => i64({ value }),
  i8: (value: number) => i8({ value }),
  u8: (value: number) => u8({ value }),
  bool: (value: boolean) => bool({ value }),
  str: (value: string) => str({ value }),
  show: (c: Const) => match(c, {
    u32: ({ value }) => `${value}`,
    i32: ({ value }) => `${value}`,
    u64: ({ value }) => `${value}`,
    i64: ({ value }) => `${value}`,
    i8: ({ value }) => `${value}`,
    u8: ({ value }) => `${value}`,
    bool: ({ value }) => `${value}`,
    str: ({ value }) => `"${value}"`,
    unit: () => '()',
  }),
  eq: (a: Const, b: Const) => a.variant === b.variant && a.value === b.value,
  type: (c: Const): MonoTy => match(c, {
    u32: MonoTy.u32,
    i32: MonoTy.i32,
    u64: MonoTy.u64,
    i64: MonoTy.i64,
    u8: MonoTy.u8,
    i8: MonoTy.i8,
    bool: MonoTy.bool,
    str: MonoTy.str,
  }),
  isInt: (c: Const) => match(c, {
    'bool': () => false,
    'str': () => false,
    _: () => true,
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