import { DataType, genConstructors, match, matchMany } from "itsamatch";
import { MonoTy } from "../infer/types";
import { FileSystem } from "../resolve/fileSystem";

export type Position = {
  line: number,
  column: number,
  path: string,
};

export const Position = {
  DONT_CARE: Object.freeze<Position>({ line: 0, column: 0, path: '' }),
  show: ({ line, column, path }: Position): string => `${path}:${line}:${column}`,
  showWithShortPath: ({ line, column, path }: Position, fs: FileSystem): string => {
    const shortPath = fs.relative(process.cwd(), path);
    const finalPath = shortPath.length < path.length ? './' + shortPath : path;
    return `${finalPath}:${line}:${column}`;
  },
};

export type WithPos<T> = { [K in keyof T]: T[K] & { pos: Position } };

export type Token = DataType<WithPos<{
  Symbol: { value: Symbol },
  Operator: { value: Operator },
  Keyword: { value: Keyword },
  Const: { value: Const },
  Identifier: { name: string },
  Invalid: { lexeme: string },
  EOF: {},
}>>;

export const Token = {
  Symbol: (value: Symbol, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Symbol', value, pos }),
  Operator: (value: Operator, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Operator', value, pos }),
  Keyword: (value: Keyword, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Keyword', value, pos }),
  Const: (value: Const, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Const', value, pos }),
  Identifier: (name: string, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Identifier', name, pos }),
  Invalid: (lexeme: string, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Invalid', lexeme, pos }),
  EOF: (pos: Position = Position.DONT_CARE): Token => ({ variant: 'EOF', pos }),
  show: (token: Token) => match(token, {
    Symbol: ({ value }) => value,
    Operator: ({ value }) => value,
    Keyword: ({ value }) => value,
    Const: ({ value }) => Const.show(value),
    Identifier: ({ name }) => name,
    Invalid: ({ lexeme: message }) => `Invalid token: '${message}'`,
    EOF: () => '<EOF>',
  }),
  eq: (a: Token, b: Token) => matchMany([a, b], {
    'Symbol Symbol': (a, b) => Symbol.eq(a.value, b.value),
    'Operator Operator': (a, b) => Operator.eq(a.value, b.value),
    'Keyword Keyword': (a, b) => Keyword.eq(a.value, b.value),
    'Const Const': (a, b) => Const.eq(a.value, b.value),
    'Identifier Identifier': (a, b) => a.name === b.name,
    'Invalid Invalid': (a, b) => a.lexeme === b.lexeme,
    'EOF EOF': () => true,
    _: () => false,
  }),
};

const symbols = Object.freeze([
  '->', '=>', '(', ')', ',', ';', '{', '}', '!',
  '[', ']', ':', '.', '\'', '"', '_', '..', '...', '#',
] as const);

export type Symbol = (typeof symbols)[number];

export const Symbol = {
  values: symbols,
  eq: (a: Symbol, b: Symbol) => a === b,
};

const specialOperators = Object.freeze([
  '=', '+', '-', '*', '/', '**',
  '&', '|', '~', '^', '<<', '>>',
  '==', '!=', '<', '>', '<=', '>=',
  '+=', '-=', '*=', '/=',
  'not=', 'mod=', 'and=', 'or=', 'nand=', 'nor=', 'xor=', 'xnor=',
  '&=', '|=', '^=', '<<=', '>>=',
  '[]', '[]=',
] as const);

const allLetterOperators = Object.freeze([
  'not', 'and', 'or', 'nand', 'nor', 'xor', 'xnor', 'mod',
] as const);

export type SpecialOperator = (typeof specialOperators)[number];
export type Operator = (typeof allLetterOperators)[number] | SpecialOperator;

const specialOperatorsSet = new Set<string>(specialOperators);
const allLetterOperatorsSet = new Set<string>(allLetterOperators);

export const Operator = {
  eq: (a: Operator, b: Operator) => a === b,
  isAny: (str: string): str is Operator => specialOperatorsSet.has(str) || allLetterOperatorsSet.has(str),
  isSpecial: (str: string): str is SpecialOperator => specialOperatorsSet.has(str),
};

const keywords = Object.freeze([
  'let', 'mut', 'in', 'if', 'else', 'fun', 'while',
  'return', 'as', 'unsafe', 'impl',
  'module', 'match', 'type', 'trait', 'for',
  'import', 'export', 'sugar', 'pub', 'void',
] as const);

export type Keyword = (typeof keywords)[number];

export const Keyword = {
  values: keywords,
  valuesSet: new Set<string>(keywords),
  eq: (a: Keyword, b: Keyword) => a === b,
  is: (ident: string): ident is Keyword => Keyword.valuesSet.has(ident),
};

export type IntKind = `${'i' | 'u'}${8 | 16 | 32 | 64 | 128}`;
export type FloatKind = `f${16 | 32 | 64}`;

export type Const = DataType<{
  int: { value: bigint, kind: IntKind | '?' },
  float: { value: number, kind: FloatKind | '?' },
  bool: { value: boolean },
  str: { value: string },
  cstr: { value: string },
}>;

const { int, float, bool, str, cstr } = genConstructors<Const>(['int', 'float', 'bool', 'str', 'cstr']);

export const Const = {
  int: (n: bigint | number, kind: IntKind | '?') => int({ value: BigInt(n), kind }),
  float: (x: number, kind: FloatKind | '?') => float({ value: x, kind }),
  bool: (value: boolean) => bool({ value }),
  str: (value: string) => str({ value }),
  cstr: (value: string) => cstr({ value }),
  type: (c: Const): MonoTy => match(c, {
    bool: MonoTy.bool,
    int: ({ kind }) => MonoTy.int(kind),
    float: ({ kind }) => MonoTy.float(kind),
    str: MonoTy.str,
    cstr: MonoTy.cstr,
  }),
  show: (c: Const): string => match(c, {
    int: ({ value, kind }) => kind === '?' ? `${value}` : `${value}${kind}`,
    float: ({ value, kind }) => kind === '?' ? `${value}` : `${value}${kind}`,
    bool: ({ value }) => `${value}`,
    str: ({ value }) => `"${value}"`,
    cstr: ({ value }) => `c"${value}"`,
  }),
  eq: (a: Const, b: Const) => a.variant === b.variant && a.value === b.value,
};
