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
  Keyword: { value: Keyword },
  Const: { value: Const },
  Identifier: { name: string },
  Invalid: { lexeme: string },
  EOF: {},
}>>;

export const Token = {
  Symbol: (value: Symbol, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Symbol', value, pos }),
  Keyword: (value: Keyword, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Keyword', value, pos }),
  Const: (value: Const, pos: Position): Token => ({ variant: 'Const', value, pos }),
  Identifier: (name: string, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Identifier', name, pos }),
  Invalid: (lexeme: string, pos: Position = Position.DONT_CARE): Token => ({ variant: 'Invalid', lexeme, pos }),
  EOF: (pos: Position = Position.DONT_CARE): Token => ({ variant: 'EOF', pos }),
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

const symbols = [
  '->', '=>', '(', ')', ',', ';', '{', '}', '!',
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
  valuesSet: new Set<string>(keywords),
  eq: (a: Keyword, b: Keyword) => a === b,
  is: (ident: string): ident is Keyword => Keyword.valuesSet.has(ident),
};

export type IntKind = `${'i' | 'u'}${8 | 32 | 64}`;

type Typed<T> = { [K in keyof T]: T[K] & { ty: MonoTy } };

export type Const = DataType<Typed<{
  int: { value: number, kind: IntKind | '?' },
  bool: { value: boolean },
  str: { value: string },
}>>;

const { int, bool, str } = genConstructors<Const>(['int', 'bool', 'str']);

export const Const = {
  int: (value: number, kind: IntKind | '?') => int({ value, kind, ty: MonoTy.int(kind) }),
  bool: (value: boolean) => bool({ value, ty: MonoTy.bool() }),
  str: (value: string) => str({ value, ty: MonoTy.str() }),
  show: (c: Const): string => match(c, {
    int: ({ value, kind }) => kind === '?' ? `${value}` : `${value}${kind}`,
    bool: ({ value }) => `${value}`,
    str: ({ value }) => `"${value}"`,
    unit: () => '()',
  }),
  eq: (a: Const, b: Const) => a.variant === b.variant && a.value === b.value,
  isInt: (c: Const) => match(c, {
    int: () => true,
    _: () => false,
  }),
};
