
export type Ref<T> = { ref: T };

export const ref = <T>(data: T): Ref<T> => ({ ref: data });

export const panic = (msg: string): never => {
  throw new Error(msg);
};

export const unreachable = (msg: string): never => {
  return panic(`unreachable code was reached: ${msg}`);
};

export const fst = <A, B>([a, _]: [A, B]): A => a;
export const snd = <A, B>([_, b]: [A, B]): B => b;

export const cond = <T>(
  condition: boolean,
  branches: { then: () => T, else: () => T }
): T => {
  return condition ? branches.then() : branches.else();
};

export const id = <T>(x: T): T => x;

export const noop = () => { };

// projection
export const proj = <T, K extends keyof T>(key: K): (data: T) => T[K] => {
  return (data: T) => data[key];
};

export const matchString = <S extends string, T>(
  str: S,
  cases: ({ [str in S]: () => T }) | (Partial<{ [str in S]: () => T }> & { _: () => T })
): T => {
  /// @ts-ignore
  return cases[str in cases ? str : '_']();
};

export const parenthesized = (str: string, showParens = true): string => showParens ? `(${str})` : str;

export const mapRecord = <T, U>(obj: Record<string, T>, f: (value: T) => U): Record<string, U> => {
  return Object.fromEntries(Object.entries(obj).map(([key, value]) => [key, f(value)]));
};

export const pushRecord = <T>(obj: Record<string, T[]>, key: string, value: T): void => {
  if (key in obj) {
    obj[key].push(value);
  } else {
    obj[key] = [value];
  }
};

export const compose = <A, B, C>(f: (a: A) => B, g: (b: B) => C) => (x: A) => g(f(x));

export const not = (q: boolean) => !q;

export const forEach = <T>(it: Iterable<T>, f: (value: T) => void): void => {
  for (const value of it) {
    f(value);
  }
};

export function assert(test: boolean, message: (string | (() => string)) = ''): asserts test {
  if (!test) {
    throw new Error(`assertion failed: ${typeof message === 'string' ? message : message()}`);
  }
}

export const block = <T>(f: () => T): T => f();
