import { Expr } from "../../ast/bitter";
import { MonoTy } from "../../infer/types";
import { findRev } from "../../utils/array";
import { Maybe } from "../../utils/maybe";
import { Locals } from "./sections";
import { FuncIdx, LocalIdx } from "./types";
import { wasmTy } from "./utils";

type LocalVar = {
  name: string,
  ty: MonoTy,
  mutable: boolean,
  scopeDepth: number,
  index: number,
};

export type Func = {
  name: string,
  locals: LocalVar[],
  body: Expr,
  scopeDepth: number,
  index: FuncIdx,
};

export const Func = {
  make: (name: string, args: { name: string, ty: MonoTy, mutable: boolean }[], body: Expr, index: FuncIdx): Func => ({
    name,
    locals: args.map((a, index) => ({ ...a, index, scopeDepth: 1 })),
    body,
    scopeDepth: 1,
    index,
  }),
  declareVar: (self: Func, name: string, ty: MonoTy, mutable: boolean): LocalIdx => {
    self.locals.push({ name, ty, mutable, scopeDepth: self.scopeDepth, index: self.locals.length });
    return self.locals.length - 1;
  },
  scoped: (self: Func, f: () => void): void => {
    self.scopeDepth += 1;
    f();
    self.scopeDepth -= 1;
    self.locals = self.locals.filter(({ scopeDepth }) => scopeDepth <= self.scopeDepth);
  },
  resolveVar: (self: Func, name: string): Maybe<LocalIdx> => {
    return findRev(self.locals, ({ name: n }) => n === name).map(({ index }) => index);
  },
  locals: (self: Func): Locals => {
    return Locals.from(self.locals.flatMap(({ name, ty }) => ({ name, ty: wasmTy(ty) })));
  },
};