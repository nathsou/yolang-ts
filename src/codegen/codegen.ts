import { Decl, Expr, Prog, Stmt } from "../ast/bitter";
import bn from "binaryen";
import { VariantOf, match as matchVariant } from "itsamatch";
import { matchString, panic } from "../utils/misc";
import { compact } from "../utils/array";
import { MonoTy } from "../infer/types";
import { BinaryOperator, UnaryOperator } from "../ast/sweet";

const createModule = () => new bn.Module();
export type Module = ReturnType<typeof createModule>;

const modulesStack: string[] = [];

const compileDecl = (decl: Decl, m: Module): void => {
  matchVariant(decl, {
    Function: f => {
      compileFunction(f, m);
    },
    Module: mod => {
      modulesStack.push(mod.name);
      for (const decl of mod.decls) {
        compileDecl(decl, m);
      }
      modulesStack.pop();
    },
    _: () => {
      panic('unhandled decl variant: ' + decl.variant);
    },
  });
};

const compileFunction = (f: VariantOf<Decl, 'Function'>, m: Module): void => {
  f.name.renaming = [...modulesStack, f.name.renaming].join('_');

  m.addFunction(
    f.name.renaming,
    bn.createType(f.args.map(arg => MonoTy.wasmRepr(arg.name.ty))),
    MonoTy.wasmRepr(f.body.ty),
    [],
    compileExpr(f.body, m)!
  );

  m.addFunctionExport(f.name.renaming, f.name.renaming);
};

type ExpressionRef = number;

const compileExpr = (expr: Expr, m: Module): ExpressionRef | undefined => {
  return matchVariant(expr, {
    Const: c => matchVariant(c.value, {
      u32: ({ value }) => m.i32.const(value),
      bool: ({ value }) => m.i32.const(value ? 1 : 0),
      unit: () => { },
    }),
    Block: ({ statements }) => {
      return m.block(null, compact(statements.map((stmt, index) => {
        if (index === statements.length - 1 && stmt.variant === 'Expr') {
          return m.return(compileExpr(stmt.expr, m));
        } else {
          return compileStmt(stmt, m);
        }
      })));
    },
    BinaryOp: ({ lhs, op, rhs }) => {
      const opMap: {
        [op in BinaryOperator]: (left: number, right: number) => number
      } = {
        '+': m.i32.add,
        '-': m.i32.sub,
        '*': m.i32.mul,
        '/': m.i32.div_s,
        '%': m.i32.rem_s,
        '==': m.i32.eq,
        '!=': m.i32.ne,
        '<': m.i32.lt_s,
        '<=': m.i32.le_s,
        '>': m.i32.gt_s,
        '>=': m.i32.ge_s,
        "&&": m.i32.and,
        '||': m.i32.or,
      };

      return opMap[op](compileExpr(lhs, m)!, compileExpr(rhs, m)!);
    },
    UnaryOp: ({ op, expr }) => {
      const opMap: {
        [op in UnaryOperator]: (val: number) => number
      } = {
        '-': v => m.i32.sub(m.i32.const(0), v),
        '!': m.i32.eqz,
      };

      return opMap[op](compileExpr(expr, m)!);
    },
    IfThenElse: ({ condition, then, else_ }) => {
      return m.if(
        compileExpr(condition, m)!,
        compileExpr(then, m)!,
        else_.mapWithDefault(br => compileExpr(br, m)!, undefined)
      );
    },
    _: () => { panic(`expr ${expr.variant} not supported yet`) }
  }) ?? undefined;
};

const compileStmt = (stmt: Stmt, m: Module): ExpressionRef | undefined => {
  return matchVariant(stmt, {
    Expr: ({ expr }) => compileExpr(expr, m),
    _: () => { panic(`stmt ${stmt.variant} not supported yet`) }
  }) ?? undefined;
};

export const compile = (prog: Prog): Module => {
  const mod = createModule();

  for (const decl of prog) {
    compileDecl(decl, mod);
  }

  return mod;
};