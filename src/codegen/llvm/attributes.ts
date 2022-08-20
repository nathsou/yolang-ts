import type LLVM from 'llvm-bindings';
import { panic } from "../../utils/misc";

export const meta = (name: string, sig: { args: LLVM.Type[], ret: LLVM.Type }, f: LLVM.Function, builder: LLVM.IRBuilder): LLVM.Value => {
  switch (name) {
    case 'addU64':
    case 'addI64':
    case 'addU32':
    case 'addI32':
      return builder.CreateAdd(f.getArg(0), f.getArg(1));
    case 'subI32':
    case 'subI64':
    case 'subU64':
    case 'subU32':
      return builder.CreateSub(f.getArg(0), f.getArg(1));
    case 'negI64':
    case 'negI32':
      return builder.CreateNeg(f.getArg(0));
    case 'not':
      return builder.CreateNot(f.getArg(0));
    case 'mulI32':
    case 'mulU32':
    case 'mulI64':
    case 'mulU64':
      return builder.CreateMul(f.getArg(0), f.getArg(1));
    case 'divI32':
    case 'divI64':
      return builder.CreateSDiv(f.getArg(0), f.getArg(1));
    case 'divU32':
    case 'divU64':
      return builder.CreateUDiv(f.getArg(0), f.getArg(1));
    case 'modI32':
    case 'modI64':
      return builder.CreateSRem(f.getArg(0), f.getArg(1));
    case 'modU32':
    case 'modU64':
      return builder.CreateURem(f.getArg(0), f.getArg(1));
    case 'logicalAnd':
    case 'bitwiseAndI32':
    case 'bitwiseAndU32':
    case 'bitwiseAndI64':
    case 'bitwiseAndU64':
      return builder.CreateAnd(f.getArg(0), f.getArg(1));
    case 'logicalOr':
    case 'bitwiseOrI32':
    case 'bitwiseOrU32':
    case 'bitwiseOrI64':
    case 'bitwiseOrU64':
      return builder.CreateOr(f.getArg(0), f.getArg(1));
    case 'lssI32':
    case 'lssI64':
      return builder.CreateICmpSLT(f.getArg(0), f.getArg(1));
    case 'lssU32':
    case 'lssU64':
      return builder.CreateICmpULT(f.getArg(0), f.getArg(1));
    case 'gtrI32':
    case 'gtrI64':
      return builder.CreateICmpSGT(f.getArg(0), f.getArg(1));
    case 'gtrU32':
    case 'gtrU64':
      return builder.CreateICmpUGT(f.getArg(0), f.getArg(1));
    case 'leqI32':
    case 'leqI64':
      return builder.CreateICmpSLE(f.getArg(0), f.getArg(1));
    case 'leqU32':
    case 'leqU64':
      return builder.CreateICmpULE(f.getArg(0), f.getArg(1));
    case 'geqI32':
    case 'geqI64':
      return builder.CreateICmpSGE(f.getArg(0), f.getArg(1));
    case 'geqU32':
    case 'geqU64':
      return builder.CreateICmpUGE(f.getArg(0), f.getArg(1));
    case 'eqI32':
    case 'eqI64':
    case 'eqU32':
    case 'eqU64':
      return builder.CreateICmpEQ(f.getArg(0), f.getArg(1));
    case 'neqI32':
    case 'neqI64':
    case 'neqU32':
    case 'neqU64':
      return builder.CreateICmpNE(f.getArg(0), f.getArg(1));
    case 'getUnchecked': {
      const arrayArg = f.getArg(0);
      const indexArg = f.getArg(1);

      const elemPtr = builder.CreateGEP(
        sig.ret,
        arrayArg,
        [indexArg],
      );

      return builder.CreateLoad(sig.ret, elemPtr);
    }
    default:
      return panic(`Unknown meta attribute argument: '${name}'`);
  }
};
