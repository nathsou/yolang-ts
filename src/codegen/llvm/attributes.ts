import type LLVM from 'llvm-bindings';
import { Maybe, none, some } from '../../utils/maybe';
import { panic } from "../../utils/misc";

export const meta = (
  name: string,
  f: LLVM.Function,
  builder: LLVM.IRBuilder,
  llvm: typeof LLVM,
  context: LLVM.LLVMContext,
): Maybe<LLVM.Value> => {
  switch (name) {
    case 'addU64':
    case 'addI64':
    case 'addU32':
    case 'addI32':
      return some(builder.CreateAdd(f.getArg(0), f.getArg(1)));
    case 'subI32':
    case 'subI64':
    case 'subU64':
    case 'subU32':
      return some(builder.CreateSub(f.getArg(0), f.getArg(1)));
    case 'negI64':
    case 'negI32':
      return some(builder.CreateNeg(f.getArg(0)));
    case 'not':
      return some(builder.CreateNot(f.getArg(0)));
    case 'mulI32':
    case 'mulU32':
    case 'mulI64':
    case 'mulU64':
      return some(builder.CreateMul(f.getArg(0), f.getArg(1)));
    case 'divI32':
    case 'divI64':
      return some(builder.CreateSDiv(f.getArg(0), f.getArg(1)));
    case 'divU32':
    case 'divU64':
      return some(builder.CreateUDiv(f.getArg(0), f.getArg(1)));
    case 'modI32':
    case 'modI64':
      return some(builder.CreateSRem(f.getArg(0), f.getArg(1)));
    case 'modU32':
    case 'modU64':
      return some(builder.CreateURem(f.getArg(0), f.getArg(1)));
    case 'logicalAnd':
    case 'bitwiseAndI32':
    case 'bitwiseAndU32':
    case 'bitwiseAndI64':
    case 'bitwiseAndU64':
      return some(builder.CreateAnd(f.getArg(0), f.getArg(1)));
    case 'logicalOr':
    case 'bitwiseOrI32':
    case 'bitwiseOrU32':
    case 'bitwiseOrI64':
    case 'bitwiseOrU64':
      return some(builder.CreateOr(f.getArg(0), f.getArg(1)));
    case 'lssI32':
    case 'lssI64':
      return some(builder.CreateICmpSLT(f.getArg(0), f.getArg(1)));
    case 'lssU32':
    case 'lssU64':
      return some(builder.CreateICmpULT(f.getArg(0), f.getArg(1)));
    case 'gtrI32':
    case 'gtrI64':
      return some(builder.CreateICmpSGT(f.getArg(0), f.getArg(1)));
    case 'gtrU32':
    case 'gtrU64':
      return some(builder.CreateICmpUGT(f.getArg(0), f.getArg(1)));
    case 'leqI32':
    case 'leqI64':
      return some(builder.CreateICmpSLE(f.getArg(0), f.getArg(1)));
    case 'leqU32':
    case 'leqU64':
      return some(builder.CreateICmpULE(f.getArg(0), f.getArg(1)));
    case 'geqI32':
    case 'geqI64':
      return some(builder.CreateICmpSGE(f.getArg(0), f.getArg(1)));
    case 'geqU32':
    case 'geqU64':
      return some(builder.CreateICmpUGE(f.getArg(0), f.getArg(1)));
    case 'eqI32':
    case 'eqI64':
    case 'eqU32':
    case 'eqU64':
      return some(builder.CreateICmpEQ(f.getArg(0), f.getArg(1)));
    case 'neqI32':
    case 'neqI64':
    case 'neqU32':
    case 'neqU64':
      return some(builder.CreateICmpNE(f.getArg(0), f.getArg(1)));
    case 'getUnchecked': {
      const arrayArg = f.getArg(0);
      const indexArg = f.getArg(1);
      const elemTy = f.getReturnType();

      const elemPtr = builder.CreateGEP(
        elemTy,
        arrayArg,
        [indexArg],
      );

      return some(builder.CreateLoad(elemTy, elemPtr));
    }
    case 'setUnchecked': {
      const arrayArg = f.getArg(0);
      const indexArg = f.getArg(1);
      const valueArg = f.getArg(2);

      const elemPtr = builder.CreateGEP(
        valueArg.getType(),
        arrayArg,
        [indexArg],
      );

      builder.CreateStore(valueArg, elemPtr);
      return none;
    }
    case 'u8FromInt':
      return some(builder.CreateIntCast(f.getArg(0), llvm.Type.getInt8Ty(context), true));
    case 'u8FromUInt':
      return some(builder.CreateIntCast(f.getArg(0), llvm.Type.getInt8Ty(context), false));
    default:
      return panic(`Unknown meta attribute argument: '${name}'`);
  }
};
