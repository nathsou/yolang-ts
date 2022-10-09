import type LLVM from 'llvm-bindings';
import { Maybe, none, some } from '../../utils/maybe';
import { panic } from "../../utils/misc";

export const meta = (
  name: string,
  f: LLVM.Function,
  builder: LLVM.IRBuilder,
  llvm: typeof LLVM,
  context: LLVM.LLVMContext,
  externFuns: { printf: LLVM.Function },
): Maybe<LLVM.Value> => {
  switch (name) {
    case '+':
      return some(builder.CreateAdd(f.getArg(0), f.getArg(1)));
    case 'sub':
      return some(builder.CreateSub(f.getArg(0), f.getArg(1)));
    case 'neg':
      return some(builder.CreateNeg(f.getArg(0)));
    case '*':
      return some(builder.CreateMul(f.getArg(0), f.getArg(1)));
    case 'signedDiv':
      return some(builder.CreateSDiv(f.getArg(0), f.getArg(1)));
    case 'unsignedDiv':
      return some(builder.CreateUDiv(f.getArg(0), f.getArg(1)));
    case 'mod':
      return some(builder.CreateURem(f.getArg(0), f.getArg(1)));
    case '&':
      return some(builder.CreateAnd(f.getArg(0), f.getArg(1)));
    case '|':
      return some(builder.CreateOr(f.getArg(0), f.getArg(1)));
    case '^':
      return some(builder.CreateXor(f.getArg(0), f.getArg(1)));
    case '<<':
      return some(builder.CreateShl(f.getArg(0), f.getArg(1)));
    case 'arithmeticShiftRight':
      return some(builder.CreateAShr(f.getArg(0), f.getArg(1)));
    case 'logicalShiftRight':
      return some(builder.CreateLShr(f.getArg(0), f.getArg(1)));
    case '~':
      return some(builder.CreateNot(f.getArg(0)));
    case 'signedLss':
      return some(builder.CreateICmpSLT(f.getArg(0), f.getArg(1)));
    case 'unsignedLss':
      return some(builder.CreateICmpULT(f.getArg(0), f.getArg(1)));
    case 'signedGtr':
      return some(builder.CreateICmpSGT(f.getArg(0), f.getArg(1)));
    case 'unsignedGtr':
      return some(builder.CreateICmpUGT(f.getArg(0), f.getArg(1)));
    case 'signedLeq':
      return some(builder.CreateICmpSLE(f.getArg(0), f.getArg(1)));
    case 'unsignedLeq':
      return some(builder.CreateICmpULE(f.getArg(0), f.getArg(1)));
    case 'signedGeq':
      return some(builder.CreateICmpSGE(f.getArg(0), f.getArg(1)));
    case 'unsignedGeq':
      return some(builder.CreateICmpUGE(f.getArg(0), f.getArg(1)));
    case '==':
      return some(builder.CreateICmpEQ(f.getArg(0), f.getArg(1)));
    case '!=':
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
    case 'u64FromFloat':
      return some(builder.CreateFPCast(f.getArg(0), llvm.Type.getDoubleTy(context)));
    case 'printUInt': {
      const fmt = builder.CreateGlobalStringPtr('%u\n');
      builder.CreateCall(externFuns.printf, [fmt, f.getArg(0)])
      return none;
    }
    case 'printInt': {
      const fmt = builder.CreateGlobalStringPtr('%i\n');
      builder.CreateCall(externFuns.printf, [fmt, f.getArg(0)])
      return none;
    }
    case 'printFloat': {
      const fmt = builder.CreateGlobalStringPtr('%f\n');
      const argF64 = builder.CreateFPCast(f.getArg(0), llvm.Type.getDoubleTy(context));
      builder.CreateCall(externFuns.printf, [fmt, argF64])
      return none;
    }
    case 'printCString': {
      const fmt = builder.CreateGlobalStringPtr('%s\n');
      builder.CreateCall(externFuns.printf, [fmt, f.getArg(0)])
      return none;
    }
    default:
      return panic(`Unknown meta attribute argument: '${name}'`);
  }
};
