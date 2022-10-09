import type LLVM from 'llvm-bindings';
import { Maybe, none, some } from '../../utils/maybe';
import { panic } from "../../utils/misc";

type ExternFuns = { [Fn in 'printf']: LLVM.Function };

export const meta = (
  name: string,
  f: LLVM.Function,
  builder: LLVM.IRBuilder,
  llvm: typeof LLVM,
  context: LLVM.LLVMContext,
  externFuns: ExternFuns,
): Maybe<LLVM.Value> => {
  const printf = (format: string, arg: LLVM.Value): Maybe<LLVM.Value> => {
    const fmt = builder.CreateGlobalStringPtr(format);
    builder.CreateCall(externFuns.printf, [fmt, arg]);
    return none;
  };

  switch (name) {
    case 'addInt':
      return some(builder.CreateAdd(f.getArg(0), f.getArg(1)));
    case 'addFloat':
      return some(builder.CreateFAdd(f.getArg(0), f.getArg(1)));
    case 'subInt':
      return some(builder.CreateSub(f.getArg(0), f.getArg(1)));
    case 'subFloat':
      return some(builder.CreateFSub(f.getArg(0), f.getArg(1)));
    case 'negInt':
      return some(builder.CreateNeg(f.getArg(0)));
    case 'negFloat':
      return some(builder.CreateFNeg(f.getArg(0)));
    case 'mulInt':
      return some(builder.CreateMul(f.getArg(0), f.getArg(1)));
    case 'mulFloat':
      return some(builder.CreateFMul(f.getArg(0), f.getArg(1)));
    case 'signedDiv':
      return some(builder.CreateSDiv(f.getArg(0), f.getArg(1)));
    case 'unsignedDiv':
      return some(builder.CreateUDiv(f.getArg(0), f.getArg(1)));
    case 'divFloat':
      return some(builder.CreateFDiv(f.getArg(0), f.getArg(1)));
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
    case 'lssFloat':
      return some(builder.CreateFCmpOLT(f.getArg(0), f.getArg(1)));
    case 'signedGtr':
      return some(builder.CreateICmpSGT(f.getArg(0), f.getArg(1)));
    case 'unsignedGtr':
      return some(builder.CreateICmpUGT(f.getArg(0), f.getArg(1)));
    case 'gtrFloat':
      return some(builder.CreateFCmpOGT(f.getArg(0), f.getArg(1)));
    case 'signedLeq':
      return some(builder.CreateICmpSLE(f.getArg(0), f.getArg(1)));
    case 'unsignedLeq':
      return some(builder.CreateICmpULE(f.getArg(0), f.getArg(1)));
    case 'leqFloat':
      return some(builder.CreateFCmpOLE(f.getArg(0), f.getArg(1)));
    case 'signedGeq':
      return some(builder.CreateICmpSGE(f.getArg(0), f.getArg(1)));
    case 'unsignedGeq':
      return some(builder.CreateICmpUGE(f.getArg(0), f.getArg(1)));
    case 'geqFloat':
      return some(builder.CreateFCmpOGE(f.getArg(0), f.getArg(1)));
    case 'eqInt':
      return some(builder.CreateICmpEQ(f.getArg(0), f.getArg(1)));
    case 'eqFloat':
      return some(builder.CreateFCmpOEQ(f.getArg(0), f.getArg(1)));
    case 'neqInt':
      return some(builder.CreateICmpNE(f.getArg(0), f.getArg(1)));
    case 'neqFloat':
      return some(builder.CreateFCmpONE(f.getArg(0), f.getArg(1)));
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
    case 'toF32':
      return some(builder.CreateFPCast(f.getArg(0), llvm.Type.getFloatTy(context)));
    case 'toF64':
      return some(builder.CreateFPCast(f.getArg(0), llvm.Type.getDoubleTy(context)));
    case 'printU32': {
      return printf("%u\n", f.getArg(0));
    }
    case 'printU64': {
      return printf("%lu\n", f.getArg(0));
    }
    case 'printI32': {
      return printf("%i\n", f.getArg(0));
    }
    case 'printI64': {
      return printf("%li\n", f.getArg(0));
    }
    case 'printF32': {
      const argF64 = builder.CreateFPCast(f.getArg(0), llvm.Type.getDoubleTy(context));
      return printf("%.9g\n", argF64);
    }
    case 'printF64': {
      return printf("%.17g\n", f.getArg(0));
    }
    case 'printCString': {
      return printf("%s\n", f.getArg(0));
    }
    default:
      return panic(`Unknown meta attribute argument: '${name}'`);
  }
};
