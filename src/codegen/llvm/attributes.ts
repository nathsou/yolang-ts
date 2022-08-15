import type LLVM from 'llvm-bindings';
import { panic } from "../../utils/misc";

export const meta = (name: string, f: LLVM.Function, builder: LLVM.IRBuilder): LLVM.Value => {
  switch (name) {
    case 'addU64':
    case 'addI64':
    case 'addU32':
    case 'addI32':
      return builder.CreateAdd(f.getArg(0), f.getArg(1));
    default:
      return panic(`Invalid meta attribute argument: '${name}'`);
  }
};
