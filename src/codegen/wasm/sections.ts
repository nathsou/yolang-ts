import { DataType, match } from "itsamatch";
import { joinWith, last } from "../../utils/array";
import { ident, panic, proj } from "../../utils/misc";
import { Inst } from "./instructions";
import { Byte, FuncIdx, FuncType, LocalIdx, TypeIdx, ValueType, Vec } from "./types";
import { encodeStr, uleb128 } from "./utils";

const SECTION_TYPE_ID = {
  custom: 0,
  type: 1,
  import: 2,
  function: 3,
  table: 4,
  memory: 5,
  global: 6,
  export: 7,
  start: 8,
  element: 9,
  code: 10,
  data: 11,
};

export type SectionType = keyof typeof SECTION_TYPE_ID;

export const Section = {
  id: (type: SectionType): number => SECTION_TYPE_ID[type],
  encode: (type: SectionType, bytes: Byte[]): Vec => [Section.id(type), ...Vec.encode(bytes)],
  show: (type: SectionType, body: string): string => `[${type} section]\n${body}`,
};

// The Type Section consists of a list of function signatures.
export type TypeSection = {
  funcSignatures: Map<string, { ty: FuncType, idx: TypeIdx }>,
};

export const TypeSection = {
  make: (): TypeSection => ({ funcSignatures: new Map() }),
  getOrAdd: ({ funcSignatures }: TypeSection, ty: FuncType): TypeIdx => {
    const key = FuncType.show(ty);

    if (funcSignatures.has(key)) {
      return funcSignatures.get(key)!.idx;
    }

    const idx = funcSignatures.size;
    funcSignatures.set(key, { ty, idx });

    return idx;
  },
  encode: ({ funcSignatures }: TypeSection): Vec => {
    const sigs = [...funcSignatures.values()];
    return Section.encode('type', Vec.encodeMany(sigs.map(s => FuncType.encode(s.ty))));
  },
  show: ({ funcSignatures }: TypeSection): string => Section.show(
    'type',
    [...funcSignatures.keys()].join('\n')
  ),
};

export type FuncSection = { name: string, ty: TypeIdx }[];

export const FuncSection = {
  make: (): FuncSection => [],
  addFuncIdx: (self: FuncSection, name: string, ty: TypeIdx) => {
    self.push({ name, ty });
  },
  encode: (self: FuncSection): Vec => Section.encode(
    'function',
    Vec.encodeMany(self.map(({ ty }) => TypeIdx.encode(ty)))
  ),
  show: (self: FuncSection): string => Section.show(
    'function',
    joinWith(self, ({ name, ty }) => `${name}: ${ty}`, '\n')
  ),
};

export type ExportDesc = DataType<{
  Func: { idx: FuncIdx },
}>;

export const ExportDesc = {
  Func: (idx: FuncIdx): ExportDesc => ({ variant: 'Func', idx }),
  encode: (desc: ExportDesc): Vec => match(desc, {
    Func: ({ idx }) => [0, ...FuncIdx.encode(idx)],
  }),
  show: (desc: ExportDesc): string => match(desc, {
    Func: ({ idx }) => `func ${idx}`,
  }),
};

type ExportEntry = { name: string, desc: ExportDesc };
const ExportEntry = {
  encode: ({ name, desc }: ExportEntry): Vec => [
    ...encodeStr(name),
    ...ExportDesc.encode(desc),
  ],
};

export type ExportSection = ExportEntry[];

export const ExportSection = {
  make: (): ExportSection => [],
  add: (self: ExportSection, name: string, desc: ExportDesc): void => {
    self.push({ name, desc });
  },
  encode: (self: ExportSection): Vec => Section.encode(
    'export',
    Vec.encodeMany(self.map(ExportEntry.encode))
  ),
  show: (self: ExportSection): string => Section.show(
    'export',
    joinWith(self, ({ name, desc }) => `export ${name}: ${ExportDesc.show(desc)}`, '\n')
  ),
};

// run-length encoding
export type Locals = { names: string[], ty: ValueType }[];

export const Locals = {
  make: (): Locals => [],
  from: (locals: { name: string, ty: ValueType }[]): Locals => {
    let lastTy: ValueType | null = null;
    const sequences: Locals = [];

    for (const { name, ty } of locals) {
      if (lastTy === ty) {
        last(sequences).names.push(name);
      } else {
        sequences.push({ names: [name], ty });
        lastTy = ty;
      }
    }

    return sequences;
  },
  encode: (locals: Locals): Vec => Vec.encodeMany(
    locals.map(({ names, ty }) => [...uleb128(names.length), ...ValueType.encode(ty)])
  ),
  show: (locals: Locals): string => joinWith(locals, ({ names, ty }) => `${names.join(', ')}: ${ty}`, ', '),
  get: (locals: Locals, index: LocalIdx): string => {
    let i = 0;
    for (const { names } of locals) {
      for (const name of names) {
        if (i === index) {
          return name;
        }

        i += 1;
      }
    }

    return panic(`local index ${index} out of range`);
  },
};

type CodeEntry = {
  funcName: string,
  locals: Locals,
  instructions: Inst[],
};

const CodeEntry = {
  make: (funcName: string, locals: Locals, instructions: Inst[]): CodeEntry => ({ funcName, locals, instructions }),
  encode: ({ locals, instructions }: CodeEntry): Vec => Vec.encode([
    ...Locals.encode(locals),
    ...instructions.flatMap(Inst.encode),
    ...Inst.encode(Inst.end()),
  ]),
  showInstructions: (insts: Inst[], locals: Locals, funcNames: string[]): string => {
    let identation = 1;
    const res: string[] = [];

    insts.forEach(inst => {
      const { before, after } = Inst.identationDelta(inst);
      identation += before;
      res.push(ident(Inst.show(inst, locals, funcNames), identation));
      identation += after;
    });

    return res.join('\n') + '\nend\n';
  },
  show: ({ funcName, locals, instructions }: CodeEntry, funcNames: string[]): string => {
    return `fn ${funcName}(${Locals.show(locals)})\n${CodeEntry.showInstructions(instructions, locals, funcNames)}`;
  },
};

export type CodeSection = CodeEntry[];

export const CodeSection = {
  make: (): CodeSection => [],
  add: (self: CodeSection, funcName: string, locals: Locals, instructions: Inst[]): FuncIdx => {
    self.push(CodeEntry.make(funcName, locals, instructions));
    return self.length - 1;
  },
  encode: (self: CodeSection): Vec => Section.encode(
    'code',
    Vec.encodeMany(self.map(CodeEntry.encode))
  ),
  show: (self: CodeSection, funcNames: string[]): string => Section.show(
    'code',
    joinWith(self, c => CodeEntry.show(c, funcNames), '\n')
  ),
};

export type Module = {
  sections: {
    type: TypeSection,
    function: FuncSection,
    export: ExportSection,
    code: CodeSection,
  },
};

export const Module = {
  make: (): Module => ({
    sections: {
      type: TypeSection.make(),
      function: FuncSection.make(),
      export: ExportSection.make(),
      code: CodeSection.make(),
    },
  }),
  addFunction: (
    self: Module,
    name: string,
    ty: FuncType,
    locals: Locals,
    instructions: Inst[],
    isExported = false
  ): FuncIdx => {
    const typeIdx = TypeSection.getOrAdd(self.sections.type, ty);
    FuncSection.addFuncIdx(self.sections.function, name, typeIdx);
    const funcIdx = CodeSection.add(self.sections.code, name, locals, instructions);

    if (isExported) {
      ExportSection.add(self.sections.export, name, ExportDesc.Func(funcIdx));
    }

    return funcIdx;
  },
  encode: (self: Module): Vec => [
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, // magic cookie "\0asm" and wasm version
    ...TypeSection.encode(self.sections.type),
    ...FuncSection.encode(self.sections.function),
    ...ExportSection.encode(self.sections.export),
    ...CodeSection.encode(self.sections.code),
  ],
  encodeUin8Array: (self: Module): Uint8Array => new Uint8Array(Module.encode(self)),
  show: (self: Module): string => {
    const funcNames = self.sections.function.map(proj('name'));

    return [
      TypeSection.show(self.sections.type),
      FuncSection.show(self.sections.function),
      ExportSection.show(self.sections.export),
      CodeSection.show(self.sections.code, funcNames),
    ].join('\n\n');
  },
};