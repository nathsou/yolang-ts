import { VariantOf } from "itsamatch";
import { Decl } from "../ast/bitter";

export type Trait = VariantOf<Decl, 'Trait'>;