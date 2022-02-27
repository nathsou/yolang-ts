import { gen } from "../../utils/array";
import { Byte, Vec } from "./types"

export const uleb128 = (n: number): Byte[] => {
  const bytes: Byte[] = [];

  do {
    let byte = n & 0x7f;
    n >>= 7;
    if (n !== 0) {
      byte = byte | 0x80;
    }
    bytes.push(byte);
  } while (n != 0);

  return bytes;
};

export const sleb128 = (n: number): Byte[] => {
  const bytes: Byte[] = [];
  n |= 0;

  while (true) {
    const byte = n & 0x7f;
    n >>= 7;
    if (
      (n === 0 && (byte & 0x40) === 0) ||
      (n === -1 && (byte & 0x40) !== 0)
    ) {
      bytes.push(byte);
      return bytes;
    }

    bytes.push(byte | 0x80);
  }
};

export const encodeStr = (str: string): Byte[] => Vec.encode(gen(str.length, i => str.charCodeAt(i)));