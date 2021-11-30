import { lex } from "./parse/lex";

const res = lex(`
  fn main() {
    let letinfnif = 1 in x + 17;
  }
`);

console.log(res.unwrap());