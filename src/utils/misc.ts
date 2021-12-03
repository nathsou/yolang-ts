
export type Ref<T> = { ref: T };

export const ref = <T>(data: T): Ref<T> => ({ ref: data });

export const panic = (msg: string): never => {
  throw new Error(msg);
};

export const unreachable = (msg: string): never => {
  return panic(`unreachable code was reached: ${msg}`);
};

export const fst = <A, B>([a, _]: [A, B]): A => a;
export const snd = <A, B>([_, b]: [A, B]): B => b;