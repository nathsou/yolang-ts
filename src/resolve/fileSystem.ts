
export type File = {
  name: string,
  type: 'file' | 'dir',
};

export type FileSystem = {
  readDir: (path: string) => Promise<File[]>,
  readFile: (path: string) => Promise<string>,
  resolve: (...paths: string[]) => string,
  stdPath: string,
};