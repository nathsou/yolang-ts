
export type File = {
  name: string,
  type: 'file' | 'dir',
};

export type FileSystem = {
  readDir: (path: string) => Promise<File[]>,
  readFile: (path: string) => Promise<string>,
  directory: (path: string) => Promise<string>,
  stdPath: string,
};