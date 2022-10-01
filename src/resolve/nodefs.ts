import { FileSystem } from "./fileSystem";

export const createNodeFileSystem = async (): Promise<FileSystem> => {
  const fs = await import('fs/promises');
  const path = await import('path');

  return {
    readDir: async path => {
      const files = await fs.readdir(path);
      return await Promise.all(
        files.map(async (file) => {
          const stats = await fs.stat(`${path}/${file}`);
          return {
            name: file,
            type: stats.isDirectory() ? 'dir' : 'file',
          };
        })
      );
    },
    resolve: path.resolve,
    relative: path.relative,
    readFile: async path => await fs.readFile(path, 'utf8'),
    parentDir: path.dirname,
    stdPath: __dirname + '/..',
  };
};