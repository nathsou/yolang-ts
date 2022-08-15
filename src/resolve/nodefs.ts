import { FileSystem } from "./fileSystem";

export const createNodeFileSystem = async (): Promise<FileSystem> => {
  const fs = await import('fs/promises');

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
    readFile: async path => await fs.readFile(path, 'utf8'),
    directory: async path => {
      const { dirname } = await import('path');
      if ((await fs.stat(path)).isDirectory()) {
        return path;
      } else {
        return dirname(path);
      }
    },
    stdPath: __dirname + '/..',
  };
};