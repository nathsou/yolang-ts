import { Maybe, None, Some } from "./maybe";

type Char = string;

export type Node<K> = {
  children: Record<Char, Node<K>>,
  value: Maybe<K>,
};

export class Trie<K> {
  private root: Node<K>;

  constructor() {
    this.root = {
      children: {},
      value: None,
    };
  }

  static from<K>(entries: [string, K][]): Trie<K> {
    const trie = new Trie<K>();

    for (const [key, value] of entries) {
      trie.insert(key, value);
    }

    return trie;
  }

  static fromStrings(strings: readonly string[]): Trie<string> {
    return Trie.from(strings.map(s => [s, s]));
  }

  insert(key: string, value: K): Trie<K> {
    let node = this.root;

    for (const char of key) {
      if (!(char in node.children)) {
        node.children[char] = {
          children: {},
          value: None,
        };
      }

      node = node.children[char];
    }

    node.value = Some(value);

    return this;
  }

  find(key: string): Maybe<K> {
    let node = this.root;

    for (const char of key) {
      if (!(char in node.children)) {
        return None;
      }

      node = node.children[char];
    }

    return node.value;
  }

  static step<K>(node: Node<K>, key: string): Maybe<Node<K>> {
    for (const char of key) {
      if (!(char in node.children)) {
        return None;
      }

      node = node.children[char];
    }

    return Some(node);
  }

  getRoot(): Node<K> {
    return this.root;
  }
}