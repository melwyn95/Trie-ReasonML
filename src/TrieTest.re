module Trie = Trie.Make();

Js.log(Trie.root)

Trie.addWord(Trie.root, "abcd", 0);

Js.log(Trie.listAllWords(Trie.root, [], "a"))