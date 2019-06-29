module ChildrenMap =
  Map.Make({
    type t = char;
    let compare = compare;
  });

type node = {
  value: char,
  mutable isWord: bool,
  mutable children: ChildrenMap.t(node),
};

let dummy_node = {value: ' ', isWord: false, children: ChildrenMap.empty};

module type TrieType = {
  let root: node;
  let addWord: (node, string, int) => unit;
  let findWord: (node, string, int) => bool;
  let deleteWord: (node, string, int) => unit;
  let listAllWords: (node, list(string), string) => list(string);
  let listWords: (node, string) => list(string);
};

module Make = (()) : TrieType => {
  let root = {value: ' ', isWord: false, children: ChildrenMap.empty};
  let rec addWord = (root, word, index) =>
    if (index == String.length(word)) {
      ();
    } else {
      let childNode =
        try (ChildrenMap.find(word.[index], root.children)) {
        | Not_found =>
          let newNode = {
            value: word.[index],
            isWord: false,
            children: ChildrenMap.empty,
          };
          root.children =
            ChildrenMap.add(word.[index], newNode, root.children);
          newNode;
        };
      if (index == String.length(word) - 1) {
        childNode.isWord = true;
      };
      addWord(childNode, word, index + 1);
    };
  let rec findWord = (root, word, index): bool =>
    if (index == String.length(word) && root.isWord) {
      true;
    } else if (index == String.length(word)) {
      false;
    } else {
      let childNode =
        try (ChildrenMap.find(word.[index], root.children)) {
        | Not_found => dummy_node
        };
      if (childNode.value == ' ') {
        false;
      } else {
        findWord(childNode, word, index + 1);
      };
    };

  let rec listAllWords = (root, lst, str) => {
    let retLst = ref(lst);
    if (root.isWord) {
      retLst := List.append(lst, [str]);
    };
    ChildrenMap.iter(
      (ch, node) => {
        retLst := listAllWords(node, retLst^, str ++ String.make(1, ch));
      },
      root.children,
    );
    retLst^;
  };

  let rec deleteWord = (root, word, index) =>
    if (index == String.length(word)) {
      ();
    } else {
      let childNode =
        try (ChildrenMap.find(word.[index], root.children)) {
        | Not_found => dummy_node
        };
      if (childNode.isWord && index === String.length(word) - 1) {
        childNode.isWord = false;
        if (ChildrenMap.cardinal(childNode.children) === 0) {
          root.children = ChildrenMap.remove(word.[index], root.children);
          ();
        };
      } else {
        deleteWord(childNode, word, index + 1);
      };
    };

  let rec findPrefixRoot =
          (root: node, str: string, index: int): option(node) => {
    Js.logMany([|str, string_of_int(index)|]);
    if (index == String.length(str)) {
      Some(root);
    } else {
      let childNode =
        try (ChildrenMap.find(str.[index], root.children)) {
        | Not_found => dummy_node
        };

      if (childNode.value == ' ') {
        None;
      } else if (index === String.length(str) - 1) {
        Some(childNode);
      } else {
        findPrefixRoot(childNode, str, index + 1);
      };
    };
  };

  let listWords = (root: node, str: string): list(string) => {
    let optionRoot = findPrefixRoot(root, str, 0);
    switch (optionRoot) {
    | None => []
    | Some(prefixRoot) => listAllWords(prefixRoot, [], str)
    };
  };
};