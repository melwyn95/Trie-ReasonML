// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Trie$ReactHooksTemplate = require("./Trie.bs.js");

var Trie = Trie$ReactHooksTemplate.Make(/* module */[]);

console.log(Trie[/* root */0]);

Curry._3(Trie[/* addWord */1], Trie[/* root */0], "abcd", 0);

console.log(Curry._3(Trie[/* listAllWords */4], Trie[/* root */0], /* [] */0, "a"));

exports.Trie = Trie;
/* Trie Not a pure module */
