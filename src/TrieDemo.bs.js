// Generated by BUCKLESCRIPT VERSION 5.0.4, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Json_decode = require("@glennsl/bs-json/src/Json_decode.bs.js");
var Trie$ReactHooksTemplate = require("./Trie.bs.js");

var TrieImpl = Trie$ReactHooksTemplate.Make(/* module */[]);

function fetchQuote(dispatch) {
  fetch("https://api.noopschallenge.com/wordbot?count=100").then((function (prim) {
              return prim.json();
            })).then((function (json) {
            var response = /* record */[/* words */Json_decode.field("words", (function (param) {
                      return Json_decode.list(Json_decode.string, param);
                    }), json)];
            Curry._1(dispatch, /* Init */Block.__(1, [response[/* words */0]]));
            return Promise.resolve(/* () */0);
          })).catch((function (param) {
          return Promise.resolve(/* () */0);
        }));
  return /* () */0;
}

function TrieDemo(Props) {
  var match = React.useReducer((function (state, action) {
          if (typeof action === "number") {
            Curry._3(TrieImpl[/* addWord */1], state[/* trie_root */2], state[/* value */1], 0);
            return /* record */[
                    /* string_list */Curry._2(TrieImpl[/* listWords */5], state[/* trie_root */2], ""),
                    /* value */"",
                    /* trie_root */state[/* trie_root */2],
                    /* show_add_word */false
                  ];
          } else if (action.tag) {
            List.map((function (str) {
                    return Curry._3(TrieImpl[/* addWord */1], state[/* trie_root */2], str, 0);
                  }), action[0]);
            return /* record */[
                    /* string_list */Curry._2(TrieImpl[/* listWords */5], state[/* trie_root */2], ""),
                    /* value */state[/* value */1],
                    /* trie_root */state[/* trie_root */2],
                    /* show_add_word */state[/* show_add_word */3]
                  ];
          } else {
            var value = action[0];
            var string_list = Curry._2(TrieImpl[/* listWords */5], state[/* trie_root */2], value);
            return /* record */[
                    /* string_list */string_list,
                    /* value */value,
                    /* trie_root */state[/* trie_root */2],
                    /* show_add_word */List.length(string_list) === 0
                  ];
          }
        }), /* record */[
        /* string_list : [] */0,
        /* value */"",
        /* trie_root */TrieImpl[/* root */0],
        /* show_add_word */false
      ]);
  var dispatch = match[1];
  var state = match[0];
  React.useEffect((function () {
          fetchQuote(dispatch);
          return undefined;
        }), ([]));
  var match$1 = state[/* show_add_word */3];
  return React.createElement("div", {
              className: "container--app"
            }, React.createElement("div", {
                  className: "wrapper--searchbar"
                }, React.createElement("input", {
                      placeholder: "Search Here....",
                      value: state[/* value */1],
                      onChange: (function (_event) {
                          return Curry._1(dispatch, /* Search */Block.__(0, [_event.target.value]));
                        })
                    })), match$1 ? React.createElement("button", {
                    onClick: (function (_event) {
                        return Curry._1(dispatch, /* Add */0);
                      })
                  }, "Add Word") : null, React.createElement("div", {
                  className: "container--word_list"
                }, $$Array.of_list(List.map((function (str) {
                            return React.createElement("div", {
                                        key: str,
                                        className: "wrapper--word"
                                      }, str);
                          }), state[/* string_list */0]))));
}

var make = TrieDemo;

exports.TrieImpl = TrieImpl;
exports.fetchQuote = fetchQuote;
exports.make = make;
/* TrieImpl Not a pure module */