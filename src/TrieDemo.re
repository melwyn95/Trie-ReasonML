/* State declaration */
type state = {
  string_list: list(string),
  value: string,
  trie_root: Trie.node,
  show_add_word: bool,
};

/* Action declaration */
type action =
  | Search(string)
  | Add
  | Init(list(string));

module TrieImpl =
  Trie.Make({});

type api_response = {words: list(string)};

let fetchQuote = dispatch => {
  ignore(
    Js.Promise.(
      Fetch.fetch("https://api.noopschallenge.com/wordbot?count=100")
      |> then_(Fetch.Response.json)
      |> then_(json => {
           let response: api_response =
             Json.Decode.{words: field("words", list(string), json)};
           dispatch(Init(response.words));
           Js.Promise.resolve();
         })
      |> catch(_ => Js.Promise.resolve())
    ),
  );
};

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Search(value) =>
          let string_list = TrieImpl.listWords(state.trie_root, value);
          {
            ...state,
            value,
            string_list,
            show_add_word: List.length(string_list) == 0,
          };
        | Add =>
          TrieImpl.addWord(state.trie_root, state.value, 0);
          {
            ...state,
            value: "",
            string_list: TrieImpl.listWords(state.trie_root, ""),
            show_add_word: false,
          };
        | Init(initial_list) =>
          ignore(
            List.map(
              str => TrieImpl.addWord(state.trie_root, str, 0),
              initial_list,
            ),
          );
          {...state, string_list: TrieImpl.listWords(state.trie_root, "")};
        },
      {
        trie_root: TrieImpl.root,
        string_list: [],
        value: "",
        show_add_word: false,
      },
    );

  React.useEffect0(() => {
    fetchQuote(dispatch);
    None;
  });

  <div className="container--app">
    <div className="wrapper--searchbar">
      <input
        placeholder="Search Here...."
        value={state.value}
        onChange={_event =>
          dispatch(Search(ReactEvent.Form.target(_event)##value))
        }
      />
    </div>
    {state.show_add_word
       ? <button onClick={_event => dispatch(Add)}>
           {ReasonReact.string("Add Word")}
         </button>
       : ReasonReact.null}
    <div className="container--word_list">
      {ReasonReact.array(
         Array.of_list(
           List.map(
             str =>
               <div key=str className="wrapper--word">
                 {ReasonReact.string(str)}
               </div>,
             state.string_list,
           ),
         ),
       )}
    </div>
  </div>;
};