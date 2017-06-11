
let test_lexerror () =
  try
    let _ = Webidl.Parse.data_from_file "lex_error.idl" in ()
  with
  | Webidl.Parse.Syntax_error e ->
    assert( 
      e = { Webidl.Parse.src = "lex_error.idl"; src_type = Webidl.Parse.File; start_pos = (17, 3);
            end_pos = (17, 4); token = "!" }
    )

let test_parseerror () =
  try
    let _ = Webidl.Parse.data_from_file "parse_error.idl" in ()
  with
  | Webidl.Parse.Syntax_error e ->
    assert( 
      e = { Webidl.Parse.src = "parse_error.idl"; src_type = Webidl.Parse.File; start_pos = (6, 18);
            end_pos = (6, 27); token = "DOMString" }
    )


let () =
  try
    Webidl.Parse.data_from_file Sys.argv.(1) (* 1. *)
    |> Webidl.Parse.show_data (* 2. *)
    |> print_string
  with
  | Webidl.Parse.Syntax_error e -> (* 3. *)
    print_string "[!]Syntax Error\nInfo:\n";
    Webidl.Parse.show_syntax_error e (* 4. *)
    |> print_string
    |> print_newline
