
let () =
  try 
    Webidl.from_file Sys.argv.(1)
    |> Ast.show_ast
    |> print_string
  with
  | Webidl.Syntax_error e ->
    print_string "[!]Syntax Error\nInfo:\n";
    Webidl.show_syntax_error e
    |> print_string
    |> print_newline
