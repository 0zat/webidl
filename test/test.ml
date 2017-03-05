
let () =
  try 
    Webidl.Parse.data_from_file Sys.argv.(1)
    |> Webidl.Parse.show_data
    |> print_string
  with
  | Webidl.Parse.Syntax_error e ->
    print_string "[!]Syntax Error\nInfo:\n";
    Webidl.Parse.show_syntax_error e
    |> print_string
    |> print_newline

