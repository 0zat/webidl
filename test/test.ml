let () =
  try
    Webidl.Parse.data_from_file ~strict:false Sys.argv.(1) (* 1. *)
    |> Webidl.Parse.show_data (* 2. *)
    |> print_string
  with
  | Webidl.Parse.Syntax_error e -> (* 3. *)
    print_string "[!]Syntax Error\nInfo:\n";
    Webidl.Parse.show_syntax_error e (* 4. *)
    |> print_string
    |> print_newline
