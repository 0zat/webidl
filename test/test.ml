open Test_data

let test = function
  | [
    (int_ext, `Interface stringifier);
    ([], `Interface serializer);
    ([], `Interface attribute);
    ([], `Interface operation);
    ([], `Partial (`Interface  others));
    ([], `Namespace namespace);
    ([], `Dictionary dictionary);
    ([], `Enum enum);
    ([], `Callback callback);
    ([], `Callback_interface ext);
    ([], `Typedef typedef);
    ([], `Implements implements)
  ]
    ->
    assert(int_ext = check_int_ext);
    assert(stringifier = check_stringifier);
    assert(serializer = check_serializer);
    assert(attribute = check_attribute);
    assert(operation = check_operation);
    assert(others = check_others);
    assert(namespace = check_namespace);
    assert(dictionary = check_dictionary);
    assert(enum = check_enum);
    assert(callback = check_callback);
    assert(ext = check_ext);
    assert(typedef = check_typedef);
    assert(implements = check_implements)
  | _ -> failwith "unexpected idl structure"

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
  Webidl.Parse.data_from_file "test.idl" |> test;
  test_lexerror ();
  test_parseerror ();
  print_string "test finish\n"


