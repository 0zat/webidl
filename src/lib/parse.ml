type data = Data.definitions [@@deriving show]

type src_type = 
  | File
  | Channel
  | String
[@@deriving show]

type syntax_error = {
  src : string ;
  src_type : src_type ;
  start_pos : int * int ;
  end_pos : int * int ;
  token : string ;
  strict : bool ;
  around : string ;
} [@@deriving show]

exception Syntax_error of syntax_error

let get_around get_substr sp ep =
  let around_sp = max (sp - 20) 0 in
  let around_ep = (ep + 20) in
  let around = 
    try
      get_substr around_sp around_ep 
    with
    | _ -> get_substr around_sp ep 
  in
  "..." ^ around ^ "..." 

let get_error_info strict src src_type get_substr lexbuf =
  let open Lexing in
  let start = Lexing.lexeme_start_p lexbuf in
  let end_ = Lexing.lexeme_end_p lexbuf in
  let token = Lexing.lexeme lexbuf in
  let start_pos = (start.pos_lnum, (start.pos_cnum - start.pos_bol + 1)) in
  let end_pos = (end_.pos_lnum, (end_.pos_cnum - end_.pos_bol + 1)) in
  let around = get_around get_substr start.pos_cnum end_.pos_cnum in 
  { src; src_type; start_pos; end_pos; token; strict; around }

let main ?(trace = false) ?(strict = true) src src_type lexbuf get_substr =
  let _ = Parsing.set_trace trace in
  let module Strict = struct let strict = strict end in
  let module Parser_extend = Syntax.Parser_extend.Make(Strict) in
  let module Param = struct 
    let main sp ep =
      let ext = get_substr sp ep in
      let lexbuf = Lexing.from_string ext in
      try 
        Parser_extend.ext_main Syntax.Lexer.read lexbuf
      with
      | Parser_extend.Error
      | Parsing.Parse_error -> `Custom ext
  end 
  in
  let module Parser = Syntax.Parser.Make(Strict)(Param) in
  try
    Parser.main Syntax.Lexer.read lexbuf 
  with
  | Parser.Error
  | Parsing.Parse_error ->
    let syntax_error = get_error_info strict src src_type get_substr lexbuf in
    raise (Syntax_error syntax_error)

let ast_from_string ?(strict = true) src_name input_string =
  let get_substr sp ep = String.sub input_string sp (ep - sp) in
  let lexbuf = Lexing.from_string input_string in
  main ~strict src_name String lexbuf get_substr

let ast_from_channel ?(strict = true) src_name input_channel =
  let input_string = really_input_string input_channel (in_channel_length input_channel) in
  let get_substr sp ep = String.sub input_string sp (ep - sp) in
  let lexbuf = Lexing.from_string input_string in
  main ~strict src_name Channel lexbuf get_substr

let ast_from_file ?(strict = true) file_name =
  let input_channel = open_in file_name in
  let lexbuf = Lexing.from_channel input_channel in
  let get_substr sp ep =
    let now_pos = pos_in input_channel in
    seek_in input_channel sp;
    let ans = really_input_string input_channel (ep - sp) in
    seek_in input_channel now_pos;
    ans
  in
  main ~strict file_name File lexbuf get_substr

let data_from_string ?(strict = true) src_name input_string =
  ast_from_string ~strict src_name input_string
  |> Ast_to_data.of_difinitions

let data_from_channel ?(strict = true) src_name input_channel =
  ast_from_channel ~strict src_name input_channel
  |> Ast_to_data.of_difinitions

let data_from_file ?(strict = true) file_name =
  ast_from_file ~strict file_name 
  |> Ast_to_data.of_difinitions
