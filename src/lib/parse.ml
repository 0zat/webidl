type ast = Ast.Definition.definitions [@@deriving show]

type syntax_error = {
  src : string ;
  start_pos : int * int ;
  end_pos : int * int ;
  string : string ;
} [@@deriving show]

exception Syntax_error of syntax_error

let pos_to_error src lexbuf =
  let open Lexing in
  let start = Lexing.lexeme_start_p lexbuf in
  let end_ = Lexing.lexeme_end_p lexbuf in
  let string = String.sub lexbuf.lex_buffer start.pos_cnum (end_.pos_cnum - start.pos_cnum) in
  let start_pos = (start.pos_lnum, (start.pos_cnum - start.pos_bol + 1)) in
  let end_pos = (end_.pos_lnum, (end_.pos_cnum - end_.pos_bol + 1)) in
  {src; start_pos; end_pos; string}

let main ?(trace = false) src_name lexbuf =
  let _ = Parsing.set_trace trace in
  try
    Syntax.Parser.main Syntax.Lexer.read lexbuf 
  with
  | Syntax.Parser.Error ->
    let syntax_error = pos_to_error src_name lexbuf in
    raise (Syntax_error syntax_error)

let from_string src_name input_string =
  let lexbuf = Lexing.from_string input_string in
  main src_name lexbuf

let from_channel src_name input_channel =
  let lexbuf = Lexing.from_channel input_channel in
  main src_name lexbuf

let from_file file_name =
  let input_channel = open_in file_name in
  let src_name = Printf.sprintf "file(%s)" file_name in
  from_channel src_name input_channel
