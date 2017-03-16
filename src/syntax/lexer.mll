{
    open Lexing
    open Parser
    open Keyword

    let token_table = Hashtbl.create 63

    let () =
      let open Parser in
      List.iter (fun (key, token) -> Hashtbl.add token_table key token)
        [  
          unsigned , UNSIGNED ;
          byte , BYTE ;
          octet , OCTET ; 
          short , SHORT ;
          long , LONG ;
          domstring , DOMSTRING ;
          usvstring , USVSTRING ;
          bytestring , BYTESTRING ;
          unrestricted , UNRESTRICTED ;
          float , FLOAT ;
          double , DOUBLE ;
          any , ANY ;
          void , VOID ;
          boolean , BOOLEAN ;
          object_ , OBJECT ;
          or_ , OR ;
          true_ , TRUE ;
          false_ , FALSE ;
          null , NULL ;
          infinity_ , INFINITY ;
          nan_ , NAN ;
          setter , SETTER ;
          getter , GETTER ;
          deleter , DELETER ;
          legacycaller , LEGACYCALLER ;
          serializer , SERIALIZER ;
          stringifier , STRINGIFIER ;
          maplike , MAPLIKE ;
          setlike , SETLIKE ;
          iterable , ITERABLE ;
          promise , PROMISE ;
          record , RECORD ;
          sequence , SEQUENCE ;
          namespace , NAMESPACE ;
          callback , CALLBACK ;
          partial , PARTIAL ;
          interface , INTERFACE ;
          dictionary , DICTIONARY ;
          enum , ENUM ;
          implements , IMPLEMENTS ;
          inherit_ , INHERIT ;
          attribute , ATTRIBUTE ;
          typedef , TYPEDEF ;
          const , CONST ;
          readonly , READONLY ;
          required , REQUIRED ;
          static , STATIC ;
          optional , OPTIONAL ;
          domexception , DOMEXCEPTION ;
          error , ERROR_ ;
          int8array , INT8ARRAY ;
          int16array , INT16ARRAY ;
          int32array , INT32ARRAY ;
          uint8array  , UINT8ARRAY ;
          uint16array , UINT16ARRAY ;
          uint32array , UINT32ARRAY ;
          uint8clampedarray , UINT8CLAMPEDARRAY;
          float32array , FLOAT32ARRAY ;
          float64array , FLOAT64ARRAY ; 
          arraybuffer , ARRAYBUFFER ;
          dataview , DATAVIEW ;
          frozenarray , FROZENARRAY ;
        ]
}

let decdigit = ['0'-'9']
let hexdigit = ['0'-'9' 'A' - 'F' 'a' - 'F']
let octdigit = ['0'-'7']
let decint = '-'? ['1'-'9'] decdigit*
let hexint = '-'? '0' ['X' 'x'] hexdigit+
let octint = '0' octdigit*
let int = decint | hexint | octint
let float_no_exp = decdigit+ '.' decdigit* | '.' decdigit+
let float_exp_part = ['E' 'e'] ['+' '-']? decdigit+
let float = '-'? (float_no_exp float_exp_part? | decdigit+ float_exp_part)
let identifier = ['A'-'Z' 'a'-'z' '_'] ['0'-'9' 'A'-'Z' 'a'-'z' '_']*
let spaces = ['\t''\r'' ']+

rule read = parse
  | '#' identifier { skip_to_eol lexbuf }
  | '\n' { new_line lexbuf; read lexbuf }
  | spaces { read lexbuf }
  | '/' '*' { skip_comment lexbuf }
  | '/' '/' { skip_to_eol lexbuf }
  | "(" { LPAR }
  | ")" { RPAR }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "<" { LT }
  | ">" { GT }
  | "-Infinity" { MINUSINFINITY }
  | "?" { QUESTION }
  | "=" { EQUAL }
  | "," { COMMA }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "..." { ELLIPSIS }
  | '"' { read_string (Buffer.create 10) lexbuf }
  | int { INTVAL (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOATVAL (float_of_string (Lexing.lexeme lexbuf)) } 
  | identifier as id { 
      try
        Hashtbl.find token_table id
      with
      | Not_found -> IDENTIFIER id
    }
  | eof { EOF }
  | _  { raise Parser.Error }

and read_string buf = parse
  | '\n' { new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '"' { STRING (Buffer.contents buf) }
  | [^'"''\n']* { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | eof { failwith "Unterminated string" }

and skip_comment = parse
  | '\n' { new_line lexbuf; skip_comment lexbuf }
  | "*/" { read lexbuf }
  | '*' [^'/''\n'] { skip_comment lexbuf }
  | '*' '\n' { new_line lexbuf; skip_comment lexbuf }
  | [^'*''\n']* { skip_comment lexbuf }
  | eof { failwith "Unterminated comment" }

and skip_to_eol = parse
  | '\n' { new_line lexbuf; read lexbuf }
  | eof { EOF }
  | [^'\n']* { skip_to_eol lexbuf }