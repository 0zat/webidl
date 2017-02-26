{
    open Lexing
    open Parser
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
  | "unsigned" { UNSIGNED }
  | "byte" { BYTE }
  | "octet" { OCTET } 
  | "short" { SHORT }
  | "long" { LONG }
  | "DOMString" { DOMSTRING }
  | "USVString" { USVSTRING }
  | "ByteString" { BYTESTRING }
  | "unrestricted" { UNRESTRICTED }
  | "float" { FLOAT }
  | "double" { DOUBLE }
  | "any" { ANY }
  | "void" { VOID }
  | "boolean" { BOOLEAN }
  | "object" { OBJECT }
  | "or" { OR }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | "Infinity" { INFINITY }
  | "-Infinity" { MINUSINFINITY }
  | "NaN" { NAN }
  | "setter" { SETTER }
  | "getter" { GETTER }
  | "deleter" { DELETER }
  | "legacycaller" { LEGACYCALLER }
  | "serializer" { SERIALIZER }
  | "stringifier" { STRINGIFIER }
  | "?" { QUESTION }
  | "=" { EQUAL }
  | "," { COMMA }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "..." { ELLIPSIS }
  | "maplike" { MAPLIKE }
  | "setlike" { SETLIKE }
  | "iterable" { ITERABLE }
  | "Promise" { PROMISE }
  | "record" { RECORD }
  | "sequence" { SEQUENCE }
  | "namespace" { NAMESPACE }
  | "callback" { CALLBACK }
  | "partial" { PARTIAL }
  | "interface" { INTERFACE }
  | "dictionary" { DICTIONARY }
  | "enum" { ENUM }
  | "implements" { IMPLEMENTS }
  | "inherit" { INHERIT }
  | "attribute" { ATTRIBUTE }
  | "typedef" { TYPEDEF }
  | "const" { CONST }
  | "readonly" { READONLY }
  | "required" { REQUIRED }
  | "static" { STATIC }
  | "optional" { OPTIONAL }
  | "DOMException" { DOMEXCEPTION }
  | "Error" { ERROR_ }
  | "Int8Array" { INT8ARRAY }
  | "Int16Array" { INT16ARRAY }
  | "Int32Array" { INT32ARRAY }
  | "Uint8Array"  { UINT8ARRAY }
  | "Uint16Array" { UINT16ARRAY }
  | "Uint32Array" { UINT32ARRAY }
  | "Uint8ClampedArray" { UINT8CLAMPEDARRAY}
  | "Float32Array" { FLOAT32ARRAY }
  | "Float64Array" { FLOAT64ARRAY } 
  | "ArrayBuffer" { ARRAYBUFFER }
  | "DataView" { DATAVIEW }
  | "FrozenArray" { FROZENARRAY }
  | '"' { read_string (Buffer.create 10) lexbuf }
  | int { INTVAL (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOATVAL (float_of_string (Lexing.lexeme lexbuf)) } 
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
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