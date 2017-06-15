open Syntax.Ast
(*
type argument =  Syntax.Ast.argument [@@deriving show]
type default_value = Syntax.Ast.default_value [@@deriving show]
type const_type = Syntax.Ast.const_type [@@deriving show]
type const_value = Syntax.Ast.const_value [@@deriving show]
type serializer = Syntax.Ast.serializer [@@deriving show]

type extended_argument = extends * argument [@@deriving show]

and extended_attribute = [
    | `NoArgs of string
    | `ArgumentList of string * (extended_argument list)
    | `NamedArgList of string * string * (extended_argument list)
    | `Ident of string * string
    | `IdentList of string * (string list)
    | `Custom of string
  ] [@@deriving show]

and extends = extended_attribute list

type type_with_ext = extended_attribute list * type_ [@@deriving show]
and type_ = (type_with_ext, return_type, union_type) Syntax.Ast.type_aux [@@deriving show]
and return_type = (type_with_ext, return_type, union_type) Syntax.Ast.return_type_aux [@@deriving show]
and union_type = (type_with_ext, return_type, union_type) Syntax.Ast.union_type_aux [@@deriving show]
and nullable_non_any = type_with_ext Syntax.Ast.nullable_non_any_aux [@@deriving show]
and non_any = (type_with_ext, return_type) Syntax.Ast.non_any_aux [@@deriving show]
*)
type attribute = {
  is_static : bool ;
  is_readonly : bool ;
  is_inherit : bool ;
  type_with_ext : type_with_ext ;
  name : string ;
} [@@deriving show]

type special = [
  | `Getter 
  | `Setter 
  | `Deleter 
  | `Legacycaller 
] [@@deriving show]

type operation = { 
  specials : special list ;
  is_static : bool ;
  return_type : return_type ;
  ident : string option ;
  arguments : (extends * argument) list ;
} [@@deriving show]

type dictionary_member = {
  is_required : bool ;
  type_with_ext : type_with_ext ;
  ident : string ;
  default : default_value option ;
} [@@deriving show]

type dictionary = {
  ident : string ;
  inheritance : string option ;
  dictionary_members : (extends * dictionary_member) list ;
} [@@deriving show]

type operation_or_attribute = [
  | `Operation of operation
  | `Attribute of attribute
] [@@deriving show]

type namespace_member = operation_or_attribute [@@deriving show]

type namespace = {
  ident : string ;
  namespace_members : (extends * namespace_member) list ;
} [@@deriving show]

type pattern_list = [
  | `Getter
  | `Identifiers of string list
  | `None
] [@@deriving show]

type maplike = {
  is_readonly : bool ; 
  key_type : type_with_ext ; 
  value_type : type_with_ext ;
} [@@deriving show]

type setlike = {
  is_readonly : bool ; 
  key_type : type_with_ext ; 
} [@@deriving show]

type interface_member = [
  | `Const of const_type * string * const_value
  | `Operation of operation
  | `Serializer of serializer
  | `Stringifier of [ operation_or_attribute | `None ]
  | `Iterable of type_with_ext * (type_with_ext option) 
  | `Attribute of attribute
  | `Maplike of maplike 
  | `Setlike of setlike
] [@@deriving show]

type interface = {
  ident : string ;
  inheritance : string option ;
  interface_members : (extends * interface_member) list;
} [@@deriving show]

type partial = [
  | `Interface of interface
  | `Dictionary of dictionary
  | `Namespace of namespace
] [@@deriving show]

type callback = [
 | `Operation of operation
 | `Interface of interface
] [@@deriving show]

type definition = [
  | `Callback of callback
  | `Interface of interface
  | `Namespace of namespace
  | `Partial of partial
  | `Dictionary of dictionary
  | `Enum of string * (string list)
  | `Typedef of type_with_ext * string
  | `Implements of string * string
] [@@deriving show]

type definitions = (extends * definition) list [@@deriving show]
