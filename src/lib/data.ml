(* simplified data of ast *)

type primitive = Ast.primitive [@@deriving show]

type string_type = Ast.string_type [@@deriving show]

type buffer = Ast.buffer [@@deriving show]

type ('types, 'return_type) nullable_aux = [
  | primitive
  | string_type
  | buffer
  | `Ident of string
  | `Object
  | `Error
  | `Domexception
  | `Sequence of 'types
  | `Frozen_array of 'types
  | `Record of string_type * 'types
] [@@deriving show]

(* inside nullable *)
type ('types, 'return_type) null_aux = [
  | ('types, 'return_type) nullable_aux 
  | `Union of ('types, 'return_type) non_any_aux list
]

and ('types, 'return_type) non_any_aux = [
  | `Promise of 'return_type
  | ('types, 'return_type) nullable_aux
  | `Nullable of ('types, 'return_type) null_aux
  | `Union of ('types, 'return_type) non_any_aux list
] [@@deriving show]

type ('types, 'return_type) types_aux = [
  |  ('types, 'return_type) non_any_aux
  | `Any
] [@@deriving show]

type  ('types, 'return_type) return_type_aux = [
  | `Void
  |  ('types, 'return_type) types_aux
] [@@deriving show]

type types = (types, return_type) types_aux [@@deriving show]
and return_type = (types, return_type) return_type_aux [@@deriving show]

type non_any = (types, return_type) non_any_aux [@@deriving show]
type null = (types, return_type) null_aux [@@deriving show]

type const_value = Ast.const_value [@@deriving show]

type const_type = Ast.const_type [@@deriving show]

type default_value = [
  | `Const of const_value
  | `String of string
  | `Empty_sequence
] [@@deriving show]

type necessity = [
  | `Optional of default_value option 
  | `Required of [`Variadic | `Fixed]
] [@@deriving show]

type argument_name = Ast.argument_name [@@deriving show]

type argument = {
  type_ : types ;
  name : argument_name ;
  necessity : necessity
} [@@deriving show]

type extended_attribute = [
  | `Argument_list of string * ((extended_attribute list * argument) list)
  | `Ident_list of string * (string list)
  | `Named_arg_list of string * string * ((extended_attribute list * argument) list)
] [@@deriving show]

type extends = extended_attribute list [@@deriving show]

type attribute = {
  is_static : bool ;
  is_readonly : bool ;
  is_inherit : bool ;
  type_ : types ;
  name : [ `Ident of string | `Required ] ;
} [@@deriving show]

type special = Ast.special [@@deriving show]

type operation = { 
  specials : special list ;
  is_static : bool ;
  type_ : return_type ;
  ident : string option ;
  arguments : (extends * argument) list ;
} [@@deriving show]

type dictionary_member = {
  is_required : bool ;
  type_ : types ;
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

type pattern_list = Ast.pattern_list [@@deriving show]

type serializer = [
  | `Operation of string option * ((extends * argument) list)
  | `Pattern_map of [ pattern_list | `Inherit of string list]
  | `Pattern_list of pattern_list
  | `Ident of string
  | `None
] [@@deriving show]

type maplike = {
  is_readonly : bool ; 
  key_type : types ; 
  value_type : types ;
} [@@deriving show]

type setlike = {
  is_readonly : bool ; 
  key_type : types ; 
} [@@deriving show]

type interface_member = [
  | `Const of [const_type | `Nullable of const_type] * string * const_value
  | `Operation of operation
  | `Serializer of serializer
  | `Stringifier of [ operation_or_attribute | `None ]
  | `Iterable of types * (types option) 
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

type definition = [
  | `Callback of string * return_type * ((extends * argument) list)
  | `Callback_interface of interface
  | `Interface of interface
  | `Namespace of namespace
  | `Partial of partial
  | `Dictionary of dictionary
  | `Enum of string * (string list)
  | `Typedef of types * string
  | `Implements of string * string
] [@@deriving show]

type definitions = (extends * definition) list [@@deriving show]