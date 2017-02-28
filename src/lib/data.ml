(* convert ast to OCaml simplified data *)

type primitive = [
  | `Boolean 
  | `Byte 
  | `Octet 
  | `Unrestricted of [ `Float | `Double ]
  | `Float 
  | `Double 
  | `Unsigned of [ `Short | `Long | `Long_long ]
  | `Short 
  | `Long 
  | `Long_long 
] [@@deriving show]

type string_type = [
  | `Bytestring 
  | `Domstring 
  | `Usvstring
] [@@deriving show]

type buffer = [
  | `Arraybuffer 
  | `Dataview 
  | `Int8array 
  | `Int16array 
  | `Int32array 
  | `Uint8array 
  | `Uint16array 
  | `Uint32array 
  | `Uint8clampedarray 
  | `Float32array 
  | `Float64array 
] [@@deriving show]

type 'types nullable = [
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

type 'types null = [`Nullable of 'types nullable] [@@deriving show]

type ('types, 'return_type) nonany_aux = [
  | 'types nullable
  | 'types null
  | `Promise of 'return_type
] [@@deriving show]

type  ('types, 'return_type) types_aux = [
    |  ('types, 'return_type) nonany_aux
    | `Any
    | `Union of  ('types, 'return_type) nonany_aux list
] [@@deriving show]

type  ('types, 'return_type) return_type_aux = [
    | `Void
    |  ('types, 'return_type) types_aux
] [@@deriving show]

type types = (types, return_type) types_aux [@@deriving show]
and return_type = (types, return_type) return_type_aux [@@deriving show]

type const_value = [
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Null
] [@@deriving show]

type const_type = [
  | primitive
  | `Ident of string
]  [@@deriving show]

type const = const_type null * string * const_value [@@deriving show]

type default_value = [
  | `Const of const_value
  | `String of string
  | `Empty_sequence
] [@@deriving show]

type necessity = [
  | `Optional of default_value option 
  | `Required of [`Variadic | `Fixed]
] [@@deriving show]

type argument = {
  extended_attributes : extended_attribute list ;
  type_ : types ;
  name : Ast.Argument.argument_name ;
  necessity : necessity
} [@@deriving show]

and extended_attribute = [
    | `Argument_list of string * (argument list)
    | `Ident_list of string * (string list)
    | `Named_arg_list of string * string * (argument list)
  ] [@@deriving show]

type attribute = {
  extended_attributes : extended_attribute list ;
  specifiers : [ `Static | `Readonly | `Inherit ] list ;
  type_ : types ;
  name : Ast.Attribute.attribute_name ;
} [@@deriving show]

type operation = { 
  extended_attributes : extended_attribute list ;
  specifiers : [ `Static | Ast.Operation.special ] list ;
  type_ : return_type ;
  ident : string option ;
  arguments : argument list ;
} [@@deriving show]

type dictionary_member = {
  extended_attributes : extended_attribute list ;
  is_required : bool ;
  type_ : types ;
  ident : string ;
  default : default_value option ;
} [@@deriving show]

type dictionary = {
  ident : string ;
  inheritance : string option ;
  dictionary_members : dictionary_member list;
} [@@deriving show]

type operation_or_attribute = [
  | `Operation of operation
  | `Attribute of attribute
] [@@deriving show]

type namespace_member = operation_or_attribute [@@deriving show]

type namespace = {
  ident : string ;
  namespace_members : namespace_member list;
} [@@deriving show]

type serializer = [
  | `Operation of operation
  | `Pattern_map of Ast.Interface.pattern_map
  | `Pattern_list of Ast.Interface.pattern_list
  | `Ident of string
  | `None
] [@@deriving show]

type interface_member = [
  | `Const of const
  | `Operation  of operation
  | `Serializer of serializer
  | `Stringifier of [ operation_or_attribute | `None ]
  | `Iterable of types * (types option) * (extended_attribute list)
  | `Attribute of attribute
  | `Maplike of types * types * (extended_attribute list)
  | `Setlike of types * (extended_attribute list)
] [@@deriving show]

type interface = {
  ident : string ;
  inheritance : string option ;
  interface_members : interface_member list;
} [@@deriving show]

type partial = [
  | `Interface of interface
  | `Dictionary of dictionary
  | `Namespace of namespace
] [@@deriving show]

type definition = [
  | `Callback of string * return_type * (argument list)
  | `Callback_interface of interface
  | `Interface of interface
  | `Namespace of namespace
  | `Partial of partial
  | `Dictionary of dictionary
  | `Enum of extended_attribute * string * (string list)
  | `Typedef of extended_attribute * types * string
  | `Implements of extended_attribute * string * string
] [@@deriving show]
