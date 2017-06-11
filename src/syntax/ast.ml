type primitive = [
  | `Boolean 
  | `Byte 
  | `Octet 
  | `Unrestricted of [ `Float | `Double ]
  | `Float 
  | `Double 
  | `Unsigned of [ `Short | `Long | `LongLong ]
  | `Short 
  | `Long 
  | `LongLong 
] [@@deriving show]

type string_type = [
  | `ByteString 
  | `DOMString 
  | `USVString
] [@@deriving show]

type buffer = [
  | `ArrayBuffer 
  | `DataView 
  | `Int8Array 
  | `Int16Array 
  | `Int32Array 
  | `Uint8Array 
  | `Uint16Array 
  | `Uint32Array 
  | `Uint8Clampedarray 
  | `Float32Array 
  | `Float64Array 
] [@@deriving show]

type nullable_non_any = [
  | primitive
  | string_type
  | `Ident of string
  | `Sequence of type_with_ext
  | `Object
  | `Error
  | `DomException
  | buffer
  | `FrozenArray of type_with_ext
  | `Record of string_type * type_with_ext
] [@@deriving show]

and nullable_union = [
    | primitive
    | string_type
    | `Ident of string
    | `Sequence of type_with_ext
    | `Object
    | `Error
    | `DomException
    | buffer
    | `FrozenArray of type_with_ext
    | `Record of string_type * type_with_ext
    | `Union of union_type
  ] [@@deriving show]

and non_any = [
    | `Promise of return_type
    | `Nullable of nullable_non_any
    | primitive
    | string_type
    | `Ident of string
    | `Sequence of type_with_ext
    | `Object
    | `Error
    | `DomException
    | buffer
    | `FrozenArray of type_with_ext
    | `Record of string_type * type_with_ext
  ] [@@deriving show]

and single_type =  [
    | `Promise of return_type
    | `Nullable of nullable_non_any
    | primitive
    | string_type
    | `Ident of string
    | `Sequence of type_with_ext
    | `Object
    | `Error
    | `DomException
    | buffer
    | `FrozenArray of type_with_ext
    | `Record of string_type * type_with_ext
    | `Any
  ] [@@deriving show]

and type_with_ext = extends * type_ [@@deriving show]

and union_member = [
    | `NonAny of extends * non_any
    | `Union of union_type
    | `Nullable of [`Union of union_type]
  ] [@@deriving show]

and union_type =  union_member list [@@deriving show]

and type_ = [
    | `Promise of return_type
    | `Nullable of nullable_union
    | primitive
    | string_type
    | `Ident of string
    | `Sequence of type_with_ext
    | `Object
    | `Error
    | `DomException
    | buffer
    | `FrozenArray of type_with_ext
    | `Record of string_type * type_with_ext
    | `Any
    | `Union of union_type
  ] [@@deriving show]

and return_type = [
    | `Promise of return_type
    | `Nullable of nullable_union
    | primitive
    | string_type
    | `Ident of string
    | `Sequence of type_with_ext
    | `Object
    | `Error
    | `DomException
    | buffer
    | `FrozenArray of type_with_ext
    | `Record of string_type * type_with_ext
    | `Any
    | `Union of union_type
    | `Void
  ] [@@deriving show]

and const_value = [
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `Null
  ] [@@deriving show]

and const = [
    | primitive
    | `Ident of string
  ]  [@@deriving show]

and default_value = [
    | `Const of const_value
    | `String of string
    | `EmptySequence
  ] [@@deriving show]

and argument = [
    | `Optional of type_with_ext * string * default_value option 
    | `Variadic of type_ * string 
    | `Fixed of type_ * string 
  ] [@@deriving show]

and extended_argument = extended_attribute list * argument [@@deriving show]

and extended_attribute = [
    | `NoArgs of string
    | `ArgumentList of string * (extended_argument list)
    | `NamedArgList of string * string * (extended_argument list)
    | `Ident of string * string
    | `IdentList of string * (string list)
  ] [@@deriving show]

and extends = extended_attribute list [@@deriving show]

type special = [
  | `Getter 
  | `Setter 
  | `Deleter 
  | `Legacycaller 
] [@@deriving show]

type operation_rest = string option * ((extends * argument) list) [@@deriving show]

type no_special_operation = [ `NoSpecialOperation of return_type * operation_rest] [@@deriving show]

type operation = [
  | no_special_operation
  | `SpecialOperation of special list * return_type * operation_rest
] [@@deriving show]

type dictionary_member = [
  | `Required of type_with_ext * string * default_value option
  | `NotRequired of type_ * string * default_value option
] [@@deriving show]

type dictionary = string * string option * (extends * dictionary_member) list
[@@deriving show]

type attribute_rest = [`AttributeRest of type_with_ext * string] [@@deriving show]

type operation_or_attribute = [
  | no_special_operation
  | attribute_rest
] [@@deriving show]

type namespace_member = [
  | no_special_operation
  | `ReadOnly of attribute_rest
] [@@deriving show]

type namespace = string * (extends * namespace_member) list
[@@deriving show]

type pattern_list = [
  | `Getter
  | `Identifiers of string list
  | `None
] [@@deriving show]

type serializer = [
  | `OperationRest of operation_rest
  | `PatternMap of [ pattern_list | `Inherit of string list]
  | `PatternList of pattern_list
  | `Ident of string
  | `None
] [@@deriving show]

type maplike = type_with_ext * type_with_ext [@@deriving show]

type setlike = type_with_ext [@@deriving show]

type const_type = [const | `Nullable of const] [@@deriving show]

type static_member = [
  | operation_or_attribute
  | `ReadOnly of attribute_rest
] [@@deriving show]

type readonly_member = [
  | `Maplike of maplike 
  | `Setlike of setlike
  | attribute_rest
] [@@deriving show]

type attribute = [
  | `Inherit of attribute_rest
  | attribute_rest
] [@@deriving show]

type interface_member = [
  | `ReadOnly of readonly_member
  | `Static of static_member
  | `Const of const_type * string * const_value
  | `Operation of operation
  | `Serializer of serializer
  | `Stringifier of [ static_member| `None ]
  | `Iterable of type_with_ext * (type_with_ext option) 
  | `Attribute of attribute
  | `Maplike of maplike 
  | `Setlike of setlike
] [@@deriving show]

type interface = string * string option * (extends * interface_member) list 
[@@deriving show]

type partial = [
  | `PartialInterface of string * (extends * interface_member) list
  | `PartialDictionary of string * (extends * dictionary_member) list
  | `Namespace of namespace
] [@@deriving show]

type callback = [
  | `CallbackRest of string * return_type * ((extends * argument) list)
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