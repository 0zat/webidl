open Types

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