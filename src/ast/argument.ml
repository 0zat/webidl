open Types
open Const

type optional_argument = {
  type_ : types ; 
  name : string ; 
  default : default_value option
} [@@deriving show]

type required_argument = {
  type_ : types ;
  name : string ;
  is_variadic : bool ;
} [@@deriving show]

type argument = [
  | `Optional of optional_argument
  | `Required of required_argument
] [@@deriving show]