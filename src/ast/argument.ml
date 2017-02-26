open Types
open Const


type argument_name_keyword = [
  | `Attribute   
  | `Callback   
  | `Const   
  | `Deleter   
  | `Dictionary   
  | `Enum   
  | `Getter   
  | `Implements   
  | `Inherit   
  | `Interface   
  | `Iterable   
  | `Legacycaller   
  | `Maplike   
  | `Namespace   
  | `Partial   
  | `Required   
  | `Serializer   
  | `Setlike   
  | `Setter   
  | `Static   
  | `Stringifier   
  | `Typedef   
  | `Unrestricted 
] [@@deriving show]

type argument_name = [
  | argument_name_keyword
  | `Ident of string
] [@@deriving show]

type optional_argument = {
  type_ : types ; 
  name : argument_name ; 
  default : default_value option
} [@@deriving show]

type required_argument = {
  type_ : types ;
  name : argument_name ;
  is_variadic : bool ;
} [@@deriving show]

type argument = [
  | `Optional of optional_argument
  | `Required of required_argument
] [@@deriving show]