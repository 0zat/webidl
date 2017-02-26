open Types
open Argument

type attribute_name = [
  | `Ident of string
  | `Required
] [@@deriving show]

type attribute = types * attribute_name [@@deriving show]

type read_only_attribute = {
  type_ : types ;
  name : attribute_name ;
  is_read_only : bool ;
} [@@deriving show]