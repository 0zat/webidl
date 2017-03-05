open Types
open Argument

type attribute_name = [
  | `Ident of string
  | `Required
] [@@deriving show]

type attribute = types * attribute_name [@@deriving show]

type read_only_attribute = {
  is_readonly : bool ;
  attribute : attribute ;
} [@@deriving show]