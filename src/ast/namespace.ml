open Types
open Argument
open Extended
open Attribute

type namespace_member = [
  | `Operation of return_type * string option * (argument with_extAttr list)
  | `Attribute of read_only_attribute
] [@@deriving show]

type namespace = namespace_member members [@@deriving show]