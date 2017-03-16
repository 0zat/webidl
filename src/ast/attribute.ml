open Types
open Argument

type attribute = types * string [@@deriving show]

type read_only_attribute = {
  is_readonly : bool ;
  attribute : attribute ;
} [@@deriving show]