open Argument

type extended_attribute = [
    | `No_args of string
    | `Argument_list of string * (argument with_extAttr list)
    | `Ident of string * string
    | `Ident_list of string * (string list)
    | `Named_arg_list of string * string * (argument with_extAttr list)
] [@@deriving show]

and 'a with_extAttr = {
  extAttr : extended_attribute list;
  value : 'a;
} [@@deriving show]

type 'a members = {
  ident : string ;
  members : 'a with_extAttr list;
} [@@deriving show]

type 'a members_with_inherit = {
  ident : string ;
  inheritance : string option ;
  members : 'a with_extAttr list;
} [@@deriving show]