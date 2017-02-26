open Types
open Const
open Extended

type dictionary_member = {
  is_required : bool ;
  type_ : types ;
  ident : string ;
  default : default_value option ;
} [@@deriving show]

type dictionary = dictionary_member members_with_inherit [@@deriving show]