open Types
open Argument
open Extended

type special = [
  | `Getter 
  | `Setter 
  | `Deleter 
  | `Legacycaller 
] [@@deriving show]

type operation = { 
  specials : special list;
  type_ : return_type;
  ident : string option;
  arguments : argument with_extAttr list;
} [@@deriving show]