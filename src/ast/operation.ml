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
  arguments : string option * (argument with_extAttr list);
} [@@deriving show]