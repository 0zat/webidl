open Types
open Extended

type partial = [
  | `Interface of Interface.interface_member members
  | `Dictionary of Dictionary.dictionary_member members
  | `Namespace of Namespace.namespace
] [@@deriving show]

type definition = [
  | `Callback of string * return_type * (Argument.argument with_extAttr list)
  | `Callback_interface of Interface.interface
  | `Interface of Interface.interface
  | `Namespace of Namespace.namespace
  | `Partial of partial
  | `Dictionary of Dictionary.dictionary
  | `Enum of string * (string list)
  | `Typedef of types * string
  | `Implements of string * string
] [@@deriving show]

type definitions = definition with_extAttr list [@@deriving show]
