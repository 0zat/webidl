open Types
open Const
open Argument
open Attribute
open Extended
open Operation

type pattern_list = [
  | `Getter
  | `Identifiers of string list
  | `None
] [@@deriving show]

type pattern_map = [
  | pattern_list
  | `Inherit of string list
] [@@deriving show]

type serializer = [
  | `Operation of string option * (argument with_extAttr list)
  | `Pattern_map of pattern_map
  | `Pattern_list of pattern_list
  | `Ident of string
  | `None
] [@@deriving show]

type static_member = [
  | `Attribute of read_only_attribute
  | `Operation of return_type * (string option * argument with_extAttr list)
] [@@deriving show]

type stringifier = [
  | static_member
  | `None
] [@@deriving show]

type iterable = types * (types option) [@@deriving show]

type maplike = types * types [@@deriving show]

type setlike = types [@@deriving show]

type read_only_member = [
  | `Attribute of attribute
  | `Maplike of maplike
  | `Setlike of setlike
] [@@deriving show]

type read_write_attribute = {
  is_inherit : bool ;
  attribute : attribute ;
} [@@deriving show]

type interface_member = [
  | `Const   of const
  | `Operation   of operation
  | `Serializer   of serializer
  | `Stringifier   of stringifier
  | `Static_member   of static_member
  | `Iterable   of iterable
  | `Read_only_member   of read_only_member
  | `Read_write_attribute   of read_write_attribute
  | `Maplike   of maplike
  | `Setlike   of setlike
] [@@deriving show]

type interface = interface_member members_with_inherit [@@deriving show]