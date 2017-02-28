type primitive = [
  | `Boolean 
  | `Byte 
  | `Octet 
  | `Unrestricted of [ `Float | `Double ]
  | `Float 
  | `Double 
  | `Unsigned of [ `Short | `Long | `Long_long ]
  | `Short 
  | `Long 
  | `Long_long 
] [@@deriving show]

type string_type = [
  | `Bytestring 
  | `Domstring 
  | `Usvstring
] [@@deriving show]

type 'a null = {
  type_ : 'a; 
  has_null : bool;
} [@@deriving show]

type buffer = [
  | `Arraybuffer 
  | `Dataview 
  | `Int8array 
  | `Int16array 
  | `Int32array 
  | `Uint8array 
  | `Uint16array 
  | `Uint32array 
  | `Uint8clampedarray 
  | `Float32array 
  | `Float64array 
] [@@deriving show]

type ('return_type, 'types) non_any_aux = [
  | `Promise of 'return_type
  | `Sequence of 'types null
  | `Primitive of primitive null
  | `String of string_type null
  | `Ident of string null
  | `Object of unit null
  | `Error of unit null
  | `Domexception of unit null
  | `Buffer of buffer null
  | `Frozen_array of 'types null
  | `Record of (string_type * 'types) null
] [@@deriving show]

type ('return_type, 'types) union_aux = [
    | `Or of ('return_type, 'types) union_member_aux list
  ] [@@deriving show]

(* non_any | union  *)
and ('return_type, 'types) union_member_aux = [
    | ('return_type, 'types) non_any_aux
    | `Union of ('return_type, 'types) union_aux null
  ] [@@deriving show]

type ('return_type, 'types) types_aux = [
  | `Any 
  | ('return_type, 'types) non_any_aux
  | `Union of ('return_type, 'types) union_aux null
] [@@deriving show]

(* void | types *)
type ('return_type, 'types) return_type_aux = [
    | `Void
    | ('return_type, 'types) types_aux 
  ] [@@deriving show]

type types = (return_type, types) types_aux [@@deriving show]
and return_type = (return_type, types) return_type_aux [@@deriving show]

type union = (return_type, types) union_aux
type union_member = (return_type, types) union_member_aux