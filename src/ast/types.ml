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

type non_any_non_rec = [
  | `Primitive of primitive null
  | `String of string_type null
  | `Ident of string null
  | `Object of unit null
  | `Error of unit null
  | `Domexception of unit null
  | `Buffer of buffer null
] [@@deriving show]

type non_any = [
  | `Promise of return_type
  | `Sequence of types null
  | non_any_non_rec
  | `Frozen_array of types null
  | `Record of (string_type * types) null
] [@@deriving show]

(* any | non_any | union *)
and types = [
  | `Any 
  | `Promise of return_type
  | `Sequence of types null
  | non_any_non_rec
  | `Frozen_array of types null
  | `Record of (string_type * types) null
  | `Union of union null
] [@@deriving show]

(* void | types *)
and return_type = [
  | `Void
  | `Any 
  | `Promise of return_type
  | `Sequence of types null
  | non_any_non_rec
  | `Frozen_array of types null
  | `Record of (string_type * types) null
  | `Union of union null
] [@@deriving show]

and union = [
  | `Or of union_member list
] [@@deriving show]

(* non_any | union  *)
and union_member = [
  | `Promise of return_type
  | `Sequence of types null
  | non_any_non_rec
  | `Frozen_array of types null
  | `Record of (string_type * types) null
  | `Union of union null
] [@@deriving show]
