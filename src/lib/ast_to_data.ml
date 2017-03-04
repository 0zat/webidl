open Ast.Types
open Ast.Const
open Ast.Argument
open Ast.Extended
open Ast.Operation
open Ast.Attribute
open Ast.Interface
open Ast.Dictionary
open Ast.Namespace
open Ast.Definition

open Data

let of_null cond (dst : Data.null) = 
  match cond, dst with
  | false, _ -> (dst :> Data.non_any)
  | true, dst -> `Nullable dst

let rec of_non_any : Ast.Types.non_any -> Data.non_any = function
  | `Promise return_type -> `Promise(of_return_type return_type)
  | `Sequence {type_; has_null} -> 
    let types = of_types type_ in
    of_null has_null (`Sequence types) 
  | `Primitive {type_; has_null} -> of_null has_null (type_ :> Data.null)
  | `String {type_; has_null} -> of_null has_null (type_ :> Data.null)
  | `Ident {type_; has_null} -> 
    let ident = `Ident type_ in
    of_null has_null ident
  | `Object {type_; has_null} -> of_null has_null `Object 
  | `Error {type_; has_null} -> of_null has_null `Error   
  | `Domexception {type_; has_null} -> of_null has_null `Domexception 
  | `Buffer {type_; has_null} -> of_null has_null (type_ :> Data.null)
  | `Frozen_array {type_; has_null} -> 
    let types = of_types type_ in
    of_null has_null (`Frozen_array types)
  | `Record {type_; has_null} ->
    let string_type, types = type_ in
    of_null has_null (`Record(string_type, of_types types))

and of_union_member : Ast.Types.union_member -> Data.non_any = function
  | `Union {type_; has_null} -> 
    let members = List.map of_union_member type_ in
    of_null has_null (`Union members)
  | #Ast.Types.non_any as non_any -> 
    let member = of_non_any non_any in
    member

and of_types : Ast.Types.types -> Data.types = function
  | `Any -> `Any
  | #Ast.Types.union_member as union -> (of_union_member union :> Data.types)

and of_return_type : Ast.Types.return_type -> Data.return_type = function
  | `Void -> `Void
  | #Ast.Types.types as types -> (of_types types :> Data.return_type)

let rec of_extAttr = function
  | `No_args ident -> `Argument_list(ident, [])
  | `Argument_list(ident, arguments) -> `Argument_list(ident, List.map of_argument arguments)
  | `Ident(ident, ident_1) -> `Ident_list(ident, [ident_1]) 
  | `Ident_list(ident, idents) -> `Ident_list(ident, idents) 
  | `Named_arg_list(ident, name, arguments) 
    -> `Named_arg_list(ident, name, List.map of_argument arguments)

and of_argument argument = 
  let extended_attributes = List.map of_extAttr argument.extAttr in
  match argument.value with
  | `Optional {type_; name; default} -> 
    let type_ = of_types type_ in
    {type_; name; extended_attributes; necessity = `Optional default}
  | `Required {type_; name; is_variadic} -> 
    let type_ = of_types type_ in 
    let necessity = 
      match is_variadic with
      | true -> `Required `Variadic 
      | false -> `Required `Fixed 
    in
    {type_; name; extended_attributes; necessity}

(*
  let of_interface_member member =
    let extAttr = member.extAttr in
    match member.value with
    | `Const   
    | `Operation  
    | `Serializer  
    | `Stringifier 
    | `Static_member  
    | `Iterable  
    | `Read_only_member   
    | `Read_write_attribute   
    | `Maplike   
    | `Setlike  

let of_interface ast_interface extAttr = {
  extended_attributes = extAttr ;
  ident = ast_interface.ident ;
  inheritance = ast_interface.inheritance ;
  interface_members = List.map of_interface_member ast_interface.members ;
}

let of_definition extAttr = function
  | `Callback(string, return_type, arguments) -> `Callback(extAttr, string, return_type, arguments)
  | `Callback_interface interface
  | `Interface interface
  | `Namespace namespace
  | `Partial partial
  | `Dictionary dictionary
  | `Enum(string, values) -> `Enum(extAttr, string, values) 
  | `Typedef(types, string) -> `Typedef(extAttr, types, string)
  | `Implements(ident, implement) -> `Implements(extAttr, ident, implement)

  *)