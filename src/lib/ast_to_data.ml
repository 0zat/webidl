open Ast

open Data

let of_null cond (dst : Data.null) = 
  match cond, dst with
  | false, _ -> (dst :> Data.non_any)
  | true, dst -> `Nullable dst

let rec of_non_any : Ast.non_any -> Data.non_any = function
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

and of_union_member : Ast.union_member -> Data.non_any = function
  | `Union {type_; has_null} -> 
    let members = List.map of_union_member type_ in
    of_null has_null (`Union members)
  | #Ast.non_any as non_any -> 
    let member = of_non_any non_any in
    member

and of_types : Ast.types -> Data.types = function
  | `Any -> `Any
  | #Ast.union_member as union -> (of_union_member union :> Data.types)

and of_return_type : Ast.return_type -> Data.return_type = function
  | `Void -> `Void
  | #Ast.types as types -> (of_types types :> Data.return_type)

let of_argument = function
  | `Optional {type_; name; default} -> 
    let type_ = of_types type_ in
    {type_; name; necessity = `Optional default}
  | `Required {type_; name; is_variadic} -> 
    let type_ = of_types type_ in 
    let necessity = 
      match is_variadic with
      | true -> `Required `Variadic 
      | false -> `Required `Fixed 
    in
    {type_; name; necessity}

let of_with_extAttr of_extAttr f {extAttr; value} =
  (List.map of_extAttr extAttr, f value)

let of_ext_list of_extAttr f list =
  List.map (of_with_extAttr of_extAttr f) list

let rec of_extAttr : Ast.extended_attribute -> Data.extended_attribute = function
  | `No_args ident -> `Argument_list(ident, [])
  | `Argument_list(ident, arguments) -> 
    `Argument_list(ident, of_ext_list of_extAttr of_argument arguments)
  | `Ident(ident, ident_1) -> `Ident_list(ident, [ident_1]) 
  | `Ident_list(ident, idents) -> `Ident_list(ident, idents) 
  | `Named_arg_list(ident, name, arguments) ->
    `Named_arg_list(ident, name, of_ext_list of_extAttr of_argument arguments)

let of_ext_list f list = of_ext_list of_extAttr f list 

let of_operation is_static specials type_ ident arguments =
  let type_ = of_return_type type_ in
  let arguments = of_ext_list of_argument arguments in
  {is_static; specials; type_; ident; arguments}

let of_attribute is_static is_inherit is_readonly (type_, name) =
  let type_ = of_types type_ in
  {is_static; is_inherit; is_readonly; name; type_}

let of_const (type_, ident, value) = 
  let type_ =
    match type_.has_null with
    | true -> `Nullable (type_.type_ :> const_type)
    | false -> (type_.type_ :> [const_type | `Nullable of const_type])
  in
  (type_, ident, value)

let of_serializer = function
  | `Operation(ident, arguments) -> 
    `Operation(ident, of_ext_list of_argument arguments)
  | `Pattern_map pattern_map -> `Pattern_map pattern_map
  | `Pattern_list pattern_list -> `Pattern_list pattern_list
  | `Ident ident -> `Ident ident
  | `None -> `None

let of_static = function 
  | `Operation(return_type, (ident, arguments)) ->
    `Operation(of_operation true [] return_type ident arguments)
  | `Attribute {is_readonly; attribute} -> 
    `Attribute(of_attribute true false is_readonly attribute)

let of_stringifier = function
  | `Operation(return_type, (ident, arguments)) ->
    `Operation(of_operation false [] return_type ident arguments)
  | `Attribute {is_readonly; attribute} -> 
    `Attribute (of_attribute false false is_readonly attribute)
  | `None -> `None

let of_maplike is_readonly (key_type, value_type) = 
  let key_type = of_types key_type in
  let value_type = of_types value_type in
  {is_readonly; key_type; value_type}

let of_readonly : Ast.read_only_member -> Data.interface_member = function
  | `Attribute attribute -> 
    `Attribute (of_attribute false false true attribute)
  | `Maplike maplike -> `Maplike(of_maplike true maplike)
  | `Setlike key_type -> `Setlike {is_readonly = true; key_type= of_types key_type}

let of_interface_member : Ast.interface_member -> Data.interface_member = function
  | `Const const -> `Const(of_const const) 
  | `Operation {specials; type_; arguments} -> 
    let ident, arguments = arguments in
    `Operation(of_operation false specials type_ ident arguments)
  | `Serializer serializer -> `Serializer(of_serializer serializer)
  | `Stringifier stringifier -> `Stringifier(of_stringifier stringifier)
  | `Static_member static -> of_static static
  | `Iterable(types, None) -> `Iterable(of_types types, None) 
  | `Iterable(types, Some type_) -> `Iterable(of_types types, Some (of_types type_))
  | `Read_only_member member -> of_readonly member  
  | `Read_write_attribute {is_inherit; attribute} ->
    `Attribute (of_attribute false is_inherit false attribute)
  | `Maplike maplike -> `Maplike(of_maplike false maplike)
  | `Setlike key_type -> `Setlike {is_readonly = false; key_type= of_types key_type}

let of_interface ident inheritance members = {
  ident = ident ;
  inheritance = inheritance ;
  interface_members =  of_ext_list of_interface_member members ;
}

let of_namespace_member = function
  | `Operation(return_type, (ident, arguments)) ->
    let operation = of_operation false [] return_type ident arguments in
    `Operation operation
  | `Attribute {is_readonly; attribute} ->
    `Attribute (of_attribute false false is_readonly attribute)

let of_namespace ident members = {
  ident = ident ;
  namespace_members = of_ext_list of_namespace_member members ;
}

let of_dictionary_member (member : Ast.dictionary_member) = {
  is_required = member.is_required ; 
  type_ = of_types member.type_ ; 
  ident = member.ident ; 
  default = member.default ;
}

let of_dictionary ident inheritance members = {
  ident = ident ;
  inheritance = inheritance ;
  dictionary_members =  of_ext_list of_dictionary_member members ;
}

let of_partial : Ast.partial -> Data.partial = function
  | `Interface {ident; members} -> `Interface(of_interface ident None members)
  | `Dictionary {ident; members} -> `Dictionary(of_dictionary ident None members)
  | `Namespace {ident; members} -> `Namespace(of_namespace ident members) 

let of_definition : Ast.definition -> Data.definition = function
  | `Callback(string, return_type, arguments) -> 
    `Callback(string, of_return_type return_type, of_ext_list of_argument arguments)
  | `Callback_interface {ident; inheritance; members} ->
    `Callback_interface(of_interface ident inheritance members)
  | `Interface {ident; inheritance; members} -> 
    `Interface(of_interface ident inheritance members)
  | `Namespace {ident; members} -> `Namespace(of_namespace ident members)
  | `Partial partial -> `Partial(of_partial partial)
  | `Dictionary {ident; inheritance; members} ->
    `Dictionary(of_dictionary ident inheritance members)
  | `Enum(string, values) -> `Enum(string, values) 
  | `Typedef(types, string) -> `Typedef(of_types types, string)
  | `Implements(ident, implement) -> `Implements(ident, implement)

let of_difinitions (definitions : Ast.definitions) : Data.definitions =
  of_ext_list of_definition definitions