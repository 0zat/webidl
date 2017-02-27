%{
    (* Web IDL parser
     * The below rules are based on (except some bug fixes)
     * Editor’s Draft, 7 February 2017 
     * https://heycam.github.io/webidl/#idl-grammar 
     *
     * fixed Editor’s Draft bugs : 
     *  modify extended attribute rules to match their explain of Editor’s Draft
     *  remove readonly from readWriteAttribute
     *  remove some rules which are not called
    *)
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
%}

%token LBRACE RBRACE LBRACKET RBRACKET LPAR RPAR LT GT
%token UNSIGNED BYTE OCTET SHORT LONG
%token DOMSTRING USVSTRING BYTESTRING 
%token UNRESTRICTED FLOAT DOUBLE
%token ANY VOID BOOLEAN OBJECT OR 
%token TRUE FALSE NULL INFINITY MINUSINFINITY NAN
%token GETTER SETTER DELETER LEGACYCALLER
%token SERIALIZER STRINGIFIER  
%token QUESTION EQUAL COMMA COLON SEMICOLON ELLIPSIS
%token MAPLIKE SETLIKE ITERABLE
%token PROMISE RECORD SEQUENCE 
%token NAMESPACE CALLBACK PARTIAL INTERFACE DICTIONARY ENUM
%token IMPLEMENTS INHERIT ATTRIBUTE TYPEDEF  CONST
%token READONLY REQUIRED STATIC OPTIONAL   
%token DOMEXCEPTION ERROR_       
%token INT8ARRAY INT16ARRAY INT32ARRAY 
%token UINT8ARRAY UINT16ARRAY UINT32ARRAY UINT8CLAMPEDARRAY 
%token FLOAT32ARRAY FLOAT64ARRAY 
%token ARRAYBUFFER DATAVIEW FROZENARRAY

%token<int> INTVAL
%token<float> FLOATVAL
%token<string> IDENTIFIER STRING
%token EOF

%start main
%type <Ast.Definition.definitions> main
%%

main :
    | definitions EOF { $1 }

definitions :
    | extendedAttributeList definition definitions { {extAttr = $1; value = $2} :: $3 }
    |    { [] }

definition :
    | callbackOrInterface    { $1 }
    | namespace   { `Namespace $1 }
    | partial   { `Partial $1 }
    | dictionary   { `Dictionary $1 }
    | enum   { `Enum $1 }
    | typedef   { `Typedef $1 }
    | implementsStatement   { `Implements $1 }

callbackOrInterface :
    | CALLBACK callbackRestOrInterface   { $2 }
    | interface   { `Interface $1 }

argumentNameKeyword :
    | ATTRIBUTE { `Attribute }
    | CALLBACK { `Callback }
    | CONST { `Const }
    | DELETER { `Deleter }
    | DICTIONARY { `Dictionary }
    | ENUM { `Enum }
    | GETTER { `Getter }
    | IMPLEMENTS { `Implements }
    | INHERIT { `Inherit }
    | INTERFACE { `Interface }
    | ITERABLE { `Iterable }
    | LEGACYCALLER { `Legacycaller }
    | MAPLIKE { `Maplike }
    | NAMESPACE { `Namespace }
    | PARTIAL { `Partial }
    | REQUIRED { `Required }
    | SERIALIZER { `Serializer }
    | SETLIKE { `Setlike }
    | SETTER { `Setter }
    | STATIC { `Static }
    | STRINGIFIER { `Stringifier }
    | TYPEDEF { `Typedef }
    | UNRESTRICTED { `Unrestricted }

callbackRestOrInterface :
    | callbackRest   { `Callback $1 }
    | interface   { `Callback_interface $1 }

interface :
    | INTERFACE IDENTIFIER inheritance LBRACE interfaceMembers RBRACE SEMICOLON  
    { {ident = $2; inheritance = $3; members = $5} }
     
partial :
    | PARTIAL partialDefinition   { $2 }

partialDefinition :
    | partialInterface   { `Interface $1 }
    | partialDictionary   { `Dictionary $1 }
    | namespace   { `Namespace $1 }

partialInterface :
    | INTERFACE IDENTIFIER LBRACE interfaceMembers RBRACE SEMICOLON   
    { {ident = $2; members = $4} }

interfaceMembers :
    | extendedAttributeList interfaceMember interfaceMembers   { {extAttr = $1; value = $2} :: $3 }
    |    { [] }

interfaceMember :
    | const { `Const $1 } 
    | operation { `Operation $1 }
    | serializer { `Serializer $1 }
    | stringifier { `Stringifier $1 }
    | staticMember { `Static_member $1 }
    | iterable { `Iterable $1 }
    | readOnlyMember { `Read_only_member $1 }
    | readWriteAttribute { `Read_write_attribute $1 }
    | readWriteMaplike { `Maplike $1 }
    | readWriteSetlike { `Setlike $1 }

inheritance :
    | COLON IDENTIFIER   { Some $2 }
    |    { None }

const :
    | CONST constType IDENTIFIER EQUAL constValue SEMICOLON { ($2, $3, $5) }

constValue :
    | booleanLiteral   { `Bool $1}
    | floatLiteral   { `Float $1 }
    | INTVAL   { `Int $1 }
    | NULL   { `Null }

booleanLiteral :
    | TRUE   { true }
    | FALSE   { false }

floatLiteral :
    | FLOATVAL   { $1 }
    | MINUSINFINITY   { neg_infinity }
    | INFINITY   { infinity }
    | NAN    { nan }

constType :
    | primitiveType null   { {type_ = ($1 :> const_type); has_null = $2} }
    | IDENTIFIER null   { {type_ = (`Ident $1); has_null = $2} }

readOnlyMember :
    | READONLY readOnlyMemberRest   { $2 }

readOnlyMemberRest :
    | attributeRest { `Attribute $1 }
    | readWriteMaplike { `Maplike $1 }
    | readWriteSetlike { `Setlike $1 }

readWriteAttribute :
    | INHERIT attributeRest   { {is_inherit = true; attribute = $2} }
    | attributeRest { {is_inherit = false; attribute = $1} }

attributeRest :
    | ATTRIBUTE types attributeName SEMICOLON   { ($2, $3) }

attributeName :
    | attributeNameKeyword { $1 }
    | IDENTIFIER { `Ident $1 }

attributeNameKeyword :
    | REQUIRED { `Required }

readOnly :
    | READONLY { true }
    |  { false }

defaultValue :
    | constValue { `Const $1 }
    | STRING { `String $1 }
    | LBRACKET RBRACKET { `Empty_sequence }

operation :
    | returnType operationRest { {specials = []; type_ = $1; ident = fst $2; arguments = snd $2} }
    | specialOperation { $1 }

specialOperation :
    | special specials returnType operationRest 
    { {specials = $1 :: $2; type_ = $3; ident = fst $4; arguments = snd $4} }

specials :
    | special specials { $1 :: $2 }
    |  { [] }

special :
    | GETTER { `Getter }
    | SETTER { `Setter }
    | DELETER { `Deleter }
    | LEGACYCALLER { `Legacycaller }

operationRest :
    | optionalIdentifier LPAR argumentList RPAR SEMICOLON { ($1, $3) }

optionalIdentifier :
    | IDENTIFIER { Some $1 }
    |  { None }

argumentList :
    | argument arguments { $1 :: $2 }
    |  { [] }

arguments :
    | COMMA argument arguments { $2 :: $3 }
    |  { [] }

argument :
    | extendedAttributeList optionalOrRequiredArgument { {extAttr = $1; value = $2} }

optionalOrRequiredArgument :
    | OPTIONAL types argumentName default { `Optional {type_ = $2; name = $3; default = $4} }
    | types ellipsis argumentName { `Required {type_ = $1; is_variadic = $2; name = $3} }

argumentName :
    | argumentNameKeyword { ($1 :> argument_name) }
    | IDENTIFIER { `Ident $1 }

ellipsis :
    | ELLIPSIS { true }
    |  { false }

returnType :
    | types { ($1 :> return_type) }
    | VOID { `Void }

stringifier :
    | STRINGIFIER stringifierRest { $2 }

stringifierRest :
    | readOnly attributeRest { `Attribute {is_read_only = $1; type_ = fst $2; name = snd $2} }
    | returnType operationRest { `Operation($1, (fst $2), snd $2) }
    | SEMICOLON { `None }

serializer :
    | SERIALIZER serializerRest { $2 }

serializerRest :
    | operationRest { `Operation $1 }
    | EQUAL serializationPattern SEMICOLON { $2 }
    | SEMICOLON { `None }

serializationPattern :
    | LBRACE serializationPatternMap RBRACE { `Pattern_map $2}
    | LBRACKET serializationPatternList RBRACKET { `Pattern_list $2 }
    | IDENTIFIER { `Ident $1 }

serializationPatternMap :
    | GETTER { `Getter }
    | INHERIT identifiers { `Inherit $2 }
    | IDENTIFIER identifiers { `Identifiers ($1 :: $2) }
    |  { `None }

serializationPatternList :
    | GETTER { `Getter }
    | IDENTIFIER identifiers { `Identifiers ($1 :: $2) }
    |  { `None }

identifiers :
    | COMMA IDENTIFIER identifiers { $2 :: $3 }
    |  { [] }

staticMember :
    | STATIC staticMemberRest { $2 }

staticMemberRest :
    | readOnly attributeRest { `Attribute {is_read_only = $1; type_ = fst $2; name = snd $2} }
    | returnType operationRest { `Operation($1, fst $2, snd $2)}

iterable :
    | ITERABLE LT types optionalType GT SEMICOLON { ($3, $4) }

optionalType :
    | COMMA types { Some $2 }
    |  { None }

readWriteMaplike :
    | maplikeRest { $1 }

maplikeRest :
    | MAPLIKE LT types COMMA types GT SEMICOLON { ($3, $5) }

readWriteSetlike :
    | setlikeRest { $1 }

setlikeRest :
    | SETLIKE LT types GT SEMICOLON { $3 }

namespace :
    | NAMESPACE IDENTIFIER LBRACE namespaceMembers RBRACE SEMICOLON 
    { {ident = $2; members = $4} }

namespaceMembers :
    |  { [] }
    | extendedAttributeList namespaceMember namespaceMembers 
    { {extAttr = $1; value = $2} :: $3 }

namespaceMember :
    | returnType operationRest { `Operation($1, (fst $2), snd $2) }
    | READONLY attributeRest { `Attribute {is_read_only = true; type_ = fst $2; name = snd $2} }

dictionary :
    | DICTIONARY IDENTIFIER inheritance LBRACE dictionaryMembers RBRACE SEMICOLON 
    { {ident = $2; inheritance = $3; members = $5} }

dictionaryMembers :
    | extendedAttributeList dictionaryMember dictionaryMembers 
    { {extAttr = $1; value = $2} :: $3 }
    |  { [] }

dictionaryMember :
    | required types IDENTIFIER default SEMICOLON 
    { {is_required = $1; type_ = $2; ident = $3; default = $4} }

required :
    | REQUIRED { true }
    |  { false }

partialDictionary :
    | DICTIONARY IDENTIFIER LBRACE dictionaryMembers RBRACE SEMICOLON 
    { {ident = $2; members = $4} }

default :
    | EQUAL defaultValue { Some $2 }
    |  { None }

enum :
    | ENUM IDENTIFIER LBRACE enumValueList RBRACE SEMICOLON { ($2, $4) }

enumValueList :
    | STRING enumValueListComma { $1 :: $2 }

enumValueListComma :
    | COMMA enumValueListString { $2 }
    |  { [] }

enumValueListString :
    | STRING enumValueListComma { $1 :: $2 }
    |  { [] }

callbackRest :
    | IDENTIFIER EQUAL returnType LPAR argumentList RPAR SEMICOLON { ($1, $3, $5) }

typedef :
    | TYPEDEF types IDENTIFIER SEMICOLON { ($2, $3) }

implementsStatement :
    | IDENTIFIER IMPLEMENTS IDENTIFIER SEMICOLON { ($1, $3) }

types :
    | singleType { ($1 :> types) }
    | unionType null { `Union {type_ = $1; has_null = $2} }

singleType :
    | nonAnyType { ($1 :> types) }
    | ANY { `Any }

unionType :
    | LPAR unionMemberType OR unionMemberType unionMemberTypes RPAR 
    { `Or($2 :: $4 :: $5) }

unionMemberType :
    | nonAnyType { ($1 :> union_member) }
    | unionType null { `Union {type_ = $1; has_null = $2} }

unionMemberTypes :
    | OR unionMemberType unionMemberTypes { $2 :: $3 }
    |  { [] }

nonAnyType :
    | promiseType  { `Promise $1 }
    | primitiveType null { `Primitive {type_ = $1; has_null = $2} }
    | stringType null { `String {type_ = $1; has_null = $2} }
    | IDENTIFIER null { `Ident {type_ = $1; has_null = $2} }
    | SEQUENCE LT types GT null { `Sequence {type_ = $3; has_null = $5} }
    | OBJECT null { `Object {type_ = (); has_null = $2} }
    | ERROR_ null { `Error {type_ = (); has_null = $2} }
    | DOMEXCEPTION null { `Domexception {type_ = (); has_null = $2} }
    | bufferRelatedType null { `Buffer {type_ = $1; has_null = $2} }
    | FROZENARRAY LT types GT null {`Frozen_array {type_ = $3; has_null = $5} }
    | recordType null { `Record {type_ = $1; has_null = $2} }

primitiveType :
    | unsignedIntegerType { $1 }
    | unrestrictedFloatType { $1 }
    | BOOLEAN { `Boolean }
    | BYTE { `Byte }
    | OCTET { `Octet }

unrestrictedFloatType :
    | UNRESTRICTED floatType { `Unrestricted $2 }
    | floatType { ($1 :> primitive) }

floatType :
    | FLOAT { `Float }
    | DOUBLE { `Double }

unsignedIntegerType :
    | UNSIGNED integerType { `Unsigned $2 }
    | integerType { ($1 :> primitive) }

integerType :
    | SHORT { `Short }
    | LONG optionalLong { $2 }

optionalLong :
    | LONG { `Long_long }
    |  { `Long }

stringType :
    | BYTESTRING { `Bytestring }
    | DOMSTRING { `Domstring }
    | USVSTRING { `Usvstring }

promiseType :
    | PROMISE LT returnType GT { $3 }

recordType :
    | RECORD LT stringType COMMA types GT { ($3, $5) }

null :
    | QUESTION { true }
    |  { false }

bufferRelatedType :
    | ARRAYBUFFER { `Arraybuffer }
    | DATAVIEW { `Dataview }
    | INT8ARRAY { `Int8array }
    | INT16ARRAY { `Int16array }
    | INT32ARRAY { `Int32array }
    | UINT8ARRAY { `Uint8array }
    | UINT16ARRAY { `Uint16array }
    | UINT32ARRAY { `Uint32array }
    | UINT8CLAMPEDARRAY { `Uint8clampedarray }
    | FLOAT32ARRAY { `Float32array }
    | FLOAT64ARRAY { `Float64array }

extendedAttributeList :
    | LBRACKET extendedAttribute extendedAttributes RBRACKET { $2 :: $3 }
    |  { [] }

extendedAttributes :
    | COMMA extendedAttribute extendedAttributes { $2 :: $3 }
    |  { [] }

extendedAttribute :
    | extendedAttributeNoArgs { $1 }
    | extendedAttributeArgList { $1 }
    | extendedAttributeIdent { $1 }
    | extendedAttributeIdentList { $1 }
    | extendedAttributeNamedArgList { $1 }

identifierList :
    | IDENTIFIER identifiers { $1 :: $2 }

extendedAttributeNoArgs :
    | IDENTIFIER { `No_args $1 }

extendedAttributeArgList :
    | IDENTIFIER LPAR argumentList RPAR { `Argument_list($1, $3) }

extendedAttributeIdent :
    | IDENTIFIER EQUAL IDENTIFIER { `Ident($1, $3) }

extendedAttributeIdentList :
    | IDENTIFIER EQUAL LPAR identifierList RPAR { `Ident_list($1, $4) }

extendedAttributeNamedArgList :
    | IDENTIFIER EQUAL IDENTIFIER LPAR argumentList RPAR 
    { `Named_arg_list($1, $3, $5) }
