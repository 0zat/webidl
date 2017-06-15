

extendedAttribute :
    | LPAR extendedAttributeInner RPAR extendedAttributeRest { `LPAR(Parsing. ) }
    | LBRACKET extendedAttributeInner RBRACKET extendedAttributeRest {}
    | LBRACE extendedAttributeInner RBRACE extendedAttributeRest {}
    | other extendedAttributeRest {}

extendedAttributeRest :
    | extendedAttribute {}
    | { [] }
    
extendedAttributeInner :
    | LPAR extendedAttributeInner RPAR extendedAttributeInner {}
    | LBRACKET extendedAttributeInner RBRACKET extendedAttributeInner {}
    | LBRACE extendedAttributeInner RBRACE extendedAttributeInner {}
    | otherOrComma extendedAttributeInner {}
    |  {}

other :
    | INTVAL {}
    | FLOATVAL {}
    | IDENTIFIER {}
    | STRING {}
    | OTHER {}
    | MINUS {}
    | MINUSINFINITY {}
    | DOT {}
    | ELLIPSIS {}
    | COLON {}
    | SEMICOLON {}
    | LT {}
    | EQUAL {}
    | GT {}
    | QUESTION {}
    | BYTESTRING {}
    | DOMSTRING {}
    | FROZENARRAY {}
    | INFINITY {}
    | NAN {}
    | USVSTRING {}
    | ANY {}
    | BOOLEAN {}
    | BYTE {}
    | DOUBLE {}
    | FALSE {}
    | FLOAT {}
    | LONG {}
    | NULL {}
    | OBJECT {}
    | OCTET {}
    | OR {}
    | OPTIONAL {}
    | SEQUENCE {}
    | SHORT {}
    | TRUE {}
    | UNSIGNED {}
    | VOID {}
    | argumentNameKeyword {}
    | bufferRelatedType {}

otherOrComma :
    | other {}
    | COMMA {}
extendedAttributesStandard :
    | COMMA extendedAttributeStandard extendedAttributesStandard { $2 :: $3 }
    |  { [] }

extendedAttributeStandard :
    | extendedAttributeNoArgs { $1 }
    | extendedAttributeArgList { $1 }
    | extendedAttributeIdent { $1 }
    | extendedAttributeIdentList { $1 }
    | extendedAttributeNamedArgList { $1 }

identifierList :
    | IDENTIFIER identifiers { $1 :: $2 }

extendedAttributeNoArgs :
    | IDENTIFIER { `NoArgs $1 }

extendedAttributeArgList :
    | IDENTIFIER LPAR argumentList RPAR { `ArgumentList($1, $3) }

extendedAttributeIdent :
    | IDENTIFIER EQUAL IDENTIFIER { `Ident($1, $3) }

extendedAttributeIdentList :
    | IDENTIFIER EQUAL LPAR identifierList RPAR { `IdentList($1, $4) }

extendedAttributeNamedArgList :
    | IDENTIFIER EQUAL IDENTIFIER LPAR argumentList RPAR 
    { `NamedArgList($1, $3, $5) }