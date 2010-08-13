grammar M2FormalParams;

options {
    k = 1; backtrack = no;
}

tokens {
    CONST = 'CONST';
    OF = 'OF';
    VAR = 'VAR';
    VARIADIC = 'VARIADIC';
}

simpleFormalType :
    Ident
    ;

// PROCEDURE foo1 ( CONST v1, v2 : VARIADIC OF Bar );
// n := HIGH(v1) => v1[n]; m := HIGH(v2) => v2[m];
// PROCEDURE foo2 ( v1, v2 : VARIADIC OF ( CONST a : Bar; VAR b : Baz ) );
// n := HIGH(v1) => v1[n].a; v1[n].b; m := HIGH(v2) => v2[m].a; v2[m].b;

formalParams : // #32
    formalValueParams | formalConstOrVarParams
    ;

formalValueParams : // #32b
    identList ':'
    ( simpleFormalType | variadicHeader
      ( simpleFormalType | '(' simpleFormalParams ( ';' simpleFormalParams )* ')' ) )
    ;

formalConstOrVarParams : // #32a
    ( CONST | VAR {}) identList ':' variadicHeader? simpleFormalType
    ;

simpleFormalParams : // #33
	( CONST | VAR {})? identList ':' simpleFormalType
	;

variadicHeader : // #34
     VARIADIC ( variadicCounter | '[' variadicTerminator ']' )? OF
	;

variadicCounter :
    Ident
    ;

variadicTerminator :
    Ident | Number
    ;

// boilerplate only after this point

identList :
    Ident ( ',' Ident )*
    ;

Ident :
    ( '_' | '$' | Letter {}) ( '_' | '$' | Letter | Digit {})*
    ;

Number :
    Digit*
    ;

fragment Letter :
    'a' .. 'z' | 'A' .. 'Z' {}
    ;

fragment Digit :
    '0' .. '9'
    ;
