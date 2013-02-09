/* (C) 2009-2012 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Feb 8, 2013 */


// ---------------------------------------------------------------------------
// N A M I N G   C O N V E N T I O N
// ---------------------------------------------------------------------------
//
// Non-Terminals:
//  camelCase with first character lowercase
// 
// Terminals:
//  CamelCase with first character uppercase
//
// Reserved Words:
//  ALL_UPPERCASE


options {

// *** enforce strict LL(1) ***

    k = 1; backtrack = no;
}


// ---------------------------------------------------------------------------
// T O K E N   S Y M B O L S
// ---------------------------------------------------------------------------
// 45 reserved words, 17 identifiers, 18 pragma words

tokens {
	
// *** Reserved Words, 45 tokens ***

    ALIAS          = 'ALIAS';
    AND            = 'AND';            /* also a RW within pragmas */
    ARRAY          = 'ARRAY';
    ASSOCIATIVE    = 'ASSOCIATIVE';
    BEGIN          = 'BEGIN';
    BLUEPRINT      = 'BLUEPRINT';
    BY             = 'BY';
    CASE           = 'CASE';
    CONST          = 'CONST';
    DEFINITION     = 'DEFINITION';
    DESCENDING     = 'DESCENDING';
    DIV            = 'DIV';            /* also a RW within pragmas */
    DO             = 'DO';
    ELSE           = 'ELSE';           /* also a RW within pragma */
    ELSIF          = 'ELSIF';          /* also a RW within pragma */
    END            = 'END';
    EXIT           = 'EXIT';
    FOR            = 'FOR';
    FROM           = 'FROM';           /* also a RW within pragma */
    IF             = 'IF';             /* also a RW within pragma */
    IMPLEMENTATION = 'IMPLEMENTATION';
    IMPORT         = 'IMPORT';
    IN             = 'IN';
    INDETERMINATE  = 'INDETERMINATE';
    LOOP           = 'LOOP';
    MOD            = 'MOD';            /* also a RW within pragmas */
    MODULE         = 'MODULE';
    NOT            = 'NOT';            /* also a RW within pragmas */
    OF             = 'OF';
    OPAQUE         = 'OPAQUE';
    OR             = 'OR';             /* also a RW within pragmas */
    PLACEHOLDERS   = 'PLACEHOLDERS';
    POINTER        = 'POINTER';
    PROCEDURE      = 'PROCEDURE';
    RECORD         = 'RECORD';
    REPEAT         = 'REPEAT';
    RETURN         = 'RETURN';
    SET            = 'SET';
    THEN           = 'THEN';
    TO             = 'TO';
    TYPE           = 'TYPE';           /* also a RW within pragma */
    UNTIL          = 'UNTIL';
    VAR            = 'VAR';
    VARIADIC       = 'VARIADIC';
    WHILE          = 'WHILE';

// *** CAST ***

//  CAST is both an Identifier and a Reserved Word
//  Ambiguity is resolvable using the Schroedinger's Token technique

    CAST           = 'CAST';           /* RW within procedure header */

// *** Bindable Identifiers ***

//  Bindable Identifiers are both Identifiers and Reserved Words
//  Ambiguity is resolvable using the Schroedinger's Token technique

    TSIG           = 'TSIG';           /* RW within constant definition */
    TEXP           = 'TEXP';           /* RW within constant definition */

    ABS            = 'ABS';            /* RW within procedure header */
    NEG            = 'NEG';            /* RW within procedure header */
    ODD            = 'ODD';            /* RW within procedure header */
    COUNT          = 'COUNT';          /* RW within procedure header */
    LENGTH         = 'LENGTH';         /* RW within procedure header */
    NEW            = 'NEW';            /* RW within procedure header */
    DISPOSE        = 'DISPOSE';        /* RW within procedure header */
    RETAIN         = 'RETAIN';         /* RW within procedure header */
    RELEASE        = 'RELEASE';        /* RW within procedure header */
    TLIMIT         = 'TLIMIT';         /* RW within procedure header */
    TMAX           = 'TMAX';           /* RW within procedure header */
    TMIN           = 'TMIN';           /* RW within procedure header */
    SXF            = 'SXF';            /* RW within procedure header */
    VAL            = 'VAL';            /* RW within procedure header */

// *** Language Defined Pragma Words, 19 tokens ***

//  Pragma Words are Reserved Words only within pragmas
//  Ambiguity is resolvable using the Schroedinger's Token technique

    MSG            = 'MSG';            /* RW within pragma only */
    INFO           = 'INFO';           /* RW within pragma only */
    WARN           = 'WARN';           /* RW within pragma only */
    ERROR          = 'ERROR';          /* RW within pragma only */
    FATAL          = 'FATAL';          /* RW within pragma only */
    ENDIF          = 'ENDIF';          /* RW within pragma only */
    ENCODING       = 'ENCODING';       /* RW within pragma only */
    GENLIB         = 'GENLIB';         /* RW within pragma only */
    FFI            = 'FFI';            /* RW within pragma only */
    INLINE         = 'INLINE';         /* RW within pragma only */
    NOINLINE       = 'NOINLINE';       /* RW within pragma only */
    ALIGN          = 'ALIGN';          /* RW within pragma only */
    PADBITS        = 'PADBITS';        /* RW within pragma only */
    ADDR           = 'ADDR';           /* RW within pragma only */
    REG            = 'REG';            /* RW within pragma only */
    PURITY         = 'PURITY';         /* RW within pragma only */
    SINGLEASSIGN   = 'SINGLEASSIGN' ;  /* RW within pragma only */
    VOLATILE       = 'VOLATILE';       /* RW within pragma only */
    FORWARD        = 'FORWARD';        /* RW within pragma only */

// *** Special Characters, 3 tokens ***

    BACKSLASH      = '\\';             /* for readability */
    SINGLE_QUOTE   = '\'' ;            /* for readability */
    DOUBLE_QUOTE   = '\"' ;            /* for readability */

// *** Ignore Characters, 3 tokens ***

    ASCII_TAB      = '\t';             /* for readability */
    ASCII_LF       = '\n';             /* for readability */
    ASCII_CR       = '\r';             /* for readability */
}


// ---------------------------------------------------------------------------
// N O N - T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 60 productions

// *** Compilation Units ***

// production #1
compilationUnit :	
      IMPLEMENTATION? programModule | definitionOfModule | blueprint
    ;

// production #2
programModule :
    MODULE moduleIdent ';'
    importList* block moduleIdent '.'
    ;

// alias 2.1
moduleIdent : Ident ;

// production #3
definitionOfModule :
    DEFINITION MODULE moduleIdent ( '[' blueprintIdent ']' )? ';'
    importList* definition*
    END moduleIdent '.'
    ;

// production #4
blueprint :
    BLUEPRINT blueprintIdent '[' requiredConformance ']' ';'
    ( PLACEHOLDERS identList ';' )?
    requiredTypeDeclaration ';'
    ( requiredBinding ';' )*
    END blueprintIdent '.'
    ;

// alias #4.1
blueprintIdent : Ident ;

// alias #4.2
requiredConformance : blueprintIdent ;

// alias #4.3
requiredBinding : procedureHeader ;


// *** Import Lists, Blocks, Definitions and Declarations ***

// production #5
importList :
    ( IMPORT moduleIdent '+'? ( ',' moduleIdent '+'? )* |
      FROM moduleIdent IMPORT ( identList | '*' ) ) ';'
    ;

/* Import with experimental aliased import qualifier '=>'
   IMPORT Foo => Bar would import module Foo to be referenced as Bar
   Strongest use case: import of target architecture specific modules, eg.
   IMPORT Registers_x86 => Registers, Opcodes_x86 => Opcodes, etc
   requires further deliberation
   
importListWithAlias :
    ( FROM moduleIdent IMPORT ( identList | '*' ) |
    IMPORT moduleIdent ( '+' | '=>' Ident )? ( ',' moduleIdent ( '+' | '=>' Ident )? )* ) ';'
    ;
*/

// production #6
block :
    declaration*
    ( BEGIN statementSequence )? END
    ;

// production #7
definition :
    CONST (  publicConstDeclaration ';' )+ |
    TYPE ( publicTypeDeclaration ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';'
    ;

// production #8
publicConstDeclaration :	
    ( '[' constBindableIdent ']' )? Ident '=' constExpression
    ;

// alias #8.1
constBindableIdent : /* Ident */ TSIG | TEXP
    {} /* make ANTLRworks display separate branches */
    ;

// alias #8.2
constExpression : expression ; /* no type identifiers */

// production #9
publicTypeDeclaration :
    Ident '=' ( type | OPAQUE recordType? )
    ;

// production #10
declaration :
    CONST ( Ident '=' constExpression ';' )+ |
    TYPE ( Ident '=' type ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';' block Ident ';'
    ;

// production #11
requiredTypeDeclaration :
    TYPE typeIdent '='
    permittedTypeDeclaration ( '|' permittedTypeDeclaration )*
    ( ':=' protoliteral ( '|' protoliteral )* )?
    ;

// production #12
permittedTypeDeclaration :
    RECORD | OPAQUE RECORD?
    ;

// production #13
protoliteral :
    simpleProtoliteral | structuredProtoliteral
    ;

// alias #13.1
simpleProtoliteral : Ident ; /* CHAR, INTEGER or REAL */

// production #14
structuredProtoliteral :
    '{' ( VARIADIC OF simpleProtoliteral ( ',' simpleProtoliteral )* |
    structuredProtoliteral ( ',' structuredProtoliteral )* ) '}'
    ;


// *** Types ***

// production #15
type :
    (( ALIAS | range ) OF )? typeIdent | enumerationType |
    arrayType | recordType | setType | pointerType | procedureType
    ;

// alias 15.1
typeIdent : qualident ;

// production #16
range :
    '[' constExpression '..' constExpression ']'
    ;

// production #17
enumerationType :
    '(' ( '+' enumBaseType ',' )? identList ')'
    ;

/* Enumeration with experimental constant qualifier
   TYPE Foo = ( CONST foo, bar, baz ) would define each of the enumerated
   values also as constants in the module so that they could be qualified
   simply with the module name if the module is imported qualified.
   requires further deliberation
   
enumTypeWithConst :
    '(' CONST? ( '+' enumBaseType ',' )? identList ')'
    ;
*/

// alias 17.1
enumBaseType : typeIdent ;

// production #18
arrayType :
    ( ARRAY componentCount ( ',' componentCount )* |
      ASSOCIATIVE ARRAY ) OF typeIdent
    ;

// alias #18.1
componentCount : constExpression ;

// production #19
recordType :
    RECORD ( fieldList ( ';' fieldList )* indeterminateField? |
    '(' baseType ')' fieldList ( ';' fieldList )* ) END
    ;

// aliase #19.1
fieldList : variableDeclaration ;

// aliase #19.2
baseType : typeIdent ;

// production #20
indeterminateField :
    INDETERMINATE Ident ':' ARRAY discriminantFieldIdent OF typeIdent
    ;

// alias #20.1
discriminantFieldIdent : Ident ;

// production #21
setType :	
    SET OF ( enumBaseType | '(' identList ')' )
    ;

// production #22
pointerType :
    POINTER TO CONST? typeIdent
    ;

// production #23
procedureType :
    PROCEDURE
    ( '(' formalTypeList ')' )?
    ( ':' returnedType )?
    ;

// alias #23.1
returnedType : typeIdent ;

// production #24
formalTypeList :
    formalType ( ',' formalType )*
    ;

// production #25
formalType :
    attributedFormalType | variadicFormalType
    ;

// production #26
attributedFormalType :
    ( CONST | VAR {})? simpleFormalType
    ;

// production #27
simpleFormalType :
    CAST? ARRAY OF typeIdent
    ;

// production #28
variadicFormalType :
    VARIADIC OF
    ( attributedFormalType |
    '{' attributedFormalType ( ',' attributedFormalType )* '}')
    ;

// *** Variable Declarations ***

// production #29
variableDeclaration :
    identList ':' ( range OF )? typeIdent
    ;

// *** Procedures ***

// production #30
procedureHeader :
    PROCEDURE ( '[' bindableEntity ']' )?
    Ident ( '(' formalParamList ')' )?
    ( ':' returnedType )?
    ;

// production #31
bindableEntity :
    DIV | MOD | FOR | DESCENDING |
    '::' | ':=' | '?' | '!' | '~' | '+' | '-' | '*' | '/' | '=' | '<' | '>' |
    bindableIdent
    ;

// fragment #31.1
// both an identifier and a reserved word
// resolve using Schroedinger's Token
bindableIdent :
    ABS | NEG | ODD | COUNT | LENGTH | NEW | DISPOSE | RETAIN | RELEASE |
    TLIMIT | TMIN | TMAX | SXF | VAL
    {} /* make ANTLRworks display separate branches */
    ;

// *** Formal Parameters ***

// production #32
formalParamList :
    formalParams ( ';' formalParams )*
    ;

// production #33
formalParams :
    simpleFormalParams | variadicFormalParams
    ;

// production #34
simpleFormalParams :
    ( CONST | VAR {})? identList ':' simpleFormalType
    ;

// production #35
variadicFormalParams :
    VARIADIC ( '[' variadicTerminator ']' )? OF
    ( simpleFormalType | '{' simpleFormalParams ( ';' simpleFormalParams )* '}' )
    ;

// alias #35.1
variadicTerminator : constExpression ;

// *** Statements ***

// production #36
statement :
    ( assignmentOrProcedureCall | ifStatement | caseStatement |
      whileStatement | repeatStatement | loopStatement |
      forStatement | RETURN expression? | EXIT )?
    ;

// production #37
statementSequence :
    statement ( ';' statement )*
    ;

// production #38
assignmentOrProcedureCall :
    designator ( ':=' expression | '++' | '--' | actualParameters )?
    ;

// production #39
ifStatement :
    IF expression THEN statementSequence
    ( ELSIF expression THEN statementSequence )*
    ( ELSE statementSequence )?
    END
    ;

// production #40
caseStatement :
    CASE expression OF ( '|' case777 )+ ( ELSE statementSequence )? END
    ;

// production #41
case777 :
/* ANTLR has a design flaw in that it cannot handle any rule identifiers that
   coincide with reserved words of the language it uses to generate the parser */
    caseLabels ( ',' caseLabels )* ':' statementSequence
    ;

// production #42
caseLabels :
    constExpression ( '..' constExpression )?
    ;

// production #43
whileStatement :
    WHILE expression DO statementSequence END
    ;

// production #44
repeatStatement :
    REPEAT statementSequence UNTIL expression
    ;

// production #45
loopStatement :
    LOOP statementSequence END
    ;

// production #46
forStatement :
    FOR DESCENDING? controlVariable
    IN ( designator | range OF typeIdent )
    DO statementSequence END
    ;

// alias #46.1
controlVariable : Ident ;

// production #47
designator :
    qualident designatorTail?
    ;

// production #48
designatorTail :
    ( ( '[' expressionList ']' | '^' ) ( '.' Ident )* )+
    ;

// *** Expressions ***

// production #49
expressionList :
    expression ( ',' expression )*
    ;

// production #50
expression :
/* represents operator precedence level 1 */
    simpleExpression ( relOp simpleExpression )?
    ;

// fragment #50.1
relOp :
    '=' | '#' | '<' | '<=' | '>' | '>=' | IN
    {} /* make ANTLRworks display separate branches */
    ;

// production #51
simpleExpression :
/* represents operator precedence level 2 */
    ( '+' | '-' {} /* make ANTLRworks display separate branches */ )?
    term ( addOp term )*
    ;

// fragment #51.1
addOp :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
	;

// production #52
term :
/* represents operator precedence level 3 */
    factorOrNegation ( mulOp factorOrNegation )*
    ;

// fragment #52.1
mulOp :
    '*' | '/' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #53
factorOrNegation :
/* represents operator precedence level 4 */
    NOT? factor
    ;

// production #54
factor :
/* represents operator precedence level 5 */
    ( NumericLiteral | StringLiteral | structuredValue |
    designatorOrFunctionCall | '(' expression ')' )
    ( '::' typeIdent )?
    ;

// production #55
designatorOrFunctionCall :
    designator actualParameters?
    ;

// production #56
actualParameters :
    '(' expressionList? ')'
    ;

// *** Structured Values ***

// production #57
structuredValue :
    '{' ( valueComponent ( ',' valueComponent )* )? '}'	
    ;

// production #58
valueComponent :
    expression ( ( BY | '..' {}) constExpression )?
    ;

// *** Identifiers ***

// production #59
qualident :
    Ident ( '.' Ident )*
    ;

// production #60
identList :
    Ident ( ',' Ident )*
    ;


// ---------------------------------------------------------------------------
// P R A G M A   G R A M M A R
// ---------------------------------------------------------------------------
// 24 productions

// *** Pragmas ***

// production #1
pragma :
    '<*'
    ( pragmaMSG | pragmaIF | pragmaENCODING | pragmaGENLIB | pragmaFFI |
      pragmaINLINE | pragmaALIGN | pragmaPADBITS | pragmaADDR | pragmaREG |
      pragmaPURITY | variableAttrPragma | implDefinedPragma | pragmaFORWARD )
    '*>'
    ;

// production #2
pragmaMSG :
    MSG '=' ( INFO | WARN | ERROR | FATAL {}) ':'
    compileTimeMsgComponent ( ',' compileTimeMsgComponent )*
    ;

// production #3
compileTimeMsgComponent :
    StringLiteral | constQualident |
    '?' ( ALIGN | ENCODING | implDefinedPragmaName )
    ;

// alias #3.1
constQualident : qualident ; /* no type and no variable identifiers */

// alias #3.2
implDefinedPragmaName : Ident ; /* lowercase or mixed case only */

// production #4
pragmaIF :
    ( IF | ELSIF {}) inPragmaExpression | ELSE | ENDIF
    ;

// production #5
pragmaENCODING : 
    ENCODING '=' StringLiteral /* "ASCII" or "UTF8" */
    ( ':' codePointSample ( ',' codePointSample )* )?
    ;

// production #6
codePointSample :
    quotedCharacterLiteral '=' characterCodeLiteral
    ;

// alias #6.1
quotedCharacterLiteral : StringLiteral ; /* single character only */

// alias #6.2
characterCodeLiteral : NumericLiteral ; /* unicode code points only */

// production #7
pragmaGENLIB :
    GENLIB moduleIdent FROM template ':' templateParamList
    ;

// alias #7.1
template : Ident ;

// production #8
templateParamList :
    ( placeholder '=' replacement ) ( ',' placeholder '=' replacement )*
    ;

// alias #8.1
placeholder : Ident ;

// alias #8.2
replacement : StringLiteral ;

// production #9
pragmaFFI :
    FFI '=' StringLiteral /* "C" or "Fortran" */
    ;

// production #10
pragmaINLINE :
    INLINE | NOINLINE
    {} /* make ANTLRworks display separate branches */
    ;

// production #11
pragmaALIGN :
    ALIGN '=' inPragmaExpression
    ;

// production #12
pragmaPADBITS :
    PADBITS '=' inPragmaExpression
    ;

// production #13
pragmaADDR :
    ADDR '=' inPragmaExpression
    ;

// production #14
pragmaREG :
    REG '=' inPragmaExpression
    ;

// production #15
pragmaPURITY :
    PURITY '=' inPragmaExpression /* values 0 .. 3 */
    ;

// production #16
variableAttrPragma :
    SINGLEASSIGN | VOLATILE
    {} /* make ANTLRworks display separate branches */
    ;

// production #17
pragmaFORWARD :
    FORWARD ( TYPE identList | procedureHeader )
    ;

// production #18
implDefinedPragma :
    implDefinedPragmaName ( '=' inPragmaExpression )?
    ;

// production #19
inPragmaExpression :
/* represents operator precedence level 1 */
    inPragmaSimpleExpression ( inPragmaRelOp inPragmaSimpleExpression )?
    ;

// fragment #19.1
inPragmaRelOp :
    '=' | '#' | '<' | '<=' | '>' | '>='
    {} /* make ANTLRworks display separate branches */
    ;

// production #20
inPragmaSimpleExpression :
/* represents operator precedence level 2 */
    ( '+' | '-' {})? inPragmaTerm ( addOp inPragmaTerm )*
    ;

// production #21
inPragmaTerm :
/* represents operator precedence level 3 */
    inPragmaFactorOrNegation ( inPragmaMulOp inPragmaFactorOrNegation )*
    ;

// fragment #21.1
inPragmaMulOp :
    '*' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #22
inPragmaFactorOrNegation :
/* represents operator precedence level 4 */
    NOT? inPragmaFactor
    ;

// production #23
inPragmaFactor :
    wholeNumber |
    /* constQualident is covered by inPragmaCompileTimeFunctionCall */
    '(' inPragmaExpression ')' | inPragmaCompileTimeFunctionCall
    ;

// alias #23.1
wholeNumber : NumericLiteral ;

// production #24
inPragmaCompileTimeFunctionCall :
    qualident ( '(' inPragmaExpression ( ',' inPragmaExpression )* ')' )?
    ;
    

// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 4 productions, 16 fragments, 1 alias

// production #1
ReservedWord :
    ALIAS AND ARRAY ASSOCIATIVE BEGIN BLUEPRINT BY CASE CONST DEFINITION
    DESCENDING DIV DO ELSE ELSIF END EXIT FOR FROM IF IMPLEMENTATION IMPORT
    IN INDETERMINATE LOOP MOD MODULE NOT OF OPAQUE OR PLACEHOLDERS POINTER
    PROCEDURE RECORD REPEAT RETURN SET THEN TO TYPE UNTIL VAR VARIADIC WHILE
    ;

// production #2
Ident :
    IdentLeadChar IdentTail?
    ;

fragment /* #2.1 */
IdentLeadChar :
    Letter | '_' | '$'
    ;

fragment /* #2.2 */
IdentTail:
    ( IdentLeadChar | Digit )+
    ;

// production #3
NumericLiteral :
    /* number literals starting with digit 0 ... */
    '0' (
        /* without prefix are real numbers */
        RealNumberTail |
        /* with prefix 0b are base-2 numbers */
        'b' Base2DigitSeq |
        /* with prefix 0x are base-16 numbers */
        'x' Base16DigitSeq |
        /* with prefix 0u are unicode code points */
        'u' Base16DigitSeq
         )?
    /* number literals starting with digits 1 to 9 ... */
    | '1'..'9' DecimalNumberTail? /* are always decimal numbers */
    ;

fragment /* #3.1 */
DecimalNumberTail :
    DigitSep? DigitSeq RealNumberTail? | RealNumberTail
    ;

fragment /* #3.2 */
RealNumberTail :
    '.' DigitSeq ( 'e' ( '+' | '-' {})? DigitSeq )?
    ;

fragment /* #3.3 */
DigitSeq :
    Digit+ ( DigitSep Digit+ )*
    ;

fragment /* #3.4 */
Base2DigitSeq :
    Base2Digit+ ( DigitSep Base2Digit+ )*
    ;

fragment /* #3.5 */
Base16DigitSeq :
    Base16Digit+ ( DigitSep Base16Digit+ )*
    ;

fragment /* #3.6 */
DigitSep : SINGLE_QUOTE {} /* make ANTLRworks display name, not literal */ ;

// production #4
StringLiteral :
    SingleQuotedString | DoubleQuotedString
    ;

fragment /* #4.1 */
SingleQuotedString :
    SINGLE_QUOTE
    ( QuotableCharacter | DOUBLE_QUOTE )*
    SINGLE_QUOTE
    ;

fragment /* #4.2 */
DoubleQuotedString :
    DOUBLE_QUOTE
    ( QuotableCharacter | SINGLE_QUOTE )*
    DOUBLE_QUOTE
    ;

fragment /* #4.3 */
QuotableCharacter :
    Digit | Letter | Space | QuotableGraphicChar | EscapedCharacter
    ;

fragment /* #4.4 */
Digit :
    Base2Digit | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.5 */
Base2Digit :
    '0' | '1'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.6 */
Base16Digit :
    Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.7 */
Letter :
    'A' .. 'Z' | 'a' .. 'z'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.8 */
Space : ' ' ;

fragment /* #4.9 */
QuotableGraphicChar :
    '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' |
    '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
    '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.10 */
EscapedCharacter :
    BACKSLASH ( '0' | 'n' | 't' | BACKSLASH | SINGLE_QUOTE | DOUBLE_QUOTE {})
    ;


// ---------------------------------------------------------------------------
// I G N O R E   S Y M B O L S
// ---------------------------------------------------------------------------
// 5 productions

// *** Whitespace ***

// production #1
Whitespace :
    ({} Space | ASCII_TAB) { $channel = HIDDEN; } /* ignore */
    ;

// *** Comments ***

// production #2
fragment
MultiLineComment :
    '(*'
    ( options { greedy=false; }: . )* /* anything other than '(*' or '*)' */
    MultiLineComment*
    '*)'
    ;	

// production #3
fragment
SingleLineComment :
    '//'
    ( options { greedy=false; }: . )* /* anything other than EOL */
    EndOfLine
    ;

// pseudo-procudion to make #2 and #3 hidden
Comment :
    MultiLineComment | SingleLineComment
    { $channel = HIDDEN; } /* ignore */
    ;

// production #4
fragment
EndOfLine :
    ASCII_LF | ASCII_CR (ASCII_LF{})?
    ;

// END OF FILE
