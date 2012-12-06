/* (C) 2009-2012 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Dec 5, 2012 */


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
    PROTOTYPE      = 'PROTOTYPE';
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

// *** Language Defined Pragma Words, 18 tokens ***

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
    prototype | definitionOfModule | IMPLEMENTATION? programModule
    ;

// production #2
prototype :
    PROTOTYPE prototypeIdent '[' requiredConformance ']' ';'
    ( PLACEHOLDERS identList ';' )?
    requiredTypeDefinition
    ( requiredBinding ';' )*
    END prototypeIdent '.'
    ;

// alias #2.1
prototypeIdent : Ident ;

// alias #2.2
requiredConformance : prototypeIdent ;

// production #3
programModule :
    MODULE moduleIdent ';'
    importList* block moduleIdent '.'
    ;

// alias 3.1
moduleIdent : Ident ;

// production #4
definitionOfModule :
    DEFINITION MODULE moduleIdent ( '[' prototypeIdent ']' )? ';'
    importList* definition*
    END moduleIdent '.'
    ;

// production #5
requiredTypeDefinition :
    TYPE '=' permittedTypeDefinition ( '|' permittedTypeDefinition )*
    ( ':=' protoliteral ( '|' protoliteral )* )?
    ;

// production #6
permittedTypeDefinition :
    RECORD | OPAQUE RECORD?
    ;

// production #7
protoliteral :
    simpleProtoliteral | structuredProtoliteral
    ;

// alias #7.1
simpleProtoliteral : Ident ; /* CHAR, INTEGER or REAL */

// production #8
structuredProtoliteral :
    '{' ( VARIADIC OF simpleProtoliteral ( ',' simpleProtoliteral )* |
    structuredProtoliteral ( ',' structuredProtoliteral )* ) '}'
    ;

// production #9
requiredBinding :
    CONST '[' constBindableIdent ']' ':' pervasiveType |
    procedureHeader
    ;

// alias #9.1
constBindableIdent : /* Ident */ TSIG | TEXP
    {} /* make ANTLRworks display separate branches */
    ;

// alias #9.2
pervasiveType : Ident ;


// *** Import Lists, Blocks, Declarations and Definitions ***

// production #10
importList :
    ( FROM moduleIdent IMPORT ( identList | '*' ) |
    IMPORT Ident '+'? ( ',' Ident '+'? )* ) ';'
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

// production #11
block :
    declaration*
    ( BEGIN statementSequence )? END
    ;

// production #12
declaration :
    CONST ( constantDeclaration ';' )+ |
    TYPE ( Ident '=' type ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureDeclaration ';'
    ;

// production #13
definition :
    CONST ( ( '[' constBindableIdent ']' )? constantDeclaration ';' )+ |
    TYPE ( Ident '=' ( type | OPAQUE recordType? ) ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';'
    ;

// production #14
constantDeclaration :	
    Ident '=' constExpression /* no type identifiers */
    ;

// alias #14.1
constExpression : expression ;


// *** Type Declarations ***

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
    '(' ( ( '+' enumTypeIdent ) | Ident ) ( ',' ( ( '+' enumTypeIdent ) | Ident ) )* ')'
    ;

// alias 17.1
enumTypeIdent : typeIdent ;

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
    INDETERMINATE Ident ':' ARRAY discriminantField OF typeIdent
    ;

// alias #20.1
discriminantField : Ident ;

// production #21
setType :	
    SET OF ( enumTypeIdent | '(' identList ')' )
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

// *** Procedure Declarations ***

// production #30
procedureDeclaration :
    procedureHeader ';' block Ident
    ;

// production #31
procedureHeader :
    PROCEDURE ( '[' bindableEntity ']' )?
    Ident ( '(' formalParamList ')' )?
    ( ':' returnedType )?
    ;

// production #32
bindableEntity :
    DIV | MOD | FOR | DESCENDING |
    '::' | ':=' | '?' | '!' | '~' | '+' | '-' | '*' | '/' | '=' | '<' | '>' |
    bindableIdent
    ;

// fragment #32.1
// both an identifier and a reserved word
// resolve using Schroedinger's Token
bindableIdent :
    ABS | NEG | ODD | COUNT | LENGTH | NEW | DISPOSE | RETAIN | RELEASE |
    TLIMIT | TMIN | TMAX | SXF | VAL
    {} /* make ANTLRworks display separate branches */
    ;

// *** Formal Parameters ***

// production #33
formalParamList :
    formalParams ( ';' formalParams )*
    ;

// production #34
formalParams :
    simpleFormalParams | variadicFormalParams
    ;

// production #35
simpleFormalParams :
    ( CONST | VAR {})? identList ':' simpleFormalType
    ;

// production #36
variadicFormalParams :
    VARIADIC ( '[' variadicTerminator ']' )? OF
    ( simpleFormalType | '{' simpleFormalParams ( ';' simpleFormalParams )* '}' )
    ;

// alias #36.1
variadicTerminator : constExpression ;

// *** Statements ***

// production #37
statement :
    ( assignmentOrProcedureCall | ifStatement | caseStatement |
      whileStatement | repeatStatement | loopStatement |
      forStatement | RETURN expression? | EXIT )?
    ;

// production #38
statementSequence :
    statement ( ';' statement )*
    ;

// production #39
assignmentOrProcedureCall :
    designator ( ':=' expression | '++' | '--' | actualParameters )?
    ;

// production #40
ifStatement :
    IF expression THEN statementSequence
    ( ELSIF expression THEN statementSequence )*
    ( ELSE statementSequence )?
    END
    ;

// production #41
caseStatement :
    CASE expression OF ( '|' case777 )+ ( ELSE statementSequence )? END
    ;

// production #42
case777 :
/* ANTLR has a design flaw in that it cannot handle any rule identifiers that
   coincide with reserved words of the language it uses to generate the parser */
    caseLabels ( ',' caseLabels )* ':' statementSequence
    ;

// production #43
caseLabels :
    constExpression ( '..' constExpression )?
    ;

// production #44
whileStatement :
    WHILE expression DO statementSequence END
    ;

// production #45
repeatStatement :
    REPEAT statementSequence UNTIL expression
    ;

// production #46
loopStatement :
    LOOP statementSequence END
    ;

// production #47
forStatement :
    FOR DESCENDING? controlVariable
    IN ( designator | range OF typeIdent )
    DO statementSequence END
    ;

// alias #47.1
controlVariable : Ident ;

// production #48
designator :
    qualident designatorTail?
    ;

// production #49
designatorTail :
    ( ( '[' expressionList ']' | '^' ) ( '.' Ident )* )+
    ;

// *** Expressions ***

// production #50
expressionList :
    expression ( ',' expression )*
    ;

// production #51
expression :
    simpleExpression ( relOp simpleExpression )?
    ;

// fragment #51.1
relOp :
    '=' | '#' | '<' | '<=' | '>' | '>=' | IN
    {} /* make ANTLRworks display separate branches */
    ;

// production #52
simpleExpression :
    ( '+' | '-' {} /* make ANTLRworks display separate branches */ )?
    term ( addOp term )*
    ;

// fragment #52.1
addOp :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
	;

// production #53
term :
    factor ( mulOp factor )*
    ;

// fragment #53.1
mulOp :
    '*' | '/' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #54
factor :
    ( NumericLiteral | StringLiteral | structuredValue |
    designatorOrFunctionCall | '(' expression ')' )
    ( '::' typeIdent )? | NOT factor
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
// 23 productions

// *** Pragmas ***

// production #1
pragma :
    '<*'
    ( pragmaMSG | pragmaIF | pragmaENCODING | pragmaGENLIB | pragmaFFI |
      pragmaINLINE | pragmaALIGN | pragmaPADBITS | pragmaADDR | pragmaREG |
      pragmaPURITY | pragmaVOLATILE | implDefinedPragma | pragmaFORWARD )
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
pragmaVOLATILE :
    VOLATILE
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
    inPragmaSimpleExpression ( inPragmaRelOp inPragmaSimpleExpression )?
    ;

// fragment #19.1
inPragmaRelOp :
    '=' | '#' | '<' | '<=' | '>' | '>='
    {} /* make ANTLRworks display separate branches */
    ;

// production #20
inPragmaSimpleExpression :
    ( '+' | '-' {})? inPragmaTerm ( addOp inPragmaTerm )*
    ;

// production #21
inPragmaTerm :
    inPragmaFactor ( inPragmaMulOp inPragmaFactor )*
    ;

// fragment #21.1
inPragmaMulOp :
    '*' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #22
inPragmaFactor :
    wholeNumber |
    /* constQualident is covered by inPragmaCompileTimeFunctionCall */
    inPragmaCompileTimeFunctionCall |
    '(' inPragmaExpression ')' |
    NOT inPragmaFactor
    ;

// alias #22.1
wholeNumber : NumericLiteral ;

// production #23
inPragmaCompileTimeFunctionCall :
    qualident ( '(' inPragmaExpression ( ',' inPragmaExpression )* ')' )?
    ;
    

// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 4 productions, 16 fragments, 1 alias

// production #1
ReservedWord :
    ALIAS AND ARRAY ASSOCIATIVE BEGIN BY CASE CONST DEFINITION DESCENDING?
    DIV DO ELSE ELSIF END EXIT FOR FROM IF IMPLEMENTATION IMPORT IN?
    INDETERMINATE LOOP MOD MODULE NOT OF OPAQUE OR PLACEHOLDERS POINTER?
    PROCEDURE PROTOTYPE RECORD REPEAT RETURN SET THEN TO TYPE UNTIL VAR?
    VARIADIC WHILE
    ;

// production #2
Ident :
    IdentLeadChar IdentTailChar*
    ;

fragment /* #2.1 */
IdentLeadChar :
    Letter | '_' | '$'
    ;

fragment /* #2.2 */
IdentTailChar :
    Letter | '_' | '$' | Digit
    ;

// production #3
NumericLiteral :
    /* number literals starting with digit 0 ... */
    '0' (
        /* without prefix are decimal numbers */
        DecimalNumberTail |
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
    DigitSep? DigitSeq ( '.' DigitSeq ( 'e' ( '+' | '-' {})? DigitSeq )? )?
    ;

fragment /* #3.2 */
DigitSeq :
    Digit+ ( DigitSep Digit+ )*
    ;

fragment /* #3.3 */
Base2DigitSeq :
    Base2Digit+ ( DigitSep Base2Digit+ )*
    ;

fragment /* #3.4 */
Base16DigitSeq :
    Base16Digit+ ( DigitSep Base16Digit+ )*
    ;

fragment /* #3.5 */
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
    ( ASCII_LF ASCII_CR? | ASCII_CR ASCII_LF? )
    ;

// END OF FILE
