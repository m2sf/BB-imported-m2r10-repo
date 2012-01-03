/* (C) 2009-2012 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Dec 3, 2012 */


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
// 45 reserved words, 20 pragma words

tokens {
	
// *** Reserved Words, 45 tokens ***

    ALIAS          = 'ALIAS';
    AND            = 'AND';
    ARRAY          = 'ARRAY';
    ASSOCIATIVE    = 'ASSOCIATIVE';
    BEGIN          = 'BEGIN';
    BY             = 'BY';
    CASE           = 'CASE';
    CONST          = 'CONST';
    DEFINITION     = 'DEFINITION';
    DESCENDING     = 'DESCENDING';
    DIV            = 'DIV';
    DO             = 'DO';
    ELSE           = 'ELSE';
    ELSIF          = 'ELSIF';
    END            = 'END';
    EXIT           = 'EXIT';
    FOR            = 'FOR';
    FROM           = 'FROM';
    IF             = 'IF';
    IMPLEMENTATION = 'IMPLEMENTATION';
    IMPORT         = 'IMPORT';
    IN             = 'IN';
    INDETERMINATE  = 'INDETERMINATE';
    LOOP           = 'LOOP';
    MOD            = 'MOD';
    MODULE         = 'MODULE';
    NOT            = 'NOT';
    OF             = 'OF';
    OPAQUE         = 'OPAQUE';
    OR             = 'OR';
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
    TYPE           = 'TYPE';
    UNTIL          = 'UNTIL';
    VAR            = 'VAR';
    VARIADIC       = 'VARIADIC';
    WHILE          = 'WHILE';

// *** CAST ***

//  CAST is both an Identifier and a Reserved Word
//  Ambiguity is resolvable using the Schroedinger's Token technique

    CAST           = 'CAST';

// *** Bindable Identifiers ***

//  Bindable Identifiers are both Identifiers and Reserved Words
//  Ambiguity is resolvable using the Schroedinger's Token technique

    ABS            = 'ABS';      /* bindableIdent */
    NEG            = 'NEG';      /* bindableIdent */
    ODD            = 'ODD';      /* bindableIdent */
    COUNT          = 'COUNT';    /* bindableIdent */
    LENGTH         = 'LENGTH';   /* bindableIdent */
    NEW            = 'NEW';      /* bindableIdent */
    DISPOSE        = 'DISPOSE';  /* bindableIdent */
    RETAIN         = 'RETAIN';   /* bindableIdent */
    RELEASE        = 'RELEASE';  /* bindableIdent */
    TLIMIT         = 'TLIMIT';   /* bindableIdent */
    TMAX           = 'TMAX';     /* constBindableIdent */
    TMIN           = 'TMIN';     /* constBindableIdent */
    SXF            = 'SXF';      /* bindableIdent */
    VAL            = 'VAL';      /* bindableIdent */

// *** Language Defined Pragma Words, 27 tokens ***

//  Pragma Words are Reserved Words only within pragmas
//  Ambiguity is resolvable using the Schroedinger's Token technique

    MSG            = 'MSG';      /* pragma only */
    INFO           = 'INFO';     /* pragma only */
    WARN           = 'WARN';     /* pragma only */
    ERROR          = 'ERROR';    /* pragma only */
    FATAL          = 'FATAL';    /* pragma only */
    IF             = 'IF';       /* RW and pragma */
    ELSIF          = 'ELSIF';    /* RW and pragma */
    ELSE           = 'ELSE';     /* RW and pragma */
    ENDIF          = 'ENDIF';    /* pragma only */
    ENCODING       = 'ENCODING'; /* pragma only */
    GENLIB         = 'GENLIB';   /* pragma only */
    FROM           = 'FROM';     /* RW and pragma */
    FFI            = 'FFI';      /* pragma only */
    INLINE         = 'INLINE';   /* pragma only */
    NOINLINE       = 'NOINLINE'; /* pragma only */
    ALIGN          = 'ALIGN';    /* pragma only */
    PADBITS        = 'PADBITS';  /* pragma only */
    ADDR           = 'ADDR';     /* pragma only */
    REG            = 'REG';      /* pragma only */
    PURITY         = 'PURITY';   /* pragma only */
    VOLATILE       = 'VOLATILE'; /* pragma only */
    FORWARD        = 'FOWARD';   /* pragma only */
    TYPE           = 'TYPE';     /* RW and pragma */
    DIV            = 'DIV';      /* RW and pragma */
    MOD            = 'MOD';      /* RW and pragma */
    AND            = 'AND';      /* RW and pragma */
    NOT            = 'NOT';      /* RW and pragma */

// *** Special Characters, 3 tokens ***

    BACKSLASH      = '\\';
    SINGLE_QUOTE   = '\'' ;
    DOUBLE_QUOTE   = '\"' ;

// *** Ignore Characters, 3 tokens ***

    ASCII_TAB      = '\t';
    ASCII_LF       = '\n';
    ASCII_CR       = '\r';
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
fragment
prototypeIdent : Ident ;

// alias #2.2
fragment
requiredConformance : prototypeIdent ;

// production #3
requiredTypeDefinition :
    TYPE '=' permittedTypeDefinition ( '|' permittedTypeDefinition )*
    ( ':=' protoliteral ( '|' protoliteral )* )?
    ;

// production #4
permittedTypeDefinition :
    RECORD | OPAQUE RECORD?
    ;

// production #5
protoliteral :
    simpleProtoliteral | structuredProtoliteral
    ;

// alias #5.1
fragment
simpleProtoliteral : Ident ;

// production #6
structuredProtoliteral :
    '{' ( VARIADIC OF simpleProtoliteral ( ',' simpleProtoliteral )* |
    structuredProtoliteral ( ',' structuredProtoliteral )* ) '}'
    ;

// production #7
requiredBinding :
    CONST '[' constBindableIdent ']' ':' pervasiveType |
    procedureHeader
    ;

// alias #7.1
fragment
constBindableIdent : TMAX | TMIN {} ;

// alias #7.2
fragment
pervasiveType : Ident ;

// production #8
definitionOfModule :
    DEFINITION MODULE moduleIdent ( '[' prototypeIdent ']' )? ';'
    importList* definition*
    END moduleIdent '.'
    ;

// alias 8.1
fragment
moduleIdent : Ident ;

// production #9
programModule :
    MODULE moduleIdent ';'
    importList* block moduleIdent '.'
    ;

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
    Ident '=' constExpression // no type identifiers
    ;

// alias #14.1
constExpression : expression ;

// production #15
type :
    (( ALIAS | range ) OF )? typeIdent | enumerationType |
    arrayType | recordType | setType | pointerType | procedureType
    ;

// production #16
range :
    '[' constExpression '..' constExpression ']'
    ;

// production #17
enumerationType :
    '(' ( ( '+' enumTypeIdent ) | Ident ) ( ',' ( ( '+' enumTypeIdent ) | Ident ) )* ')'
    ;

// alias 17.1
fragment
enumTypeIdent : qualident ;

// alias 17.2
fragment
typeIdent : qualident ;

// production #18
arrayType :
    ( ARRAY componentCount ( ',' componentCount )* |
      ASSOCIATIVE ARRAY ) OF typeIdent
    ;

// alias #18.1
fragment
componentCount : constExpression ;

// production #19
recordType :
    RECORD ( fieldList ( ';' fieldList )* indeterminateField? |
    '(' baseType ')' fieldList ( ';' fieldList )* ) END
    ;

// aliase #19.1
fragment
fieldList : variableDeclaration ;

// aliase #19.2
fragment
baseType : typeIdent ;

// production #20
indeterminateField :
    INDETERMINATE Ident ':' ARRAY discriminantField OF typeIdent
    ;

// alias #20.1
fragment
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
fragment
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

// production #29
variableDeclaration :
    identList ':' ( range OF )? typeIdent
    ;

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
    DIV | MOD | IN | FOR | DESCENDING |
    '::' | ':=' | '?' | '!' | '~' | '+' | '-' | '*' | '/' | '=' | '<' | '>' |
    bindableIdent
    ;

// token #32.1
// both an identifier and a reserved word
// resolve using Schroedinger's Token
fragment
bindableIdent :
    ABS | NEG | ODD | COUNT | LENGTH | NEW | DISPOSE | RETAIN | RELEASE |
    SXF | VAL
    {} // make ANTLRworks display alternatives as separate branches
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
fragment
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
fragment
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
fragment
relOp :
    '=' | '#' | '<' | '<=' | '>' | '>=' | IN
    {} // make ANTLRworks display separate branches
    ;

// production #52
simpleExpression :
    ( '+' | '-' {})? term ( addOp term )*
    ;

// fragment #52.1
fragment
addOp :
    '+' | '-' | OR
    {} // make ANTLRworks display separate branches
	;

// production #53
term :
    factor ( mulOp factor )*
    ;

// fragment #53.1
fragment
mulOp :
    '*' | '/' | DIV | MOD | AND
    {} // make ANTLRworks display separate branches
    ;

// production #54
factor :
    (( Number | String | structuredValue |
    designatorOrFunctionCall | '(' expression ')' )
    ( '::' typeIdent )? ) | NOT factor
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
// P R A G M A   S Y M B O L S
// ---------------------------------------------------------------------------
// 22 productions

// *** Pragmas ***

pragma :
    '<*'
    ( compileTimeMessagePragma |
      condtitionalCompilationPragma |
      characterEncodingPragma |
      libraryTemplateExpansionPragma |
      foreignFunctionInterfacePragma |
      procedureInliningPragma |
      memoryAlignmentPragma |
      bitPaddingPragma |
      memoryMappingPragma |
      registerMappingPragma |
      purityAttributePragma |
      volatileAttributePragma |
      implementationDefinedPragma |
      forwardDeclarationPragma )
    '*>'
    ;

compileTimeMessagePragma :
    MSG '=' ( INFO | WARN | ERROR | FATAL {}) ':'
    compileTimeMsgComponent ( ',' compileTimeMsgComponent )*
    ;

compileTimeMsgComponent :
    String | constQualident |
    '?' ( ALIGN | ENCODING | implDefinedPragmaName )
    ;

fragment
constQualident : qualident ; // no type and no variable identifiers

fragment
implDefinedPragmaName : Ident ; // lowercase or mixed case only

condtitionalCompilationPragma :
    ( IF | ELSIF {}) inPragmaExpression | ELSE | ENDIF
    ;

characterEncodingPragma : 
    ENCODING '=' String // "ASCII" or "UTF8"
    ( ':' codePointSample ( ',' codePointSample )* )?
    ;

codePointSample :
    quotedCharacterLiteral '=' characterCodeLiteral
    ;

fragment
quotedCharacterLiteral : String ; // single character only

fragment
characterCodeLiteral : Number ; // unicode code points only

libraryTemplateExpansionPragma :
    GENLIB moduleIdent FROM template ':' templateParamList
    ;

fragment
template : Ident ;

templateParamList :
    templateParam ( ',' templateParam )*
    ;

templateParam :
    placeholder '=' replacement
    ;

fragment
placeholder : Ident ;

fragment
replacement : String ;

foreignFunctionInterfacePragma :
    FFI '=' String // "C" or "Fortran"
    ;

procedureInliningPragma :
    INLINE | NOINLINE
    {} // make ANTLRworks display separate branches
    ;

memoryAlignmentPragma :
    ALIGN '=' inPragmaExpression
    ;

bitPaddingPragma :
    PADBITS '=' inPragmaExpression
    ;

memoryMappingPragma :
    ADDR '=' inPragmaExpression
    ;

registerMappingPragma :
    REG '=' inPragmaExpression
    ;

purityAttributePragma :
    PURITY '=' inPragmaExpression /* values 0 .. 3 */
    ;

volatileAttributePragma :
    VOLATILE
    ;

implementationDefinedPragma :
    implDefinedPragmaName ( '=' inPragmaExpression )?
    ;

inPragmaExpression :
    inPragmaSimpleExpression ( inPragmaRelOp inPragmaSimpleExpression )?
    ;

fragment
inPragmaRelOp :
    '=' | '#' | '<' | '<=' | '>' | '>='
    {} // make ANTLRworks display separate branches
    ;

inPragmaSimpleExpression :
    ( '+' | '-' {})? inPragmaTerm ( addOp inPragmaTerm )*
    ;

inPragmaTerm :
    inPragmaFactor ( inPragmaMulOp inPragmaFactor )*
    ;

fragment
inPragmaMulOp :
    '*' | DIV | MOD | AND
    {} // make ANTLRworks display separate branches
    ;

inPragmaFactor :
    wholeNumber |
    // constQualident or inPragmaCompileTimeFunctionCall
    qualident ( '(' inPragmaExpression ( ',' inPragmaExpression )* ')' )? |
    '(' inPragmaExpression ')' |
    NOT inPragmaFactor
    ;

fragment
wholeNumber : Number ;

forwardDeclarationPragma :
    FORWARD ( TYPE identList | procedureHeader )
    ;


// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 8 productions

// production #1
Ident :
    IdentLeadChar IdentTailChar*
    ;

fragment
IdentLeadChar :
    Letter | '_' | '$'
    ;

fragment
IdentTailChar :
    Letter | '_' | '$' | Digit
    ;

fragment
Letter :
    'A' .. 'Z' | 'a' .. 'z'
    {} // make ANTLRworks display separate branches
    ;

fragment 
Digit :
    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    {} // make ANTLRworks display separate branches
    ;

/* production #2 */
Number :
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
         )
    /* number literals starting with digits 1 to 9 ... */
    | '1'..'9' DecimalNumberTail /* are always decimal numbers */
    ;

fragment
DecimalNumberTail :
    DigitSep? DigitSeq? ( '.' DigitSeq ( 'e' ( '+' | '-' {})? DigitSeq )? )?
    ;

fragment
DigitSeq :
    DigitGroup ( DigitSep DigitGroup )*
    ;

fragment
DigitGroup :
    Digit+
    ;

fragment
DigitSep : SINGLE_QUOTE {};


fragment
Base2DigitSeq :
    Base2DigitGroup ( DigitSep Base2DigitGroup )*
    ;

fragment
Base2DigitGroup :
    Base2Digit+
    ;

fragment
Base2Digit :
    '0' | '1'
    {} // make ANTLRworks display separate branches
    ;

fragment
Base16DigitSeq :
    Base16DigitGroup ( DigitSep Base16DigitGroup )*
    ;

fragment
Base16DigitGroup :
    Base2Digit+
    ;

fragment
Base16Digit :
    Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
    {} // make ANTLRworks display separate branches
    ;

// production #3
String :
    SingleQuotedString | DoubleQuotedString
    ;

fragment
SingleQuotedString :
    SINGLE_QUOTE
    ( Character | DOUBLE_QUOTE )*
    SINGLE_QUOTE
    ;

fragment
DoubleQuotedString :
    DOUBLE_QUOTE
    ( Character | SINGLE_QUOTE )*
    DOUBLE_QUOTE
    ;

fragment
Character :
    Digit | Letter | Space |
    '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' |
    '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
    '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' |
    EscapeSequence
    ;

fragment
EscapeSequence :
    BACKSLASH ( '0' | 'n' | 't' | BACKSLASH | SINGLE_QUOTE | DOUBLE_QUOTE {})
    ;

fragment
Space : ' ';


// ---------------------------------------------------------------------------
// I G N O R E   S Y M B O L S
// ---------------------------------------------------------------------------
// 5 productions

// *** Whitespace ***

// production #1
Whitespace :
    ({} Space | ASCII_TAB) { $channel = HIDDEN; } // ignore
    ;

// *** Comments ***

// production #2
Comment :
    MultiLineComment | SingleLineComment
    { $channel = HIDDEN; } // ignore
    ;

// production #3
fragment
MultiLineComment :
    '(*'
    ( options { greedy=false; }: . )* // anything other than '(*' or '*)'
    MultiLineComment*
    '*)'
    ;	

// production #5
fragment
SingleLineComment :
    '//'
    ( options { greedy=false; }: . )* // anything other than EOL
    EndOfLine
    ;

// production #5
fragment
EndOfLine :
    ( ASCII_LF ASCII_CR? | ASCII_CR ASCII_LF? )
    ;

// END OF FILE
