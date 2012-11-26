/* (C) 2009-2012 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Nov 25, 2012 */


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
// 45 reserved words, 21 pragma words

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

// *** Language Defined Pragma Words, 20 tokens ***

    ENDIF          = 'ENDIF';
    MSG            = 'MSG';
    INFO           = 'INFO';
    WARN           = 'WARN';
    ERROR          = 'ERROR';
    FATAL          = 'FATAL';
    FFI            = 'FFI';
    GENLIB         = 'GENLIB';
    INLINE         = 'INLINE';
    NOINLINE       = 'NOINLINE';
    ALIGN          = 'ALIGN';
    PADBITS        = 'PADBITS';
    ADDR           = 'ADDR';
    REG            = 'REG';
    PURE           = 'PURE';
    WEAK           = 'WEAK';
    STRONG         = 'STRONG';
    VOLATILE       = 'VOLATILE';
    ENCODING       = 'ENCODING';
    FORWARD        = 'FOWARD';

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
prototypeIdent : Ident ;

// alias #2.2
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
simpleProtoliteral :
    Ident
    ;

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
constBindableIdent : Ident ;

// alias #7.2
pervasiveType : Ident ;

// production #8
definitionOfModule :
    DEFINITION MODULE moduleIdent ( '[' prototypeIdent ']' )? ';'
    importList* definition*
    END moduleIdent '.'
    ;

// alias 8.1
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
    CONST ( ( '[' bindableIdent ']' )? constantDeclaration ';' )+ |
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
    '(' ( ( '+' typeIdent ) | Ident ) ( ',' ( ( '+' typeIdent ) | Ident ) )* ')'
    ;

// alias 17.1
enumTypeIdent : qualident ;

// alias 17.2
typeIdent : qualident ;

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
    // ( autocast? ARRAY OF )? typeIdent
    ( 'CAST'? ARRAY OF )? typeIdent
    ;

// alias #27.1
autocast : Ident ; // CAST

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

// alias #32.1
bindableIdent : Ident ;
// TMIN, TMAX, TLIMIT, ABS, NEG, ODD, COUNT, LENGTH, NEW, DISPOSE, RETAIN, RELEASE, SXF, VAL

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
    CASE expression OF ( '|' case )+ ( ELSE statementSequence )? END
    ;

// production #42
case :
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
    simpleExpression ( RelOp simpleExpression )?
    ;

// fragment #51.1
fragment RelOp :
    '=' | '#' | '<' | '<=' | '>' | '>=' | IN
    {}
    ;

// production #52
simpleExpression :
    ( '+' | '-' {})? term ( AddOp term )*
    ;

// fragment #52.1
fragment AddOp :
    '+' | '-' | OR
    {} // make ANTLRworks display separate branches
	;

// production #53
term :
    factor ( MulOp factor )*
    ;

// fragment #53.1
fragment MulOp :
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
      volatileAttributePragma |
      pureFunctionAttributePragma |
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

constQualident : qualident ; // no type and no variable identifiers

implDefinedPragmaName : Ident ; // lowercase or mixed case only

condtitionalCompilationPragma :
    ( IF | ELSIF {}) inPragmaExpression | ELSE | ENDIF
    ;

characterEncodingPragma : 
    ENCODING '=' String // "ASCII" or "UTF8"
    ( ':' codePointSample ( ',' codePointSample )* )?
    ;

codePointSample :
    quotedCharacterLiteral '=' CharacterCodeLiteral
    ;

quotedCharacterLiteral : String ; // single character only

libraryTemplateExpansionPragma :
    GENLIB moduleIdent FROM template ':' templateParamList
    ;

template : Ident ;

templateParamList :
    templateParam ( ',' templateParam )*
    ;

templateParam :
    placeholder '=' replacement
    ;

placeholder : Ident ;

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

volatileAttributePragma :
    VOLATILE
    ;

pureFunctionAttributePragma :
    PURE '=' ( WEAK | STRONG {})
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
    ( '+' | '-' {})? inPragmaTerm ( AddOp inPragmaTerm )*
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

Number :
    DecimalNumber | Base2Number | Base16Number | CharacterCodeLiteral
    ;

fragment
DecimalNumber :
    DigitSeq '.' DigitSeq
    'E' ( '+' | '-' {})? DigitSeq
    ;

fragment
DigitSeq :
    DigitGroup ( SINGLE_QUOTE DigitGroup )*
    ;

fragment
DigitGroup :
    Digit+
    ;

fragment
Base2Number :
    '0b' Base2DigitSeq
    ;

fragment
Base2DigitSeq :
    Base2DigitGroup ( SINGLE_QUOTE Base2DigitGroup )*
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
Base16Number :
    '0x' Base16DigitSeq
    ;

fragment
Base16DigitSeq :
    Base16DigitGroup ( SINGLE_QUOTE Base16DigitGroup )*
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

fragment
CharacterCodeLiteral :
    '0u' Base16DigitSeq
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
    Space | ASCII_TAB
    {} // make ANTLRworks display separate branches
    { $channel = HIDDEN; } // ignore
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
    ASCII_LF ASCII_CR? | ASCII_CR ASCII_LF?
    ;

// END OF FILE
