/* (C) 2009-2014 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Oct 31, 2014 */


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
// 45 reserved words, 26 identifiers, 23 pragma symbols

tokens {
	
// *** Reserved Words, 45 tokens ***

    ALIAS          = 'ALIAS';
    AND            = 'AND';            /* also a RW within pragmas */
    ARGLIST        = 'ARGLIST';
    ARRAY          = 'ARRAY';
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
    GENLIB         = 'GENLIB';
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
    POINTER        = 'POINTER';
    PROCEDURE      = 'PROCEDURE';
    RECORD         = 'RECORD';
    REFERENTIAL    = 'REFERENTIAL';
    REPEAT         = 'REPEAT';
    RETURN         = 'RETURN';
    SET            = 'SET';
    THEN           = 'THEN';
    TO             = 'TO';
    TYPE           = 'TYPE';
    UNTIL          = 'UNTIL';
    VAR            = 'VAR';
    WHILE          = 'WHILE';

// *** Dual-Use RW-Identifiers, 2 tokens ***

//  CAST and NIL are both identifiers and Reserved Words
//  Ambiguity is resolvable using the Schroedinger's Token technique

    CAST           = 'CAST';           /* RW within formal parameter */
    NIL            = 'NIL';            /* RW within blueprint */

// *** Bindable Identifiers, 28 tokens ***

//  Bindable Identifiers are both Identifiers and Reserved Words
//  Ambiguity is resolvable using the Schroedinger's Token technique

    TLIMIT         = 'TLIMIT';         /* RW within blueprint reqConst */
    TSIGNED        = 'TSIGNED';        /* RW within blueprint reqConst */
    TBASE          = 'TBASE';          /* RW within blueprint reqConst */
    TPRECISION     = 'TPRECISION';     /* RW within blueprint reqConst */
    TMINEXP        = 'TMINEXP';        /* RW within blueprint reqConst */
    TMAXEXP        = 'TMAXEXP';        /* RW within blueprint reqConst */

    ABS            = 'ABS';            /* RW within procedure header */
    NEG            = 'NEG';            /* RW within procedure header */
    DUP            = 'DUP';            /* RW within procedure header */
    COPY           = 'COPY';           /* RW within procedure header */
    COUNT          = 'COUNT';          /* RW within procedure header */
    LENGTH         = 'LENGTH';         /* RW within procedure header */
    CONCAT         = 'CONCAT';         /* RW within procedure header */
    STORE          = 'STORE';          /* RW within procedure header */
    RETRIEVE       = 'RETRIEVE';       /* RW within procedure header */
    INSERT         = 'INSERT';         /* RW within procedure header */
    REMOVE         = 'REMOVE';         /* RW within procedure header */
    NEW            = 'NEW';            /* RW within procedure header */
    RETAIN         = 'RETAIN';         /* RW within procedure header */
    RELEASE        = 'RELEASE';        /* RW within procedure header */
    SUBSET         = 'SUBSET';         /* RW within procedure header */
    READ           = 'READ';           /* RW within procedure header */
    WRITE          = 'WRITE';          /* RW within procedure header */
    WRITEF         = 'WRITEF';         /* RW within procedure header */
    TMAX           = 'TMAX';           /* RW within procedure header */
    TMIN           = 'TMIN';           /* RW within procedure header */
    SXF            = 'SXF';            /* RW within procedure header */
    VAL            = 'VAL';            /* RW within procedure header */

// *** Reserved Words of the Pragma Language, 23 tokens ***

//  Symbols that are reserved words only within pragmas

    MSG            = 'MSG';            /* RW within pragma only */
    INFO           = 'INFO';           /* RW within pragma only */
    WARN           = 'WARN';           /* RW within pragma only */
    ERROR          = 'ERROR';          /* RW within pragma only */
    FATAL          = 'FATAL';          /* RW within pragma only */
    ENDIF          = 'ENDIF';          /* RW within pragma only */
    INLINE         = 'INLINE';         /* RW within pragma only */
    NOINLINE       = 'NOINLINE';       /* RW within pragma only */
    NORETURN       = 'NORETURN';       /* RW within pragma only */
    PTW            = 'PTW';            /* RW within pragma only */
    FORWARD        = 'FORWARD';        /* RW within pragma only */    
    ENCODING       = 'ENCODING';       /* RW within pragma only */
    ALIGN          = 'ALIGN';          /* RW within pragma only */
    PADBITS        = 'PADBITS';        /* RW within pragma only */
    PURITY         = 'PURITY';         /* RW within pragma only */
    SINGLEASSIGN   = 'SINGLEASSIGN' ;  /* RW within pragma only */
    LOWLATENCY     = 'LOWLATENCY';     /* RW within pragma only */
    VOLATILE       = 'VOLATILE';       /* RW within pragma only */
    DEPRECATED     = 'DEPRECATED';     /* RW within pragma only */
    GENERATED      = 'GENERATED';      /* RW within pragma only */
    ADDR           = 'ADDR';           /* RW within pragma only */
    FFI            = 'FFI';            /* RW within pragma only */
    FFIDENT        = 'FFIDENT';        /* RW within pragma only */

// *** RWs for Optional Language Facilities, 2 tokens ***
/*
    ASM            = 'ASM';
    REG            = 'REG';
*/
// *** Special Characters, 3 tokens ***

    BACKSLASH      = '\\';  /*\*/      /* for readability */
    SINGLE_QUOTE   = '\'' ; /*'*/      /* for readability */
    DOUBLE_QUOTE   = '\"' ; /*"*/      /* for readability */

// *** Ignore Characters, 3 tokens ***

    ASCII_TAB      = '\t';             /* for readability */
    ASCII_LF       = '\n';             /* for readability */
    ASCII_CR       = '\r';             /* for readability */
}


// ---------------------------------------------------------------------------
// N O N - T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 64 productions

// *** Compilation Units ***

// production #1
compilationUnit :	
      IMPLEMENTATION? programModule | definitionModule | blueprint
    ;

// production #2
programModule :
    MODULE moduleIdent ';'
    importList* block moduleIdent '.'
    ;

// alias 2.1
moduleIdent : Ident ;

// production #3
definitionModule :
    DEFINITION MODULE moduleIdent
    ( '[' blueprintToObey ']' )? ( FOR typeToExtend )? ';'
    importList* definition*
    END moduleIdent '.'
    ;

// alias #3.1
blueprintToObey : blueprintIdent ;

// alias #3.2
blueprintIdent : Ident ;

// alias #3.3
typeToExtend : Ident ;

// production #4
blueprint :
    BLUEPRINT blueprintIdent
    ( '[' blueprintToRefine ']' )? ( FOR blueprintForTypeToExtend )? ';'
    ( REFERENTIAL identList ';' )? moduleTypeRequirement ';'
    ( constOrTypeOrProcRequirement ';' )*
    END blueprintIdent '.'
    ;

// alias #4.1
blueprintToRefine : blueprintIdent ;

// alias #4.2
blueprintForTypeToExtend : blueprintIdent ;

// production #5
moduleTypeRequirement :
    MODULE TYPE
      ( '=' ( OPAQUE | RECORD | '*' )
        ( ':=' protoLiteral ( '|' protoLiteral )* )? | ':' NIL )
    ;

// fragment #5.1
protoLiteral :
    simpleProtoLiteral | structuredProtoLiteral
    ;

// alias #5.2
simpleProtoLiteral : Ident;

// production #6
structuredProtoLiteral :
    '{' ( protoLiteralList |
        ARGLIST itemCount? OF
          ( simpleProtoLiteral | '{' protoLiteralList '}' ) ) '}'
    ;

// alias #6.1
protoLiteralList : identList;

// alias #6.2
itemCount : constIdent;

// alias #6.3
constIdent : Ident ; /* only identifiers of constants */

// production #7
constOrTypeOrProcRequirement :
    ( boolConstIdent '->' )?
    ( constRequirement | procedureRequirement | TYPE Ident '=' procedureType )
    ;

// production #8
constRequirement :
    CONST (
    ( simpleConstRequirement |
      '[' constBindableProperty ']' ( simpleConstRequirement | ':' NIL ) )
    ;

// fragment #8.1
constBindableProperty :
    DESCENDING | ConstBindableIdent
    ;

// production #9
simpleConstReq :
    Ident ( '=' constExpression | ':' predefOrRefTypeIdent )
    ;

// alias #9.1
constExpression : expression ; /* but no type identifiers */

// alias #9.2
predefOrRefTypeIdent : Ident ;

// production #10
procedureRequirement :
    PROCEDURE (
    ( restrictedExport? procedureSignature ) |
    ( '[' procBindableEntity ']' ( procedureSignature | ':' NIL ) ) )
    ;

// alias #10.1
restrictedExport : '*' ;

// fragment #10.2
procBindableEntity :
    '+' | '-' | '*' | '/' | '=' | '<' | '>' | '::' | ':=' | '..' |
    IN | DIV | MOD | FOR | ProcBindableIdent
    {} /* make ANTLRworks display separate branches */
    ;


// *** Import Lists ***

// production #11
importList :
    ( libGenDirective | importDirective ) ';'
    ;

// production #12
libGenDirective :
    GENLIB libIdent FROM template FOR templateParamList END
    ;

// alias #12.1
libIdent : Ident ;

// alias #12.2
template : Ident ;

// production #13
templateParamList :
    templateParam ( ';' templateParam )* ;

// fragment #13.1
templateParam :
    placeholder '=' replacement
    ;

// alias #13.2
placeholder : Ident ;

// alias #13.3
replacement : StringLiteral ;

// production #14
importDirective :
    IMPORT moduleIdent reExport? ( ',' moduleIdent reExport? )* |
    FROM moduleOrEnumIdent IMPORT ( identList | '*' )
    ;

// alias #14.1
reExport : '+' ;

// alias #14.2
moduleOrEnumIdent : Ident ;


*** Blocks, Definitions and Declarations ***

// production #15
block :
    declaration*
    ( BEGIN statementSequence )? END
    ;

// production #16
definition :
    CONST ( constDeclaration ';' )+ |
    TYPE ( Ident '=' typeOrOpaqueType ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';'
    ;

// fragment #16.1
typeOrOpaqueType :
    type | OPAQUE
    ;

// production #17
declaration :
    CONST ( Ident '=' constExpression ';' )+ |
    TYPE ( Ident '=' type ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';' block Ident ';'
    ;

// production #18
constDefinition :
    ( '[' constBindableProperty ']' )? Ident '=' constExpression
    ;

// production #19
variableDeclaration :
    identList ':' ( range OF )? typeIdent
    ;


// *** Types ***

// production #20
type :
    (( ALIAS | SET | range ) OF )? typeIdent |
    enumType | arrayType | recordType | pointerType | procedureType
    ;

// alias 20.1
typeIdent : qualident ;

// production #21
range :
    '[' '>'? constExpression '..' '<'? constExpression ']'
    ;

// production #22
enumType :
    '(' ( '+' enumBaseType ',' )? identList ')'
    ;

// alias 22.1
enumBaseType : enumTypeIdent ;

// alias 22.2
enumTypeIdent : typeIdent ;

// production #23
arrayType :
    ARRAY componentCount ( ',' componentCount )* OF typeIdent
    ;

// alias #23.1
componentCount : constExpression ;

// production #24
recordType :
    RECORD ( fieldList ( ';' fieldList )* indeterminateField? |
    '(' recBaseType ')' fieldList ( ';' fieldList )* ) END
    ;

// alias 24.1
recBaseType : recTypeIdent ;

// alias 24.2
recTypeIdent : typeIdent ;

// production #25
fieldList :
    restrictedExport? variableDeclaration
    ;

// production #26
indeterminateField :
    INDETERMINATE Ident ':' ARRAY discriminantFieldIdent OF typeIdent
    ;

// alias #26.1
discriminantFieldIdent : Ident ;

// production #27
pointerType :
    POINTER TO CONST? typeIdent
    ;

// production #28
procedureType :
    PROCEDURE
    ( '(' formalTypeList ')' )?
    ( ':' returnedType )?
    ;

// alias #28.1
returnedType : typeIdent ;

// production #29
formalTypeList :
    formalType ( ',' formalType )*
    ;

// fragment #29.1
formalType :
    attributedFormalType | variadicFormalType
    ;

// production #30
attributedFormalType :
    ( CONST | VAR {})? simpleFormalType
    ;

// production #31
simpleFormalType :
    CAST? ( ARRAY OF )? typeIdent
    ;

// production #32
variadicFormalType :
    ARGLIST numberOfArgumentsToPass? OF
    ( attributedFormalType |
      '{' attributedFormalType ( ',' attributedFormalType )* '}' )
    ( '|' variadicTerminator )?
    ;


// *** Procedures ***

// production #33
procedureHeader :
    PROCEDURE ( '[' procBindableEntity ']' | restrictedExport )?
    procedureSignature
    ;

// production #34
procedureSignature :
    Ident ( '(' formalParamList ')' )? returnedType?
    ;

// *** Formal Parameters ***

// production #35
formalParamList :
    formalParams ( ';' formalParams )*
    ;

// fragment #35.1
formalParams :
    simpleFormalParams | variadicFormalParams
    ;

// production #36
simpleFormalParams :
    ( CONST | VAR {})? identList ':' simpleFormalType
    ;

// production #37
variadicFormalParams :
    ARGLIST numberOfArgumentsToPass? OF
    ( simpleFormalType |
      '{' simpleFormalParams ( ';' simpleFormalParams )* '}' )
    ( '|' variadicTerminator )? ;    ;

// alias #37.1
numberOfArgumentsToPass : constExpression ;

// alias #37.2
variadicTerminator : constExpression ;


// *** Statements ***

// production #38
statement :
    ( assignmentOrProcedureCall | ifStatement | caseStatement |
      whileStatement | repeatStatement | loopStatement |
      forStatement | RETURN expression? | EXIT )?
    ;

// production #39
statementSequence :
    statement ( ';' statement )*
    ;

// production #40
assignmentOrProcedureCall :
    designator ( ':=' expression | incOrDevSuffix | actualParameters )?
    ;

// fragment #40.1
incOrDevSuffix :
    '++' | '--'
    {} /* make ANTLRworks display separate branches */
    ;

// production #41
ifStatement :
    IF expression THEN statementSequence
    ( ELSIF expression THEN statementSequence )*
    ( ELSE statementSequence )?
    END
    ;

// production #42
caseStatement :
    CASE expression OF ( '|' case777 )+ ( ELSE statementSequence )? END
    ;

// production #43
case777 :
/* ANTLR has a design flaw in that it cannot handle any rule identifiers that
   coincide with reserved words of the language it uses to generate the parser */
    caseLabels ( ',' caseLabels )* ':' statementSequence
    ;

// production #44
caseLabels :
    constExpression ( '..' constExpression )?
    ;

// production #45
whileStatement :
    WHILE expression DO statementSequence END
    ;

// production #46
repeatStatement :
    REPEAT statementSequence UNTIL expression
    ;

// production #47
loopStatement :
    LOOP statementSequence END
    ;

// production #48
forStatement :
    FOR DESCENDING? controlVariable
    IN ( designator | range OF typeIdent )
    DO statementSequence END
    ;

// alias #48.1
controlVariable : Ident ;

// production #49
designator :
    qualident designatorTail?
    ;

// production #50
designatorTail :
    ( ( '[' exprListOrSlice ']' | '^' ) ( '.' Ident )* )+
    ;

// *** Expressions ***

// production #51
exprListOrSlice :
    expression ( ( ',' expression )+ | '..' expression )?
    ;

// production #52
expression :
/* represents operator precedence level 1 */
    simpleExpression ( relOp simpleExpression )?
    ;

// fragment #52.1
relOp :
    '=' | '#' | '<' | '<=' | '>' | '>=' | '==' | IN
    {} /* make ANTLRworks display separate branches */
    ;

// production #53
simpleExpression :
/* represents operator precedence level 2 */
    ( '+' | '-' {} /* make ANTLRworks display separate branches */ )?
    term ( addOp term )*
    ;

// fragment #53.1
addOp :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
	;

// production #54
term :
/* represents operator precedence level 3 */
    factorOrNegation ( mulOp factorOrNegation )*
    ;

// fragment #54.1
mulOp :
    '*' | '/' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #55
factorOrNegation :
/* represents operator precedence level 4 */
    NOT? factor
    ;

// production #56
factor :
/* represents operator precedence level 5 */
    simpleFactor ( '::' typeIdent )?
    ;

// production #57
simpleFactor :
    NumericLiteral | StringLiteral | structuredValue |
    designatorOrFunctionCall | '(' expression ')'
    ;

// production #58
designatorOrFunctionCall :
    designator actualParameters?
    ;

// production #59
actualParameters :
    '(' expressionList? ')'
    ;

// production #60
expressionList :
    expression ( ',' expression )*
    ;

// *** Structured Values ***

// production #61
structuredValue :
    '{' ( valueComponent ( ',' valueComponent )* )? '}'	
    ;

// production #62
valueComponent :
    expression ( ( BY | '..' {}) constExpression )?
    ;

// *** Identifiers ***

// production #63
qualident :
    Ident ( '.' Ident )*
    ;

// production #64
identList :
    Ident ( ',' Ident )*
    ;


// ---------------------------------------------------------------------------
// O P T I O N A L   L A N G U A G E   F A C I L I T I E S
// ---------------------------------------------------------------------------
/*
// *** Architecture Specific Implementation Module Selection ***

// replacement for production #2
langExtn_programModule :
    MODULE moduleIdent ( '(' archSelector ')' )? ';'
    importList* block moduleIdent '.'
    ;

// alias: Architecture Selector
langExtn_archSelector : Ident ;

// *** Register Mapping ***

// replacement for production #29
langExtn_simpleFormalType :
    CAST? ( ARRAY OF )? typeIdent regAttribute?
    ;

// Register Mapping Attribute
regAttribute :
    IN REG ( registerNumber | registerMnemonic )
    ;

// alias: Register Number
registerNumber : constExpression ;

// alias: Register Mnemonic
registerMnemonic : qualident ;

// *** Symbolic Assembly Inlining ***

// replacement for production #36
langExtn_statement :
    ( assignmentOrProcedureCall | ifStatement | caseStatement |
      whileStatement | repeatStatement | loopStatement |
      forStatement | assemblyBlock | RETURN expression? | EXIT )?
    ;

// Assembly Block
assemblyBlock :
    ASM assemblySourceCode END
    ;

// Assembly Source Code
assemblySourceCode :
    <implementation defined syntax>
    ;
*/

// ---------------------------------------------------------------------------
// P R A G M A   G R A M M A R
// ---------------------------------------------------------------------------
// 23 productions

// *** Pragmas ***

// production #1
pragma :
    '<*' pragmaBody '*>'
    ;

// fragment #1.1
pragmaBody :
	pragmaMSG | pragmaIF | procAttrPragma | pragmaPTW | pragmaFORWARD |
    pragmaENCODING | pragmaALIGN | pragmaPADBITS | pragmaPURITY |
    variableAttrPragma | pragmaDEPRECATED | pragmaGENERATED |
    pragmaADDR | pragmaFFI | pragmaFFIDENT | implDefinedPragma
    ;

// production #2
pragmaMSG :
    MSG '=' messageMode ':'
    compileTimeMsgComponent ( ',' compileTimeMsgComponent )*
    ;

// fragment #2.1
messageMode :
    ( INFO | WARN | ERROR | FATAL {})
    ;

// production #3
compileTimeMsgComponent :
    StringLiteral | constQualident |
    '@' ( ALIGN | ENCODING | implDefinedPragmaSymbol )
    ;

// alias #3.1
constQualident : qualident ; /* no type and no variable identifiers */

// alias #3.2
implDefinedPragmaSymbol : Ident ; /* lowercase or mixed case only */

// production #4
pragmaIF :
    ( IF | ELSIF {}) inPragmaExpression | ELSE | ENDIF
    ;

// production #5
procAttrPragma :
    INLINE | NOINLINE | NORETURN
    {} /* make ANTLRworks display separate branches */
    ;

// production #6
pragmaPTW :
    PTW
    ;

// production #7
pragmaFORWARD :
    FORWARD ( TYPE identList | procedureHeader )
    ;

/* multi-pass compilers ignore and skip any token after FORWARD */

// production #8
pragmaENCODING : 
    ENCODING '=' StringLiteral /* "ASCII" or "UTF8" */
    ( ':' codePointSample ( ',' codePointSample )* )?
    ;

// production #9
codePointSample :
    quotedCharacterLiteral '=' characterCodeLiteral
    ;

// alias #9.1
quotedCharacterLiteral : StringLiteral ; /* single character only */

// alias #9.2
characterCodeLiteral : NumericLiteral ; /* unicode code points only */

// production #10
pragmaALIGN :
    ALIGN '=' inPragmaExpression
    ;

// production #11
pragmaPADBITS :
    PADBITS '=' inPragmaExpression
    ;

// production #12
pragmaPURITY :
    PURITY '=' inPragmaExpression /* values 0 .. 3 */
    ;

// production #13
variableAttrPragma :
    SINGLEASSIGN | LOWLATENCY | VOLATILE
    {} /* make ANTLRworks display separate branches */
    ;

// production #14
pragmaDEPRECATED :
    DEPRECATED
    ;

// production #15
pragmaGENERATED :
    GENERATED template ',' datestamp ',' timestamp
    ;

// fragment #15.1
datestamp :
    year '-' month '-' day
    ;

// fragment #15.2
timestamp :
    hours ':' minutes ':' seconds '+' timezone
    ;

// alias #15.3a
year : wholeNumber ;

// alias #15.3b
month : wholeNumber ;

// alias #15.3c
day : wholeNumber ;

// alias #15.3d
hours : wholeNumber ;

// alias #15.3e
minutes : wholeNumber ;

// alias #15.3f
seconds : wholeNumber ;

// alias #15.4g
timezone : wholeNumber ;

// production #16
pragmaADDR :
    ADDR '=' inPragmaExpression
    ;

// production #17
pragmaFFI :
    FFI '=' StringLiteral /* "C", "Fortran", "CLR" or "JVM" */
    ;

// production #18
pragmaFFIDENT :
    FFIDENT '=' StringLiteral /* foreign library identifier */
    ;

// production #19
implDefinedPragma :
    implDefinedPragmaSymbol ( '=' inPragmaExpression )? '|' messageMode
    ;

// production #20
inPragmaExpression :
/* represents operator precedence level 1 */
    inPragmaSimpleExpression ( inPragmaRelOp inPragmaSimpleExpression )?
    ;

// fragment #20.1
inPragmaRelOp :
    '=' | '#' | '<' | '<=' | '>' | '>='
    {} /* make ANTLRworks display separate branches */
    ;

// production #21
inPragmaSimpleExpression :
/* represents operator precedence level 2 */
    ( '+' | '-' {})? inPragmaTerm ( addOp inPragmaTerm )*
    ;

// production #22
inPragmaTerm :
/* represents operator precedence level 3 */
    inPragmaFactor ( inPragmaMulOp inPragmaFactor )*
    ;

// fragment #22.1
inPragmaMulOp :
    '*' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #23
inPragmaFactor :
/* represents operator precedence level 4 */
    NOT? inPragmaSimpleFactor
    ;

// production #24
inPragmaSimpleFactor :
    wholeNumber |
    /* constQualident is covered by inPragmaCompileTimeFunctionCall */
    '(' inPragmaExpression ')' | inPragmaCompileTimeFunctionCall
    ;

// alias #24.1
wholeNumber : NumericLiteral ;

// production #25
inPragmaCompileTimeFunctionCall :
    qualident ( '(' inPragmaExpression ( ',' inPragmaExpression )* ')' )?
    ;
    

// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 4 productions, 16 fragments, 1 alias

// production #1
ReservedWord :
    ALIAS | AND | ARGLIST | ARRAY | BEGIN | BLUEPRINT | BY | CASE | CONST |
    DEFINITION | DESCENDING | DIV | DO | ELSE | ELSIF | END | EXIT | FOR |
    FROM | GENLIB | IF | IMPLEMENTATION | IMPORT | IN | INDETERMINATE | LOOP |
    MOD | MODULE | NOT | OF | OPAQUE | OR | POINTER | PROCEDURE | RECORD |
    REFERENTIAL | REPEAT | RETURN | SET | THEN | TO | TYPE | UNTIL | VAR |
    WHILE
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
IdentTail :
    ( IdentLeadChar | Digit )+
    ;

// fragment #2.3
// both an identifier and a reserved word
// resolve using Schroedinger's Token
ConstBindableIdent :  /* Ident */
    NIL | TLIMIT | TSIGNED | TBASE | TPRECISION | TMINEXP | TMAXEXP
    {} /* make ANTLRworks display separate branches */
	;

// fragment #2.4
// both an identifier and a reserved word
// resolve using Schroedinger's Token
ProcBindableIdent : /* Ident */
    ABS | NEG | DUP | COPY | COUNT | LENGTH | NEW | RETAIN | RELEASE |
    CONCAT | STORE | RETRIEVE | INSERT | REMOVE | SUBSET |
    READ | WRITE | WRITEF | TMIN | TMAX | SXF | VAL
    {} /* make ANTLRworks display separate branches */
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
Digit :
    Base2Digit | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #3.7 */
Base16Digit :
    Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #3.8 */
Base2Digit :
    '0' | '1'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #3.9 */
DigitSep : SINGLE_QUOTE {} /* make ANTLRworks display name, not literal */ ;

// production #4
StringLiteral :
    SingleQuotedString | DoubleQuotedString
    ;

fragment /* #4.1 */
SingleQuotedString :
    SINGLE_QUOTE ( QuotableCharacter | DOUBLE_QUOTE )* SINGLE_QUOTE
    ;

fragment /* #4.2 */
DoubleQuotedString :
    DOUBLE_QUOTE ( QuotableCharacter | SINGLE_QUOTE )* DOUBLE_QUOTE
    ;

fragment /* #4.3 */
QuotableCharacter :
    Digit | Letter | Space | NonAlphaNumQuotable | EscapedCharacter
    ;

fragment /* #4.4 */
Letter :
    'A' .. 'Z' | 'a' .. 'z'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.5 */
Space : ' ' ;

fragment /* #4.6 */
NonAlphaNumQuotable :
    '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' |
    '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
    '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #4.7 */
EscapedCharacter :
    BACKSLASH ( 'n' | 't' | BACKSLASH {})
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

// pseudo-procudion to make #2 and #3 hidden
Comment :
    BlockComment | LineComment
    { $channel = HIDDEN; } /* ignore */
    ;

// production #2
fragment
BlockComment :
    '(*'
    ( options { greedy=false; }: . )* /* anything other than '(*' or '*)' */
    BlockComment*
    '*)'
    ;	

// production #3
fragment
LineComment :
    '//'
    ( options { greedy=false; }: . )* /* anything other than EOL */
    EndOfLine
    ;

// production #4
fragment
EndOfLine :
    ASCII_LF | ASCII_CR (ASCII_LF{})?
    ;

// END OF FILE