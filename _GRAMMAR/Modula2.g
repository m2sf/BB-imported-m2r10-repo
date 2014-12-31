/* (C) 2009-2014 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Dec 31, 2014 */


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
// 50 reserved words, 26 dual-use identifiers, 23 pragma symbols

tokens {
	
// *** Reserved Words, 50 tokens ***

    ALIAS          = 'ALIAS';
    AND            = 'AND';            /* also a RW within pragmas */
    ARGLIST        = 'ARGLIST';
    ARRAY          = 'ARRAY';
    BEGIN          = 'BEGIN';
    BLUEPRINT      = 'BLUEPRINT';
    BY             = 'BY';
    CASE           = 'CASE';
    CONST          = 'CONST';
    COPY           = 'COPY';
    DEFINITION     = 'DEFINITION';
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
    LITERAL        = 'LITERAL';
    LOOP           = 'LOOP';
    MOD            = 'MOD';            /* also a RW within pragmas */
    MODULE         = 'MODULE';
    NEW            = 'NEW';
    NONE           = 'NONE';
    NOT            = 'NOT';            /* also a RW within pragmas */
    OF             = 'OF';
    OPAQUE         = 'OPAQUE';
    OR             = 'OR';             /* also a RW within pragmas */
    POINTER        = 'POINTER';
    PROCEDURE      = 'PROCEDURE';
    RECORD         = 'RECORD';
    REFERENTIAL    = 'REFERENTIAL';
    RELEASE        = 'RELEASE';
    REPEAT         = 'REPEAT';
    RETAIN         = 'RETAIN';
    RETURN         = 'RETURN';
    SET            = 'SET';
    THEN           = 'THEN';
    TO             = 'TO';
    TYPE           = 'TYPE';
    UNTIL          = 'UNTIL';
    VAR            = 'VAR';
    WHILE          = 'WHILE';


// *** Dual-Use Identifiers, 32 tokens ***

//  Dual-Use identifiers may be used as RWs depending on context. The
//  resulting ambiguity is resolvable using the Schroedinger's Token
//  technique described in an article published in SP&E:
//
//    Schroedinger’s token
//    John Aycock and Nigel R. Horspool
//    Copyright (c) 2001, Wiley & Sons, Ltd.
//    
//  DOI: Softw. Pract. Exper. 2001; 31:803–814 (DOI: 10.1002/spe.390)
//  URL: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.13.3178

/* Identifiers that are used like RWs within formal parameter lists */

    CAST           = 'CAST';           /* -> production #32 */
    ADDRESS        = 'ADDRESS';        /* -> production #32 */
    NIL            = 'NIL';            /* -> productions #33 and #38 */

/* Identifiers that are used like RWs within literal specification */

    CHAR           = 'CHAR';           /* -> production #5 */
    UNICHAR        = 'UNICHAR';        /* -> production #5 */
    INTEGER        = 'INTEGER';        /* -> production #5 */
    REAL           = 'REAL';           /* -> production #5 */


/* Identifiers that are used like RWs within bound constant declarations */

    TNIL           = 'TNIL';           /* -> production #9 */
    TBIDI          = 'TBIDI';          /* -> production #9 */
    TLIMIT         = 'TLIMIT';         /* -> production #9 */
    TSIGNED        = 'TSIGNED';        /* -> production #9 */
    TBASE          = 'TBASE';          /* -> production #9 */
    TPRECISION     = 'TPRECISION';     /* -> production #9 */
    TMINEXP        = 'TMINEXP';        /* -> production #9 */
    TMAXEXP        = 'TMAXEXP';        /* -> production #9 */

/* Identifiers that are used like RWs within bound procedure headers */

    ABS            = 'ABS';            /* -> production #11 */
    NEG            = 'NEG';            /* -> production #11 */
    COUNT          = 'COUNT';          /* -> production #11 */
    LENGTH         = 'LENGTH';         /* -> production #11 */
    STORE          = 'STORE';          /* -> production #11 */
    RETRIEVE       = 'RETRIEVE';       /* -> production #11 */
    INSERT         = 'INSERT';         /* -> production #11 */
    REMOVE         = 'REMOVE';         /* -> production #11 */
    SUBSET         = 'SUBSET';         /* -> production #11 */
    READ           = 'READ';           /* -> production #11 */
    READNEW        = 'READNEW';        /* -> production #11 */
    WRITE          = 'WRITE';          /* -> production #11 */
    WRITEF         = 'WRITEF';         /* -> production #11 */
    TMAX           = 'TMAX';           /* -> production #11 */
    TMIN           = 'TMIN';           /* -> production #11 */
    SXF            = 'SXF';            /* -> production #11 */
    VAL            = 'VAL';            /* -> production #11 */


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
    MODULE TYPE '=' ( OPAQUE | RECORD | NONE )
      ( ';' LITERAL '=' protoLiteral ( '|' protoLiteral )* )?
    ;

// fragment #5.1
protoLiteral :
    simpleProtoLiteral | structuredProtoLiteral
    ;

// fragment #5.2
simpleProtoLiteral : /* Ident */
    CHAR | UNICHAR | INTEGER | REAL
    {} /* make ANTLRworks display separate branches */
    ;

// production #6
structuredProtoLiteral :
    '{' ( simpleProtoLiteralList |
        ARGLIST reqValueCount? OF
          ( '{' simpleProtoLiteralList '}' | simpleProtoLiteral ) | '*' ) '}'
    ;

// alias #6.1
simpleProtoLiteralList : identList;

// production #7
reqValueCount :
    greaterThan? ( constIdent | wholeNumber )
    ;

// alias #7.1
greaterThan : '>' ;

// alias #7.2
constIdent : Ident ;

// alias #7.3
wholeNumber : NumberLiteral ;

// production #8
constOrTypeOrProcRequirement :
    ( NOT? boolConstIdent '->' )?
    ( constRequirement | procedureRequirement | TYPE Ident '=' procedureType )
    ;

// alias #8.1
boolConstIdent : Ident ;

// production #9
constRequirement :
    CONST (
    ( '[' constBindableIdent ']' ( simpleConstRequirement | '=' NONE )? |
      restrictedExport? simpleConstRequirement )
    ;

// fragment #9.1
constBindableIdent : /* Ident */
    TNIL | TBIDI | TLIMIT | TSIGNED | TBASE | TPRECISION | TMINEXP | TMAXEXP
    ;

// production #10
simpleConstRequirement :
    Ident ( '=' constExpression | ':' predefOrRefTypeIdent )
    ;

// alias #10.1
constExpression : expression ; /* no type identifiers */

// alias #10.2
predefOrRefTypeIdent : Ident ;

// production #11
procedureRequirement :
    PROCEDURE (
    ( restrictedExport? procedureSignature ) |
    ( '[' procBindableEntity ']' ( procedureSignature | '=' NONE )? ) )
    ;

// alias #11.1
restrictedExport : '*' ;

// fragment #11.2
procBindableEntity :
    procBindableOperator | procBindableRW | procBindableIdent
    ;

// fragment #11.3
procBindableOperator :
    '+' | '-' | '*' | '/' | BACKSLASH |
    '*.' | '=' | '<' | '>' | '::' | DIV | MOD |
    {} /* make ANTLRworks display separate branches */
    '..' bindingSelector?
    ;

// fragment #11.4
procBindableRW :
    ARRAY | NEW | RETAIN | RELEASE | IN | FOR |
    COPY bindingSelector?
    ;

// fragment #11.5
procBindableIdent : /* Ident bindingSelector? */
    ABS | NEG | COUNT | LENGTH | STORE | RETRIEVE | SUBSET |
    READ | READNEW | WRITE | WRITEF | TMIN | TMAX | SXF | VAL |
    {} /* make ANTLRworks display separate branches */
    INSERT bindingSelector? | REMOVE bindingSelector?
    ;

// fragment #11.6
bindingSelector :
    '*' | '+' | '++'
    ;


// *** Import Lists ***

// production #12
importList :
    ( libGenDirective | importDirective ) ';'
    ;

// production #13
libGenDirective :
    GENLIB libIdent FROM template FOR templateParamList END
    ;

// alias #13.1
libIdent : Ident ;

// alias #13.2
template : Ident ;

// production #14
templateParamList :
    templateParam ( ';' templateParam )* ;

// fragment #14.1
templateParam :
    placeholder '=' replacement
    ;

// alias #14.2
placeholder : Ident ;

// alias #14.3
replacement : StringLiteral ;

// production #15
importDirective :
    IMPORT moduleIdent reExport? ( ',' moduleIdent reExport? )* |
    FROM moduleOrEnumIdent IMPORT ( identList | '*' )
    ;

// alias #15.1
reExport : '+' ;

// alias #15.2
moduleOrEnumIdent : Ident ;


*** Blocks, Definitions and Declarations ***

// production #16
block :
    declaration*
    ( BEGIN statementSequence )? END
    ;

// production #17
definition :
    CONST ( constDeclaration ';' )+ |
    TYPE ( Ident '=' typeOrOpaqueType ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';'
    ;

// fragment #17.1
typeOrOpaqueType :
    type | OPAQUE
    ;

// production #18
declaration :
    CONST ( Ident '=' constExpression ';' )+ |
    TYPE ( Ident '=' type ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';' block Ident ';'
    ;

// production #19
constDefinition :
    ( '[' constBindableProperty ']' )? Ident '=' constExpression
    ;

// production #20
variableDeclaration :
    identList ':' ( range OF )? typeIdent
    ;


// *** Types ***

// production #21
type :
    (( ALIAS | SET | range ) OF )? typeIdent |
    CONST opaquePointerTypeIdent |
    enumType | arrayType | recordType | pointerType | procedureType
    ;

// alias 21.1
typeIdent : qualident ;

// alias 21.2
opaquePointerTypeIdent : typeIdent ;

// production #22
range :
    '[' '>'? constExpression '..' '<'? constExpression ']'
    ;

// production #23
enumType :
    '(' ( '+' enumBaseType ',' )? identList ')'
    ;

// alias 23.1
enumBaseType : enumTypeIdent ;

// alias 23.2
enumTypeIdent : typeIdent ;

// production #24
arrayType :
    ARRAY componentCount ( ',' componentCount )* OF typeIdent
    ;

// alias #24.1
componentCount : constExpression ;

// production #25
recordType :
    RECORD ( fieldList ( ';' fieldList )* indeterminateField? |
    '(' recBaseType ')' fieldList ( ';' fieldList )* ) END
    ;

// alias 25.1
recBaseType : recTypeIdent ;

// alias 25.2
recTypeIdent : typeIdent ;

// production #26
fieldList :
    restrictedExport? variableDeclaration
    ;

// production #27
indeterminateField :
    INDETERMINATE Ident ':' ARRAY discriminantFieldIdent OF typeIdent
    ;

// alias #27.1
discriminantFieldIdent : Ident ;

// production #28
pointerType :
    POINTER TO CONST? typeIdent
    ;

// production #29
procedureType :
    PROCEDURE
    ( '(' formalTypeList ')' )?
    ( ':' returnedType )?
    ;

// alias #29.1
returnedType : typeIdent ;

// production #30
formalTypeList :
    formalType ( ',' formalType )*
    ;

// fragment #30.1
formalType :
    attributedFormalType | variadicFormalType
    ;

// production #31
attributedFormalType :
    ( CONST | VAR | NEW {})? simpleFormalType
    ;

// production #32
simpleFormalType :
    ( ARRAY OF )? typeIdent | castingFormalType
    ;

// fragment #32.1
// only available after import of CAST
castingFormalType :
    CAST ( ADDRESS | ARRAY OF typeIdent )
    :

// production #33
variadicFormalType :
    ARGLIST numberOfArgumentsToPass? OF
    ( attributedFormalType |
      '{' attributedFormalType ( ',' attributedFormalType )* '}' )
    ( '|' variadicTerminator )?
    ;

// alias #33.1
numberOfArgumentsToPass : reqValueCount ;

// fragment #33.2
variadicTerminator :
    constIdent | wholeNumber | '-1' | NIL
    ;


// *** Procedures ***

// production #34
procedureHeader :
    PROCEDURE ( '[' procBindableEntity ']' | restrictedExport )?
    procedureSignature
    ;

// production #35
procedureSignature :
    Ident ( '(' formalParamList ')' )? returnedType?
    ;


// *** Formal Parameters ***

// production #36
formalParamList :
    formalParams ( ';' formalParams )*
    ;

// fragment #36.1
formalParams :
    simpleFormalParams | variadicFormalParams
    ;

// production #37
simpleFormalParams :
    ( CONST | VAR | NEW {})? identList ':' simpleFormalType
    ;

// production #38
variadicFormalParams :
    ARGLIST numberOfArgumentsToPass? OF
    ( simpleFormalType |
      '{' simpleFormalParams ( ';' simpleFormalParams )* '}' )
    ( '|' variadicTerminator )?
    ;


// *** Statements ***

// production #39
statement :
    assignmentOrProcedureCall | ifStatement | caseStatement |
    whileStatement | repeatStatement | loopStatement |
    forStatement | ( RETAIN | RELEASE ) expression |
    RETURN expression? | EXIT
    ;

// production #40
statementSequence :
    statement ( ';' statement )*
    ;

// production #41
assignmentOrProcedureCall :
    COPY designator ':=' expression |
    NEW designator ( ( ':=' | OF ) expression )? |
    designator ( ':=' expression | incOrDecSuffix | actualParameters )?
    ;

// fragment #41.1
incOrDecSuffix :
    '++' | '--'
    {} /* make ANTLRworks display separate branches */
    ;

// production #42
ifStatement :
    IF expression THEN statementSequence
    ( ELSIF expression THEN statementSequence )*
    ( ELSE statementSequence )?
    END
    ;

// production #43
caseStatement :
    CASE expression OF ( '|' case777 )+ ( ELSE statementSequence )? END
    ;

// production #44
case777 :
/* ANTLR has a design flaw in that it cannot handle any rule identifiers that
   coincide with reserved words of the language it uses to generate the parser */
    caseLabels ( ',' caseLabels )* ':' statementSequence
    ;

// production #45
caseLabels :
    constExpression ( '..' constExpression )?
    ;

// production #46
whileStatement :
    WHILE expression DO statementSequence END
    ;

// production #47
repeatStatement :
    REPEAT statementSequence UNTIL expression
    ;

// production #48
loopStatement :
    LOOP statementSequence END
    ;

// production #49
forStatement :
    FOR controlVariable incOrDecSuffix?
    IN ( designator | range OF typeIdent )
    DO statementSequence END
    ;

// alias #49.1
controlVariable : Ident ;

// production #50
designator :
    qualident designatorTail?
    ;

// production #51
designatorTail :
    ( ( '[' exprListOrSlice ']' | '^' ) ( '.' Ident )* )+
    ;


// *** Expressions ***

// production #52
exprListOrSlice :
    expression ( ( ',' expression )+ | '..' expression? )?
    ;

/* Operator Precedence Level 1 */

// production #53
expression :
    simpleExpression ( op1 simpleExpression )?
    ;

// fragment #53.1
op1 :
    '=' | '#' | '<' | '<=' | '>' | '>=' | '==' | '+>' | '+/' | IN
    {} /* make ANTLRworks display separate branches */
    ;

/* Operator Precedence Level 2 */

// production #54
simpleExpression :
    ( '+' | '-' {})? term ( op2 term )*
    ;

// fragment #54.1
op2 :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
	;

/* Operator Precedence Level 3 */

// production #55
term :
    factorOrNegation ( op3 factorOrNegation )*
    ;

// fragment #55.1
op3 :
    '*' | '/' | '*.' | BACKSLASH | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

/* Operator Precedence Level 4 */

// production #56
factorOrNegation :
    NOT? factor
    ;

/* Operator Precedence Level 5 */

// production #57
factor :
    simpleFactor ( '::' typeIdent )?
    ;

// production #58
simpleFactor :
    NumberLiteral | StringLiteral | structuredValue |
    '(' expression ')' | designator actualParameters?
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
characterCodeLiteral : NumberLiteral ; /* unicode code points only */

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

// fragment #21.1
addOp :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
    ;

// production #22
inPragmaTerm :
/* represents operator precedence level 3 */
    inPragmaFactor ( mulOp inPragmaFactor )*
    ;

// fragment #22.1
mulOp :
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
    WholeNumber |
    /* constQualident is covered by inPragmaCompileTimeFunctionCall */
    '(' inPragmaExpression ')' | inPragmaCompileTimeFunctionCall
    ;

// production #25
inPragmaCompileTimeFunctionCall :
    qualident ( '(' inPragmaExpression ( ',' inPragmaExpression )* ')' )?
    ;
    

// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 4 productions

// production #1
ReservedWord :
    ALIAS | AND | ARGLIST | ARRAY | BEGIN | BLUEPRINT | BY | CASE | CONST |
    COPY | DEFINITION | DIV | DO | ELSE | ELSIF | END | EXIT | FOR | FROM |
    GENLIB | IF | IMPLEMENTATION | IMPORT | IN | INDETERMINATE | LITERAL |
    LOOP | MOD | MODULE | NEW | NONE | NOT | OF | OPAQUE | OR | POINTER |
    PROCEDURE | RECORD | REFERENTIAL | RELEASE | REPEAT | RETAIN | RETURN |
    SET | THEN | TO | TYPE | UNTIL | VAR | WHILE
    ;

// production #1.1
// dual-use identifiers
ConstBindableIdent :  /* Ident */
    TNIL | TBIDI | TLIMIT | TSIGNED | TBASE | TPRECISION | TMINEXP | TMAXEXP
	;

// production #1.2
// dual-use identifiers
ProcBindableIdent : /* Ident */
    ABS | NEG | COUNT | LENGTH | STORE | RETRIEVE | INSERT | REMOVE |
    SUBSET | READ | READNEW | WRITE | WRITEF | TMIN | TMAX | SXF | VAL
    ;

// production #2
Ident :
    IdentLeadChar IdentTail?
    ;

fragment /* #2.1 */
IdentLeadChar :
    Letter | '_' | '$'
    ;

/* An identifier must not be composed solely of special characters!
   A lexer error shall be emitted for any violation of this rule. */

fragment /* #2.2 */
IdentTail :
    ( IdentLeadChar | Digit )+
    ;

// production #3
NumberLiteral :
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