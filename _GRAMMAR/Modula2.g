/* (C) 2009-2014 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Dec 25, 2014 */


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

// *** Dual-Use Identifiers, 26 tokens ***

//  The following identifiers may be used as RWs depending on context.
//  The ambiguity is resolvable using the Schroedinger's Token technique.

/* Identifier CAST is used like an RW within a formal parameter list */

    CAST           = 'CAST';

/* Identifiers that are used like RWs within bound constant declarations */

    TNIL           = 'TNIL';
    TBIDI          = 'TBIDI';
    TLIMIT         = 'TLIMIT';
    TSIGNED        = 'TSIGNED';
    TBASE          = 'TBASE';
    TPRECISION     = 'TPRECISION';
    TMINEXP        = 'TMINEXP';
    TMAXEXP        = 'TMAXEXP';

/* Identifiers that are used like RWs within bound procedure headers */

    ABS            = 'ABS';
    NEG            = 'NEG';
    COUNT          = 'COUNT';
    LENGTH         = 'LENGTH';
    STORE          = 'STORE';
    RETRIEVE       = 'RETRIEVE';
    INSERT         = 'INSERT';
    REMOVE         = 'REMOVE';
    SUBSET         = 'SUBSET';
    READ           = 'READ';
    READNEW        = 'READNEW';
    WRITE          = 'WRITE';
    WRITEF         = 'WRITEF';
    TMAX           = 'TMAX';
    TMIN           = 'TMIN';
    SXF            = 'SXF';
    VAL            = 'VAL';

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

// alias #5.2
simpleProtoLiteral : Ident;

// production #6
structuredProtoLiteral :
    '{' ( protoLiteralList |
        ARGLIST ( '>'? itemCount )? OF
          ( '{' protoLiteralList '}' | simpleProtoLiteral ) | '*' ) '}'
    ;

// alias #6.1
protoLiteralList : identList;

// alias #6.2
itemCount : constIdent;

// alias #6.3
constIdent : Ident ; /* only identifiers of constants */

// production #7
constOrTypeOrProcRequirement :
    ( NOT? boolConstIdent '->' )?
    ( constRequirement | procedureRequirement | TYPE Ident '=' procedureType )
    ;

// production #8
constRequirement :
    CONST (
    ( '[' ConstBindableIdent ']' ( simpleConstRequirement | '=' NONE )? |
      restrictedExport? simpleConstRequirement )
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
    ( '[' procBindableEntity ']' ( procedureSignature | '=' NONE )? ) )
    ;

// alias #10.1
restrictedExport : '*' ;

// fragment #10.2
procBindableEntity :
    '+' | '-' | '*' | '/' | '*.' | '=' | '<' | '>' | '::' | ':=' |
    BACKSLASH | ARRAY | NEW | RETAIN | RELEASE | IN | FOR | DIV | MOD |
    procMultiBindableEntity | ProcBindableIdent
    ;

// production #11
procMultiBindableEntity :
    ( '..' | COPY | ProcMultiBindableIdent ) bindingSelector?
    ;

// fragment #11.1
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
    CAST? ( ARRAY OF )? typeIdent
    ;

// production #33
variadicFormalType :
    ARGLIST ( '>'? numberOfArgumentsToPass )? OF
    ( attributedFormalType |
      '{' attributedFormalType ( ',' attributedFormalType )* '}' )
    ( '|' variadicTerminator )?
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
    ARGLIST ( '>'? numberOfArgumentsToPass )? OF
    ( simpleFormalType |
      '{' simpleFormalParams ( ';' simpleFormalParams )* '}' )
    ( '|' variadicTerminator )? ;    ;

// alias #38.1
numberOfArgumentsToPass : constExpression ;

// alias #38.2
variadicTerminator : constExpression ;


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
    NumericLiteral | StringLiteral | structuredValue |
    '(' expression ')' | designator actualParameters?
    ;

// production #59
actualParameters :
    '(' actualParamList? ')'
    ;

// production #60
actualParamList :
    actualParameter ( ',' actualParameter )*
    ;

// fragment #60.1
actualParameter :
    expression | omission
    ;

// alias #60.2
omission : '_' ;


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
    COPY | DEFINITION | DIV | DO | ELSE | ELSIF | END | EXIT | FOR | FROM |
    GENLIB | IF | IMPLEMENTATION | IMPORT | IN | INDETERMINATE | LITERAL |
    LOOP | MOD | MODULE | NEW | NONE | NOT | OF | OPAQUE | OR | POINTER |
    PROCEDURE | RECORD | REFERENTIAL | RELEASE | REPEAT | RETAIN | RETURN |
    SET | THEN | TO | TYPE | UNTIL | VAR | WHILE
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
    TNIL | TBIDI | TLIMIT | TSIGNED | TBASE | TPRECISION | TMINEXP | TMAXEXP
    {} /* make ANTLRworks display separate branches */
	;

// fragment #2.4
// both an identifier and a reserved word
// resolve using Schroedinger's Token
ProcBindableIdent : /* Ident */
    ABS | NEG | COUNT | LENGTH | STORE | RETRIEVE | REMOVE | SUBSET |
    READ | READNEW | WRITE | WRITEF | TMIN | TMAX | SXF | VAL
    {} /* make ANTLRworks display separate branches */
    ;

// fragment #2.5
// both an identifier and a reserved word
// resolve using Schroedinger's Token
ProcMultiBindableIdent : /* Ident */
    INSERT
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