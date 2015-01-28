/* (C) 2009-2014 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Jan 28, 2015 */


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
// 50 reserved words, 31 dual-use identifiers, 23 pragma symbols

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


// *** Dual-Use Identifiers, 31 tokens ***

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
// 66 productions

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
    ( REFERENTIAL identList ';' )? moduleTypeSpec ';'
    ( requirement ';' )*
    END blueprintIdent '.'
    ;

// alias #4.1
blueprintToRefine : blueprintIdent ;

// alias #4.2
blueprintForTypeToExtend : blueprintIdent ;

// production #5
moduleTypeSpec :
    MODULE TYPE '=' 
    ( ( OPAQUE | RECORD ) ( ';' literalSpec )? | NONE )
    ;

// production #6
literalSpec :
    LITERAL '=' protoLiteral ( '|' protoLiteral )*
    ;

// fragment #6.1
protoLiteral :
    simpleProtoLiteral | structuredProtoLiteral
    ;

// fragment #6.2
simpleProtoLiteral : /* Ident */
    CHAR | UNICHAR | INTEGER | REAL | referentialIdent
    {} /* make ANTLRworks display separate branches */
    ;

// production #7
structuredProtoLiteral :
    '{' ARGLIST reqValueCount? OF
          ( '{' simpleProtoLiteralList '}' | simpleProtoLiteral ) | '*' ) |
        simpleProtoLiteralList  '}'
    ;

// fragment #7b
// incorporated into #7 in syntax diagram
simpleProtoLiteralList : /* identList */
    simpleProtoLiteral ( ',' simpleProtoLiteral )*
    ;

// fragment #7.1
reqValueCount :
    greaterThan? wholeNumber
    ;

// alias #7.2
greaterThan : '>' ;

// alias #7.3
wholeNumber : NumberLiteral ;

// production #8
requirement :
    ( NOT? boolConstIdent '->' )?
    ( constRequirement | procedureRequirement | TYPE Ident '=' procedureType )
    ;

// alias #8.1
boolConstIdent : Ident ;

// production #9
constRequirement :
    CONST (
    ( '[' propertyToBindTo ']' ( simpleConstRequirement | '=' NONE )? |
      restrictedExport? simpleConstRequirement )
    ;

// fragment #9.1
propertyToBindTo : /* Ident */
    collectionPropertyIdent | numericPropertyIdent
    ;

// fragment #9.2
collectionPropertyIdent :
    TNIL | TBIDI | TLIMIT
    ;

// fragment #9.3
numericPropertyIdent :    
    TSIGNED | TBASE | TPRECISION | TMINEXP | TMAXEXP
    ;

// alias #9.4
restrictedExport : '*' ;

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
    ( '[' entityToBindTo ']' ( procedureSignature | '=' NONE )? ) |
      restrictedExport? procedureSignature )
    ;

// fragment #11.1
entityToBindTo :
    bindableOperator | bindableResWord | bindableMacro
    ;

// fragment #11.2
bindableOperator :
    '+' | '-' | '*' | '/' | '=' | '<' | '>' | BACKSLASH |
    '*.' | '::' | IN | DIV | MOD | '..' bindingDifferentiator1?
    {} /* make ANTLRworks display separate branches */
    ;

// fragment #11.3
bindableResWord :
    ARRAY | NEW | RETAIN | RELEASE | FOR | COPY bindingDifferentiator1?
    {} /* make ANTLRworks display separate branches */
    ;

// fragment #11.4
bindableMacro :
    ABS | NEG | COUNT | LENGTH | STORE | RETRIEVE | SUBSET |
    READ | READNEW | WRITE | WRITEF | TMIN | TMAX | SXF | VAL |
    INSERT bindingDifferentiator1? | REMOVE bindingDifferentiator2?
    {} /* make ANTLRworks display separate branches */
    ;

// fragment #11.1
bindingDifferentiator1 :
    '*' | '+' | '++'
    ;

// fragment #11.6
bindingDifferentiator2 :
    '*' | '+'
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

// fragment #13b
// incorporated into #13 in syntax diagram
templateParamList :
    placeholder '=' replacement ( ';' placeholder '=' replacement )* ;

// alias #13.1
libIdent : Ident ;

// alias #13.2
template : Ident ;

// alias #13.3
placeholder : Ident ;

// fragment #13.4
replacement : NumberLiteral | StringLiteral | ChevronText ;

// production #14
importDirective :
    IMPORT moduleIdent reExport? ( ',' moduleIdent reExport? )* |
    FROM moduleOrEnumIdent IMPORT ( identList | '*' )
    ;

// alias #14.1
reExport : '+' ;

// alias #14.2
moduleOrEnumIdent : Ident ;

// production #15
identList :
    Ident ( ',' Ident )*
    ;


*** Blocks, Definitions and Declarations ***

// production #16
block :
    declaration*
    ( BEGIN statementSequence )? END
    ;

// production #17
definition :
    CONST ( constDefinition ';' )+ |
    TYPE ( Ident '=' ( OPAQUE | type ) ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';'
    ;

// production #18
constDefinition :
    ( '[' constBindableProperty ']' | restrictedExport )?
    Ident '=' constExpression
    ;

// production #19
variableDeclaration :
    identList ':' ( range OF )? typeIdent
    ;

// production #20
declaration :
    CONST ( Ident '=' constExpression ';' )+ |
    TYPE ( Ident '=' type ';' )+ |
    VAR ( variableDeclaration ';' )+ |
    procedureHeader ';' block Ident ';'
    ;


// *** Types ***

// production #21
type :
    typeIdent | derivedSubType | enumType | setType |
    arrayType | recordType | pointerType | procedureType
    ;

// alias 22.1
typeIdent : qualident ;

// fragment #21.2
derivedSubType :
    ALIAS OF typeIdent | range OF ordinalOrScalarType | CONST dynamicTypeIdent
    ;

// alias 21.3
ordinalOrScalarType : typeIdent ;

// alias 21.4
dynamicTypeIdent : typeIdent ;

// production #22
range :
    '[' greaterThan? constExpression '..' lessThan? constExpression ']'
    ;

// alias #22.1
// greaterThan : '>' ;

// alias #22.2
lessThan : '<' ;

// production #23
enumType :
    '(' ( '+' enumTypeToExtend ',' )? identList ')'
    ;

// alias 23.1
enumTypeToExtend : enumTypeIdent ;

// alias 23.2
enumTypeIdent : typeIdent ;

// production #24
setType :
    SET OF enumTypeIdent
    ;

// production #25
arrayType :
    ARRAY componentCount ( ',' componentCount )* OF typeIdent
    ;

// alias #25.1
componentCount : constExpression ;

// production #26
recordType :
    RECORD ( fieldList ( ';' fieldList )* indeterminateField? |
    '(' recTypeToExtend ')' fieldList ( ';' fieldList )* ) END
    ;

// alias 26.1
recTypeToExtend : typeIdent ;

// alias 26.2
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
    ( '(' formalType ( ',' formalType )* ')' )?
    ( ':' returnedType )?
    ;

// fragment #29.1
formalType :
    simpleFormalType | attributedFormalType | variadicFormalType
    ;

// alias #29.2
returnedType : typeIdent ;

// production #30
simpleFormalType :
    ( ARRAY OF )? typeIdent | castingFormalType
    ;

// fragment #30.1
castingFormalType :
    CAST ( ARRAY OF OCTET | addressTypeIdent )
    ;

// fragment #30.2
addressTypeIdent :
    ADDRESS | UNSAFE.ADDRESS
    ;

// production #31
attributedFormalType :
    ( CONST | VAR | NEW {})? ( simpleFormalType | simpleVariadicFormalType )
    ;

// production #32
simpleVariadicFormalType :
    ARGLIST numOfArgsToPass? OF simpleFormalType ( '|' arglistTerminator )?
    ;

// fragment #32.1
numOfArgsToPass :
    greaterThan? constExpression
    ;

// fragment #32.2
arglistTerminator :
    NIL | minusOne | wholeNumber | constQualident
    ;

// fragment #32.3
minusOne :
    '-' '1'
    ;

// alias #32.4
constQualident : qualident ;

// production #33
variadicFormalType :
    ARGLIST numOfArgsToPass? OF
    ( '{' nonVariadicFormalType ( ',' nonVariadicFormalType )* |
      simpleFormalType )
    ( '|' arglistTerminator )?
    ;

// production #34
nonVariadicFormalType :
    ( CONST | VAR | NEW )? simpleFormalType
    ;


// *** Procedures ***

// production #35
procedureHeader :
    PROCEDURE ( '[' entityToBindTo ']' | restrictedExport )?
    procedureSignature
    ;

// production #36
procedureSignature :
    Ident ( '(' formalParams ( ';' formalParams )* ')' )? returnedType?
    ;


// *** Formal Parameters ***

// production #37
formalParams :
    identList ':' ( simpleFormalType | variadicFormalParams ) |
    attributedFormalParams
    ;

// production #38
attributedFormalParams :
    ( CONST | VAR | NEW ) identList ':'
    ( simpleFormalType | simpleVariadicFormalType )
    ;


// production #39
variadicFormalParams :
    ARGLIST numOfArgsToPass? OF
    ( '{' nonVariadicFormalParams ( ';' nonVariadicFormalParams )* '}' |
      simpleFormalType )
    ( '|' arglistTerminator )?
    ;

// production #40
nonVariadicFormalParams :
    ( CONST | VAR | NEW )? identList ':' simpleFormalType
    ;

// production #41
qualident :
    Ident ( '.' Ident )*
    ;


// *** Statements ***

// production #42
statement :
    memMgtOperation | updateOrProcCall | ifStatement | caseStatement |
    loopStatement | whileStatement | repeatStatement | forStatement | 
    RETURN expression? | EXIT
    ;

// production #43
statementSequence :
    statement ( ';' statement )*
    ;

// production #44
memMgtOperation :
    NEW designator ( OF initSize | ':=' initValue )? |
    RETAIN designator | RELEASE designator
    ;

// alias #44.1
initSize : expression ;

// alias #44.2
initValue : expression ;

// production #45
updateOrProcCall :
    designator ( ':=' expression | incOrDecSuffix | actualParameters )? |
    COPY designator ':=' expression
    ;

// fragment #45.1
incOrDecSuffix :
    '++' | '--'
    {} /* make ANTLRworks display separate branches */
    ;

// production #46
ifStatement :
    IF boolExpression THEN statementSequence
    ( ELSIF expression THEN statementSequence )*
    ( ELSE statementSequence )?
    END
    ;

// alias #46.1
boolExpression : expression ;

/* ANTLR has a design flaw in that it cannot handle any rule identifiers that
   coincide with reserved words of the language it uses to generate the parser,
   by default Java, thus Java reserved words can't be used as rule identifiers.
   
   For this reason we are using "case777" here instead of the proper "case". */

// production #47
caseStatement :
    CASE expression OF ( '|' case777 )+ ( ELSE statementSequence )? END
    ;

// production #48
case777 :
    caseLabels ( ',' caseLabels )* ':' statementSequence
    ;

// production #49
caseLabels :
    constExpression ( '..' constExpression )?
    ;

// production #50
loopStatement :
    LOOP statementSequence END
    ;

// production #51
whileStatement :
    WHILE expression DO statementSequence END
    ;

// production #52
repeatStatement :
    REPEAT statementSequence UNTIL expression
    ;

// production #53
forStatement :
    FOR controlVariable ascOrDescSuffix?
    IN ( designator | range OF ordinalType )
    DO statementSequence END
    ;

// alias #53.1
controlVariable : Ident ;

// alias #53.2
ascOrDescSuffix : incOrDecSuffix ;

// alias #53.3
ordinalType : typeIdent ;

// production #54
designator :
    qualident designatorTail?
    ;

// production #55
designatorTail :
    ( ( '[' exprListOrSlice ']' | '^' ) ( '.' Ident )* )+
    ;


// *** Expressions ***

// production #56
exprListOrSlice :
    expression ( ( ',' expression )+ | '..' expression? )?
    ;

/* Evaluation Level 1 */

// production #57
expression :
    simpleExpression ( operL1 simpleExpression )?
    ;

// fragment #57.1
operL1 :
    '=' | '#' | '<' | '<=' | '>' | '>=' | IN |
    identityOp | arrayConcatOp | dictMergeOp
    {} /* make ANTLRworks display separate branches */
    ;

// alias #57.2
identityOp : '==' ;

// alias #57.3
arrayConcatOp : '+>' ;

// alias #57.4
dictMergeOp : '+\' ;

/* Evaluation Level 2 */

// production #58
simpleExpression :
    ( '+' | '-' {})? term ( operL2 term )*
    ;

// fragment #58.1
operL2 :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
	;

/* Evaluation Level 3 */

// production #59
term :
    factorOrNegation ( operL3 factorOrNegation )*
    ;

// fragment #59.1
operL3 :
    '*' | '/' | DIV | MOD | AND |
    symDiffOp | dotProductOp
    {} /* make ANTLRworks display separate branches */
    ;

// alias #59.1
symDiffOp : BACKSLASH ;

// alias #59.2
dotProductOp : '*.' ;

/* Evaluation Level 4 */

// production #60
factorOrNegation :
    NOT? factorOrTypeConv
    ;

/* Evaluation Level 5 */

// production #61
factorOrTypeConv :
    factor ( '::' typeIdent )?
    ;

/* Evaluation Level 6 */

// production #62
factor :
    NumberLiteral | StringLiteral | structuredValue |
    '(' expression ')' | designator actualParameters?
    ;

// production #63
actualParameters :
    '(' expressionList? ')'
    ;

// production #64
expressionList :
    expression ( ',' expression )*
    ;


// *** Structured Values ***

// production #65
structuredValue :
    '{' ( valueComponent ( ',' valueComponent )* )? '}'	
    ;

// production #66
valueComponent :
    expression ( ( BY | '..' {}) constExpression )?
    ;

// production #66
// valueComponent :
//     constExpression ( ( BY | '..' {}) constExpression )? |
//     runtimeExpression
//     ;
//
// alias #66.1
// runtimeExpression : expression ;


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

// replacement for production #30
langExtn_simpleFormalType :
    typeIdent regAttribute? |
    ARRAY OF typeIdent |
    castingFormalType
    ;

// replacement for fragment #30.1
langExtn_castingFormalType :
    CAST ( ARRAY OF OCTET | addressTypeIdent regAttribute? )
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

// replacement for production #42
langExtn_statement :
    memMgtOperation | updateOrProcCall | ifStatement | caseStatement |
    loopStatement | whileStatement | repeatStatement | forStatement | 
    assemblyBlock | RETURN expression? | EXIT
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
	pragmaMSG | pragmaIF | procDeclAttrPragma | pragmaPTW | pragmaFORWARD |
    pragmaENCODING | pragmaALIGN | pragmaPADBITS | pragmaPURITY |
    varDeclAttrPragma | pragmaDEPRECATED | pragmaGENERATED |
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

// fragment #2.2
compileTimeMsgComponent :
    StringLiteral | constQualident | '@' valuePragma
    ;

// alias #2.3
constQualident : qualident ; /* no type and no variable identifiers */

// fragment #2.4
valuePragma :
    ALIGN | ENCODING | implDefinedPragmaSymbol
    ; 

// alias #2.5
implDefinedPragmaSymbol : Ident ; /* lowercase or mixed case only */

// production #3
pragmaIF :
    ( IF | ELSIF {}) inPragmaExpression | ELSE | ENDIF
    ;

// production #4
procDeclAttrPragma :
    INLINE | NOINLINE | NORETURN
    {} /* make ANTLRworks display separate branches */
    ;

// production #5
pragmaPTW :
    PTW
    ;

// production #6
pragmaFORWARD :
    FORWARD ( TYPE identList | procedureHeader )
    ;

/* multi-pass compilers ignore and skip any token after FORWARD */

// production #7
pragmaENCODING : 
    ENCODING '=' StringLiteral /* "ASCII" or "UTF8" */
    ( ':' codePointSampleList )?
    ;

// fragments #7.1a and #7.1b are combined into syntax diagram #7.1

// fragment #7.1a
codePointSampleList :
    codePointSample ( ',' codePointSample )*
    ;

// fragment #7.1b
codePointSample :
    quotedCharacterLiteral '=' characterCodeLiteral
    ;

// alias #7.2
quotedCharacterLiteral : StringLiteral ; /* single character only */

// alias #7.3
characterCodeLiteral : NumberLiteral ; /* unicode code points only */

// production #8
pragmaALIGN :
    ALIGN '=' inPragmaExpression
    ;

// production #9
pragmaPADBITS :
    PADBITS '=' inPragmaExpression
    ;

// production #10
pragmaPURITY :
    PURITY '=' inPragmaExpression /* values 0 .. 3 */
    ;

// production #11
varDeclAttrPragma :
    SINGLEASSIGN | LOWLATENCY | VOLATILE
    ;

// production #12
pragmaDEPRECATED :
    DEPRECATED
    ;

// production #13
pragmaGENERATED :
    GENERATED template ',' datestamp ',' timestamp
    ;

// fragment #13.1
datestamp :
    year '-' month '-' day
    ;

// fragment #13.2
timestamp :
    hours ':' minutes ':' seconds '+' timezone
    ;

// alias #13.3a
year : wholeNumber ;

// alias #13.3b
month : wholeNumber ;

// alias #13.3c
day : wholeNumber ;

// alias #13.3d
hours : wholeNumber ;

// alias #13.3e
minutes : wholeNumber ;

// alias #13.3f
seconds : wholeNumber ;

// alias #13.4g
timezone : wholeNumber ;

// production #14
pragmaADDR :
    ADDR '=' inPragmaExpression
    ;

// production #15
pragmaFFI :
    FFI '=' StringLiteral /* "C", "Fortran", "CLR" or "JVM" */
    ;

// production #16
pragmaFFIDENT :
    FFIDENT '=' StringLiteral /* foreign library identifier */
    ;

// production #17
implDefinedPragma :
    implDefinedPragmaSymbol ( '=' inPragmaExpression )? '|' messageMode
    ;

// production #18
inPragmaExpression :
/* represents operator precedence level 1 */
    inPragmaSimpleExpression ( inPragmaRelOp inPragmaSimpleExpression )?
    ;

// fragment #18.1
inPragmaRelOp :
    '=' | '#' | '<' | '<=' | '>' | '>='
    {} /* make ANTLRworks display separate branches */
    ;

// production #19
inPragmaSimpleExpression :
/* represents operator precedence level 2 */
    ( '+' | '-' {})? inPragmaTerm ( addOp inPragmaTerm )*
    ;

// fragment #19.1
addOp :
    '+' | '-' | OR
    {} /* make ANTLRworks display separate branches */
    ;

// production #20
inPragmaTerm :
/* represents operator precedence level 3 */
    inPragmaFactor ( mulOp inPragmaFactor )*
    ;

// fragment #20.1
mulOp :
    '*' | DIV | MOD | AND
    {} /* make ANTLRworks display separate branches */
    ;

// production #21
inPragmaFactorOrNegation :
/* represents operator precedence level 4 */
    NOT? inPragmaFactor
    ;

// production #22
inPragmaFactor :
    WholeNumber |
    /* constQualident is covered by inPragmaCompileTimeFunctionCall */
    '(' inPragmaExpression ')' | inPragmaCompileTimeFunctionCall
    ;

// production #23
inPragmaCompileTimeFunctionCall :
    qualident ( '(' inPragmaExpression ( ',' inPragmaExpression )* ')' )?
    ;
    

// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 7 productions

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
DualUseIdent : /* Ident */
    ABS | CAST | CHAR | COUNT | INSERT | INTEGER | LENGTH | NEG | NIL |
    READ | READNEW | REAL | REMOVE | RETRIEVE | STORE | SUBSET | SXF |
    TBASE | TBIDI | TLIMIT | TMAX | TMAXEXP | TMIN | TMINEXP | TNIL |
    TPRECISION | TSIGNED | UNICHAR | VAL | WRITE | WRITEF
    ;

// production #3
SpecialSymbol :
    '.' | ',' | ':' | ';' | '|' | '^' | '..' | '->' | '+' | '-' | '*' | '/' |
    BACKSLASH | '*.' | '+>' | '+\\' |  '=' | '#' | '>' | '>=' | '<' | '<=' |
    '==' | '::' | ':=' | '++' | '--' | SINGLE_QUOTE | DOUBLE_QUOTE | '@' |
    '//' | '(' | ')' | '[' | ']' | '{' | '}' | '(*' | '*)' | '<*' | '*>' |
    '<<' | '>>' |
    ;

// NB: SINGLE_QUOTE, DOUBLE_QUOTE, //, (*, *), << and >> are NOT tokens.
//     These symbols are NEVER returned as tokens by a Modula-2 lexer.

// production #4
Ident :
    ( Letter | ForeignIdentChar+ LetterOrDigit+ ) IdentTailChar*
    ;

// fragment #4.1
StdLibIdent :
    Letter LetterOrDigit*
    ;

fragment /* #4.2 */
LetterOrDigit :
    Letter | Digit
    ;

fragment /*  #4.3 */
ForeignIdentChar :
    '_' | '$'
    ;

fragment /* #4.4 */
IdentTailChar :
    LetterOrDigit | ForeignIdentChar
    ;

// production #5
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

fragment /* #5.1 */
DecimalNumberTail :
    DigitSep? DigitSeq RealNumberTail? | RealNumberTail
    ;

fragment /* #5.2 */
RealNumberTail :
    '.' DigitSeq ( 'e' ( '+' | '-' {})? DigitSeq )?
    ;

fragment /* #5.3 */
DigitSeq :
    Digit+ ( DigitSep Digit+ )*
    ;

fragment /* #5.4 */
Base16DigitSeq :
    Base16Digit+ ( DigitSep Base16Digit+ )*
    ;

fragment /* #5.5 */
Base2DigitSeq :
    Base2Digit+ ( DigitSep Base2Digit+ )*
    ;

fragment /* #5.6 */
Digit :
    Base2Digit | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #5.7 */
Base16Digit :
    Digit | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #5.8 */
Base2Digit :
    '0' | '1'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #5.9 */
DigitSep : SINGLE_QUOTE {} /* make ANTLRworks display name, not literal */ ;

// production #6
StringLiteral :
    SingleQuotedString | DoubleQuotedString
    ;

fragment /* #6.1 */
SingleQuotedString :
    SINGLE_QUOTE ( QuotableCharacter | DOUBLE_QUOTE )* SINGLE_QUOTE
    ;

fragment /* #6.2 */
DoubleQuotedString :
    DOUBLE_QUOTE ( QuotableCharacter | SINGLE_QUOTE )* DOUBLE_QUOTE
    ;

fragment /* #6.3 */
QuotableCharacter :
    Digit | Letter | Space | NonAlphaNumQuotable | EscapedCharacter
    ;

fragment /* #6.4 */
Letter :
    'A' .. 'Z' | 'a' .. 'z'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #6.5 */
Space : ' ' ;

fragment /* #6.6 */
NonAlphaNumQuotable :
    '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' |
    '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' | '@' |
    '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
    {} /* make ANTLRworks display separate branches */
    ;

fragment /* #6.7 */
EscapedCharacter :
    BACKSLASH ( 'n' | 't' | BACKSLASH {})
    ;

// production #7
ChevronText :
    '<<' ( QuotableCharacter | SINGLE_QUOTE | DOUBLE_QUOTE )* '>>'
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