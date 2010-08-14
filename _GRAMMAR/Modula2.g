/* (C) 2009, 2010 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status Aug 15, 2010 */


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
// Reserved Words and Fragments:
//  ALL_UPPERCASE with underscores separating words


options {

// *** enforce strict LL(1) ***

	k = 1; backtrack = no;
}


// ---------------------------------------------------------------------------
// T O K E N   S Y M B O L S
// ---------------------------------------------------------------------------
// 42 reserved words, 14 pragma words

tokens {
	
// *** Reserved Words, 44 tokens ***

	ALIAS          = 'ALIAS';
	AND            = 'AND';
	ARRAY          = 'ARRAY';
	ASSOCIATIVE    = 'ASSOCIATIVE';
	BEGIN          = 'BEGIN';
	BY             = 'BY';
	CASE           = 'CASE';
	CAST           = 'CAST';
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
	LOOP           = 'LOOP';
	MOD            = 'MOD';
	MODULE         = 'MODULE';
	NOT            = 'NOT';
	OF             = 'OF';
	OPAQUE         = 'OPAQUE';
	OR             = 'OR';
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

// *** Language Defined Pragma Words, 15 tokens ***

	IF             = 'IF';
	ELSIF          = 'ELSIF';
	ELSE           = 'ELSE';
	ENDIF          = 'ENDIF';
	INFO           = 'INFO';
	WARN           = 'WARN';
	ERROR          = 'ERROR';
	FATAL          = 'FATAL';
	ALIGN          = 'ALIGN';
	FOREIGN        = 'FOREIGN';
	MAKE           = 'MAKE';
	INLINE         = 'INLINE';
	NOINLINE       = 'NOINLINE';
	VOLATILE       = 'VOLATILE';
	ENCODING       = 'ENCODING';

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
// 67 productions

// *** Compilation Units ***

// production #1
compilationUnit :	
	prototype | programModule | definitionOfModule | implementationOfModule
	;

// production #2
prototype :
	PROTOTYPE prototypeId ';'
	TYPE '='
	( RECORD | OPAQUE RECORD? ( ':=' ( literalType | '{' '..' '}' ) )? ) ';'
	( ASSOCIATIVE ';' )?
	requiredBinding*
	END prototypeId '.'
	;

// alias
prototypeId : Ident ;

// alias
literalType : Ident ;

// production #3
programModule :
	MODULE moduleId ( '[' priority ']' )? ';'
	importList* block moduleId '.'
	;

// alias
moduleId : Ident ;

// alias
priority : constExpression ;

// production #4
definitionOfModule :
	DEFINITION MODULE moduleId ( '[' prototypeId ']' )? ';'
	importList* definition*
	END moduleId '.'
	;

// production #5
implementationOfModule :
	IMPLEMENTATION programModule
	;

// *** Bindings, Import Lists, Blocks, Declarations, Definitions ***

// production #6
requiredBinding :
    ( CONST '[' bindableIdent ']' |
	  PROCEDURE '[' ( bindableOperator | bindableIdent ) ']' ) ';' 
	;

// production #7
bindableOperator :
	DIV | MOD | IN | FOR |
	':=' | '?' | '!' | '~' | '+' | '-' | '*' | '/' | '=' | '<' | '>'
	{} // make ANTLRworks display separate branches
	;

// alias
bindableIdent : Ident ;
// TMIN, TMAX, ABS, NEG, ODD, COUNT, LENGTH, NEW, DISPOSE, SXF, VAL

// production #8
importList :
	( FROM moduleId IMPORT ( identList | '*' ) |
	IMPORT Ident '+'? ( ',' Ident '+'? )* ) ';'
	;

// production #9
block :
	declaration*
	( BEGIN statementSequence )? END
	;

// production #10
declaration :
	CONST ( constantDeclaration ';' )* |
	TYPE ( Ident '=' type ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureDeclaration ';'
	;

// production #11
definition :
	CONST ( ( '[' bindableIdent ']' )? constantDeclaration ';' )* |
	TYPE ( Ident '=' ( type | OPAQUE recordType? ) ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureHeader ';'
	;

// *** Constant Declarations ***

// production #12
constantDeclaration :	
	Ident '=' constExpression // no type identifiers
	;

// *** Type Declarations ***

// production #13
type :
	(( ALIAS | range ) OF )? namedType | enumerationType |
	arrayType | recordType | setType | pointerType | procedureType
	;

// alias
namedType : qualident ;

// production #14
range :
	'[' constExpression '..' constExpression ']'
	;

// production #15
enumerationType :
	'(' ( ( '+' namedType ) | Ident ) ( ',' ( ( '+' namedType ) | Ident ) )* ')'
	;

// production #16
arrayType :
	( ARRAY constComponentCount ( ',' constComponentCount )* |
	  ASSOCIATIVE ARRAY ) OF namedType
	;

// alias
constComponentCount : cardinalConstExpression ; // no type identifiers

// alias
cardinalConstExpression : constExpression ;

// production #17
recordType :
	RECORD ( '('  baseType  ')' )? fieldListSequence? END
	;

// alias
baseType : Ident ;

// production #18
fieldListSequence :
	fieldList ( ';' fieldList )*
	;

// production #19
fieldList :
	Ident
	( ( ',' Ident )+ ':' namedType |
	  ':' ( ARRAY determinantField OF )? namedType )
	;

// alias
determinantField : Ident ;

// production #20
setType :	
	SET OF ( namedEnumType | '(' identList ')' )
	;

// alias
namedEnumType : namedType ;

// production #21
pointerType :
	POINTER TO CONST? namedType
	;

// production #22
procedureType :
	PROCEDURE
	( '(' formalTypeList ')' )?
	( ':' returnedType )?
	;

// alias
returnedType : namedType ;

// production #23
formalTypeList :
	formalType ( ',' formalType )*
	;

// production #24
formalType :
	attributedFormalType | variadicFormalType
	;

// production #25
attributedFormalType :
	( CONST | VAR {})? simpleFormalType
	;

// production #26
simpleFormalType :
	( CAST? ARRAY OF )? namedType
	;

// production #27
variadicFormalType :
	VARIADIC OF
	( attributedFormalType |
	  '(' attributedFormalType ( ',' attributedFormalType )* ')'  )
	;

// *** Variable Declarations ***

// production #28
variableDeclaration :
	Ident ( '[' machineAddress ']' | ',' identList )?
	':' ( ARRAY constComponentCount OF )? namedType 
	;

// alias
machineAddress : constExpression ;

// *** Procedure Declarations ***

// production #29
procedureDeclaration :
	procedureHeader ';' block Ident
	;

// production #30
procedureHeader :
	PROCEDURE
	( '[' ( '::' | bindableOperator | bindableIdent ) ']' )?
	Ident ( '(' formalParamList ')' )? ( ':' returnedType )?
	;

// production #31
formalParamList :
	formalParams ( ';' formalParams )*
	;

// production #32
formalParams :
	formalValueParams | formalConstOrVarParams
    ;

formalValueParams : // #32a
    identList ':'
    ( simpleFormalType | variadicAttribute
      ( simpleFormalType | '(' simpleFormalParams ( ';' simpleFormalParams )* ')' ) )
    ;

formalConstOrVarParams : // #32b
    ( CONST | VAR {}) identList ':' variadicAttribute? simpleFormalType
    ;

// production #33
simpleFormalParams :
	( CONST | VAR {})? identList ':' simpleFormalType
	;

// production £34
variadicAttribute :
     VARIADIC ( variadicCounter | '[' variadicTerminator ']' )? OF
	;

// alias
variadicCounter : Ident ;

// alias
variadicTerminator : constExpression ;

// *** Statements ***

// production #35
statement :
	( assignmentOrProcedureCall | ifStatement | caseStatement |
	  whileStatement | repeatStatement | loopStatement |
	  forStatement | RETURN expression? | EXIT )?
	;

// production #36
statementSequence :
	statement ( ';' statement )*
	;

// production #37
assignmentOrProcedureCall :
	designator ( ':=' expression | '++' | '--' | actualParameters )?
	;

// production #38
ifStatement :
	IF expression THEN statementSequence
	( ELSIF expression THEN statementSequence )*
	( ELSE statementSequence )?
	END
	;

// production #39
caseStatement :
	CASE expression OF case ( '|' case )* ( ELSE statementSequence )? END
	;

// production #40
case :
	caseLabels ( ',' caseLabels )* ':' statementSequence
	;

// production #41
caseLabels :
	constExpression ( '..' constExpression )?
	;

// production #42
whileStatement :
	WHILE expression DO statementSequence END
	;

// production #43
repeatStatement :
	REPEAT statementSequence UNTIL expression
	;

// production #44
loopStatement :
	LOOP statementSequence END
	;

// production #45
forStatement :
    FOR DESCENDING? controlVariable ( OF namedType )?
    IN ( expression | range OF namedType )
    DO statementSequence END
    ;

// under consideration
forToByStatement :
    FOR controlVariable OF namedType
    FROM expression TO expression ( BY constExpression )?
    DO statementSequence END
    ;

// under consideration
combinedForStatement :
    FOR controlVariable
    ( DESCENDING? IN ( expression | range OF namedType) |
      OF namedType ':=' expression TO expression ( BY constExpression )? )
    DO statementSequence END
    ;

// alias
controlVariable : Ident ;


// *** Expressions ***

// production #46
constExpression :
	simpleConstExpr ( relation simpleConstExpr )?
	;

// production #47
relation :
	'=' | '#' | '<' | '<=' | '>' | '>=' | IN
	{} // make ANTLRworks display separate branches
	;

// production #48
simpleConstExpr :
	( '+' | '-' {})? constTerm ( addOperator constTerm )*
	;

// production #49
addOperator :
	'+' | '-' | OR
	{} // make ANTLRworks display separate branches
	;

// production #50
constTerm :
	constFactor ( mulOperator constFactor )*
	;

// production #51
mulOperator :
	'*' | '/' | DIV | MOD | AND
	{} // make ANTLRworks display separate branches
	;

// production #52
constFactor :
	( Number | String | constQualident | constStructuredValue |
	  '(' constExpression ')' | CAST '(' namedType ',' constExpression ')' )
	( '::' namedType )?
	| NOT constFactor
	;

// production #53
designator :
	qualident designatorTail?
	;

// production #54
designatorTail :
	( ( '[' expressionList ']' | '^' ) ( '.' Ident )* )+
	;

// production #55
expressionList :
	expression ( ',' expression )*
	;

// production #56
expression :
	simpleExpression ( relation simpleExpression )?
	;

// production #57
simpleExpression :
	( '+' | '-' {})? term ( addOperator term )*
	;

// production #58
term :
	factor ( mulOperator factor )*
	;

// production #59
factor :
	( Number | String | structuredValue |
	  designatorOrProcedureCall | '(' expression ')' |
	  CAST '(' namedType ',' expression ')' )
	( '::' namedType )?
	| NOT factor
	;

// production #60
designatorOrProcedureCall :
	qualident designatorTail? actualParameters?
	;

// production #61
actualParameters :
	'(' expressionList? ')'
	;

// *** Value Constructors ***

// production #62
constStructuredValue :
	'{' ( constValueComponent ( ',' constValueComponent )* )? '}'	
	;

// production #63
constValueComponent :
	constExpression ( ( BY | '..' {}) constExpression  )?
	;

// production #64
structuredValue :
	'{' ( valueComponent ( ',' valueComponent )* )? '}'	
	;

// production #65
valueComponent :
	expression ( ( BY | '..' {}) constExpression )?
	;

// *** Identifiers ***

// production #66
qualident :
	Ident ( '.' Ident )*
	;

// production #67
identList :
	Ident ( ',' Ident )*
	;

// alias
constQualident : qualident ; // no type and no variable identifiers


// ---------------------------------------------------------------------------
// P R A G M A   S Y M B O L S
// ---------------------------------------------------------------------------
// 6 productions

// *** Pragmas ***

// production #1
pragma :
	'<*'
	( lexicalPragma | conditionalPragma | compileTimeMessagePragma |
	  codeGenerationPragma | implementationDefinedPragma )
	'*>'
	;

// production £2

lexicalPragma :
    ENCODING '=' encodingId ( ';' codepoint '=' quotedChar )+
    ;

// production #3
conditionalPragma :
	( IF | ELSIF {}) constExpression | ELSE | ENDIF
	;

// production #4
compileTimeMessagePragma :
	( INFO | WARN | ERROR | FATAL {}) compileTimeMessage
	;

// alias
compileTimeMessage : String ;

// production #5
codeGenerationPragma :
	ALIGN '=' constExpression | FOREIGN ( '=' String )? | MAKE '=' String |
	INLINE | NOINLINE | VOLATILE
	;

// production #6
implementationDefinedPragma :
	pragmaName ( '+' | '-' | '=' constExpression )?
	;

// alias
pragmaName : Ident ; // lowercase or camelcase only

// alias
encodingId : String ; // "7BIT" or "UTF8"

// alias
codepoint : Number ; // character code literal only

// alias
quotedChar : String ; // single character string literal


// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 8 productions

// production #1
Ident :
	( '_' | '$' | LETTER ) ( '_' | '$' | LETTER | DIGIT )*
	;

// production #2
Number :
	// Decimal integer
	DIGIT+ |
	
	// Binary integer
	( '0' | '1' {})+ 'B' |
	
	// Sedecimal integer
	DIGIT BASE16_DIGIT* ( 'C' | 'H' {}) |
	
	// Real number
	DIGIT+ '.' DIGIT+ ( 'E' ( '+' | '-' {})? DIGIT+ )?
	;

// production #3
String :
//  Proper EBNF for STRING
    SINGLE_QUOTE ( CHARACTER | DOUBLE_QUOTE )* SINGLE_QUOTE |
    DOUBLE_QUOTE ( CHARACTER | SINGLE_QUOTE )* DOUBLE_QUOTE
//
//  Alternative EBNF to make ANTLRworks display the diagram properly
//	SINGLE_QUOTE ( CHARACTER | DOUBLE_QUOTE | )+ SINGLE_QUOTE |
//	DOUBLE_QUOTE ( CHARACTER | SINGLE_QUOTE | )+ DOUBLE_QUOTE
	;

// production #4
fragment
LETTER :
	'A' .. 'Z' | 'a' .. 'z'
	{} // make ANTLRworks display separate branches
	;

// production #5
fragment
DIGIT :
	'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
	{} // make ANTLRworks display separate branches
	;

// production #6
fragment
BASE16_DIGIT :
	DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
	{} // make ANTLRworks display separate branches
	;

// production #7
fragment
CHARACTER :
	DIGIT | LETTER | 	
	// any printable characters other than single and double quote
	' ' | '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' |
	',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' |
	'@' | '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' |
	ESCAPE_SEQUENCE 
	;

// production #8
fragment
ESCAPE_SEQUENCE :
	BACKSLASH
	( '0' | 'n' | 'r' | 't' | BACKSLASH | SINGLE_QUOTE | DOUBLE_QUOTE {})
	;


// ---------------------------------------------------------------------------
// I G N O R E   S Y M B O L S
// ---------------------------------------------------------------------------
// 5 productions

// *** Whitespace ***

// production #1
WHITESPACE :
	' ' | ASCII_TAB
	{} // make ANTLRworks display separate branches
	{ $channel = HIDDEN; } // ignore
	;

// *** Comments ***

// production #2
COMMENT :
	MULTI_LINE_COMMENT | SINGLE_LINE_COMMENT
	{ $channel = HIDDEN; } // ignore
	;

// production #3
fragment
MULTI_LINE_COMMENT :
	'(*'
	( options { greedy=false; }: . )* // anything other than '(*' or '*)'
	MULTI_LINE_COMMENT*
	'*)'
	;	

// production #5
fragment
SINGLE_LINE_COMMENT :
	'//'
	( options { greedy=false; }: . )* // anything other than EOL
	END_OF_LINE
	;

// production #5
fragment
END_OF_LINE :
	ASCII_LF ASCII_CR? | ASCII_CR ASCII_LF?
	;

// END OF FILE
