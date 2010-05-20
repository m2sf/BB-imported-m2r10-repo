/* (C) 2009, 2010 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status May 20, 2010 */


options {

// *** enforce strict LL(1) ***

	k = 1; backtrack = no;
}


// ---------------------------------------------------------------------------
// T O K E N   S Y M B O L S
// ---------------------------------------------------------------------------
// 42 reserved words, 14 pragma words

tokens {
	
// *** Reserved Words, 42 tokens ***

	ALIAS          = 'ALIAS';
	AND            = 'AND';
	ARRAY          = 'ARRAY';
	ASSOCIATIVE    = 'ASSOCIATIVE';
	BEGIN          = 'BEGIN';
	BINDINGS       = 'BINDINGS';
	BY             = 'BY';
	CASE           = 'CASE';
	CONST          = 'CONST';
	DEFINITION     = 'DEFINITION';
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

// *** Language Defined Pragma Words, 14 tokens ***

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
	programModule | definitionOfBindings |
	definitionOfModule | implementationOfModule
	;

// production #2
definitionOfBindings :
	BINDINGS FOR semanticType ';'
	bindingsHeader bindings*
	END semanticType '.'
	;

// production #3
programModule :
	MODULE moduleId ( '[' priority ']' )? ';'
	importList* block moduleId '.'
	;

// production #4
definitionOfModule :
	DEFINITION MODULE moduleId ( '[' semanticType ']' )? ';'
	importList* definition*
	END moduleId '.'
	;

// production #5
implementationOfModule :
	IMPLEMENTATION programModule
	;

// alias
moduleId : ident ;

// alias
priority : constExpression ;

// alias
semanticType : ident ;


// *** Bindings, Import Lists, Blocks, Declarations, Definitions ***

// production #6
bindingsHeader :
	TYPE '=' ( RECORD | OPAQUE RECORD? ( ':=' ( literalType | '{' '}' ) )? ) ';'
	;

// production #7
bindings :
    ( CONST '[' bindableIdent ']' |
	  PROCEDURE '[' ( bindableOperator | bindableIdent ) ']' ) ';' 
	;

// production #8
bindableOperator :
	DIV | MOD | IN | FOR | TO | FROM |
	':=' | '::' | '.' | '!' | '+' | '-' | '*' | '/' | '=' | '<' | '>'
	{} // make ANTLRworks display separate branches
	;

// alias
bindableIdent : ident ;
// TMIN, TMAX, ABS, NEG, ODD, COUNT, LENGTH, NEW, DISPOSE

// alias
literalType : ident ;

// production #9
importList :
	( FROM moduleId IMPORT ( identList | '*' ) |
	IMPORT ident '+'? ( ',' ident '+'? )* ) ';'
	;

// production #10
block :
	declaration*
	( BEGIN statementSequence )? END
	;

// production #11
declaration :
	CONST ( constantDeclaration ';' )* |
	TYPE ( ident '=' type ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureDeclaration ';'
	;

// production #12
definition :
	CONST ( ( '[' ident ']' )? constantDeclaration ';' )* |
	TYPE ( ident '=' ( type | OPAQUE recordType? ) ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureHeader ';'
	;

// *** Constant Declarations ***

// production #13
constantDeclaration :	
	ident '=' constExpression // no type identifiers
	;

// *** Type Declarations ***

// production #14
type :
	( ALIAS OF | '[' constExpression '..' constExpression ']' OF )? namedType |
	anonymousType | enumerationType | recordType | setType
	;

// alias
namedType : qualident ;

// production #15
anonymousType :
	arrayType | pointerType | procedureType
	;

// production #16
enumerationType :
	'(' ( ( '+' namedType ) | ident ) ( ',' ( ( '+' namedType ) | ident ) )* ')'
	;

// production #17
arrayType :
	( ARRAY arrayIndex ( ',' arrayIndex )* | ASSOCIATIVE ARRAY )
	OF ( namedType | pointerType | procedureType )
	;

// alias
arrayIndex : ordinalConstExpression ; // no type identifiers

// alias
ordinalConstExpression : constExpression ;

// production #18
recordType :
	RECORD ( '('  baseType  ')' )? fieldListSequence? END
	;

// alias
baseType : ident ;

// production #19
fieldListSequence :
	fieldList ( ';' fieldList )*
	;

// production #20
fieldList :
	identList ':'
	( namedType | arrayType | pointerType | procedureType )
	;

// production #21
setType :	
	SET OF ( namedEnumType | '(' identList ')' )
	;

// alias
namedEnumType : namedType ;

// production #22
pointerType :
	POINTER TO CONST? namedType
	;

// production #23
procedureType :
	PROCEDURE
	( '(' formalTypeList ')' )?
	( ':' returnedType )?
	;

// production #24
formalTypeList :
	attributedFormalType ( ',' attributedFormalType )*
	;

// production #25
attributedFormalType :
	( CONST | VAR {})? formalType	
	;

// production #26
formalType :
	( ARRAY OF )? namedType
	;

// alias
returnedType : namedType ;

// *** Variable Declarations ***

// production #27
variableDeclaration :
	ident ( '[' machineAddress ']' | ',' identList )?
	':' ( namedType | anonymousType )
	;

// alias
machineAddress : constExpression ;

// *** Procedure Declarations ***

// production #28
procedureDeclaration :
	procedureHeader ';' block ident
	;

// production #29
procedureHeader :
	PROCEDURE
	( '[' ( bindableOperator | ident ) ']' )?
	( '(' ident ':' receiverType ')' )?
	ident ( '(' formalParamList ')' )? ( ':' returnedType )?
	;

// alias
receiverType : ident ;

// production #30
formalParamList :
	formalParams ( ';' formalParams )*
	;

// production #31
formalParams :
	simpleFormalParams | variadicFormalParams
    ;

// production #32
simpleFormalParams :
	( CONST | VAR {})? identList ':' formalType
	;

// production #33
variadicFormalParams :
	VARIADIC ( variadicCounter | '[' variadicTerminator ']' )? OF
	( ( CONST | VAR {})? formalType |
	  '(' simpleFormalParams ( ';' simpleFormalParams )* ')' )
	;

// alias
variadicCounter : ident ;

// alias
variadicTerminator : constExpression ;

// *** Statements ***

// production #34
statement :
	( assignmentOrProcedureCall | ifStatement | caseStatement |
	  whileStatement | repeatStatement | loopStatement |
	  forStatement | RETURN expression? | EXIT )?
	;

// production #35
statementSequence :
	statement ( ';' statement )*
	;

// production #36
assignmentOrProcedureCall :
	designator ( ':=' expression | '++' | '--' | actualParameters )?
	;

// production #37
ifStatement :
	IF expression THEN statementSequence
	( ELSIF expression THEN statementSequence )*
	( ELSE statementSequence )?
	END
	;

// production #38
caseStatement :
	CASE expression OF case ( '|' case )*
	( ELSE statementSequence )?
	END
	;

// production #39
case :
	caseLabelList ':' statementSequence
	;

// production #40
caseLabelList :
	caseLabels ( ',' caseLabels )*
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
	FOR ident
	( IN expression |
	  ':' namedType ':=' expression TO expression ( BY constExpression )? )
	DO statementSequence END
	;

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
	'*' | '/' | DIV | MOD | AND | '&'
	{} // make ANTLRworks display separate branches
	;

// production #52
constFactor :
	( number | string | constQualident | constStructuredValue |
	'(' constExpression ')' ) ( '::' namedType )?
	| ( NOT | '~' {}) constFactor
	;

// production #53
designator :
	qualident designatorTail?
	;

// production #54
designatorTail :
	( ( '[' expressionList ']' | '^' ) ( '.' ident )* )+
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
	( number | string | structuredValue |
	  designatorOrProcedureCall | '(' expression ')' )
	( '::' namedType )?
	| ( NOT | '~' {}) factor
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
	ident ( '.' ident )*
	;

// production #67
identList :
	ident ( ',' ident )*
	;

// alias
ident :	IDENT ;

// alias
constQualident : qualident ; // no type and no variable identifiers

// *** Literals ***

// alias
number : NUMBER ;

// alias
string : STRING ;


// ---------------------------------------------------------------------------
// P R A G M A   S Y M B O L S
// ---------------------------------------------------------------------------
// 5 productions

// *** Pragmas ***

// production #1
pragma :
	'<*'
	( conditionalPragma | compileTimeMessagePragma | codeGenerationPragma |
	  implementationDefinedPragma )
	'*>'
	;

// production #2
conditionalPragma :
	( IF | ELSIF {}) constExpression | ELSE | ENDIF
	;

// production #3
compileTimeMessagePragma :
	( INFO | WARN | ERROR | FATAL {}) compileTimeMessage
	;

// production #4
codeGenerationPragma :
	ALIGN '=' constExpression | FOREIGN ( '=' string )? | MAKE '=' string |
	INLINE | NOINLINE | VOLATILE
	;

// production #5
implementationDefinedPragma :
	pragmaName ( '+' | '-' | '=' ( ident | number ) )?
	;

// alias
compileTimeMessage : string ;

// alias
pragmaName : ident ; // lowercase or camelcase only


// ---------------------------------------------------------------------------
// T E R M I N A L   S Y M B O L S
// ---------------------------------------------------------------------------
// 9 productions

// production #1
IDENT :
	( '_' | '$' | LETTER ) ( '_' | '$' | LETTER | DIGIT )*
	;

// production #2
NUMBER :
	// Decimal integer
	DIGIT+ |
	
	// Binary integer
	BINARY_DIGIT+ 'B' |
	
	// Sedecimal integer
	DIGIT SEDECIMAL_DIGIT* ( 'C' | 'H' {}) |
	
	// Real number
	DIGIT+ '.' DIGIT+ ( 'E' ( '+' | '-' {})? DIGIT+ )?
	;

// production #3
STRING :
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
	BINARY_DIGIT | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
	{} // make ANTLRworks display separate branches
	;

// production #6
fragment
BINARY_DIGIT :
	'0' | '1'
	{} // make ANTLRworks display separate branches
	;

// production #7
fragment
SEDECIMAL_DIGIT :
	DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
	{} // make ANTLRworks display separate branches
	;

// production #8
fragment
CHARACTER :
	DIGIT | LETTER | 	
	// any printable characters other than single and double quote
	' ' | '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' |
	',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '?' |
	'@' | '[' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~' |
	ESCAPE_SEQUENCE 
	;

// production #9
fragment
ESCAPE_SEQUENCE :
	BACKSLASH
	( '0' | 'n' | 'r' | 't' | BACKSLASH | SINGLE_QUOTE | DOUBLE_QUOTE {})
	;


// ---------------------------------------------------------------------------
// I G N O R E   S Y M B O L S
// ---------------------------------------------------------------------------
// 6 productions

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
	NESTABLE_COMMENT | NON_NESTABLE_COMMENT | SINGLE_LINE_COMMENT
	{ $channel = HIDDEN; } // ignore
	;

// production #3
fragment
NESTABLE_COMMENT :
	'(*'
	( options { greedy=false; }: . )* // anything other than '(*' or '*)'
	NESTABLE_COMMENT*
	'*)'
	;	

// production #4
fragment
NON_NESTABLE_COMMENT :
	'/*'
	( options { greedy=false; }: . )* // anything other than '*/'
	'*/'
	;

// production #5
fragment
SINGLE_LINE_COMMENT :
	'//'
	( options { greedy=false; }: . )* // anything other than EOL
	END_OF_LINE
	;

// production #6
fragment
END_OF_LINE :
	ASCII_LF ASCII_CR? | ASCII_CR ASCII_LF?
	;

// END OF FILE
