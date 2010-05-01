/* (C) 2009, 2010 by B.Kowarsch & R.Sutcliffe. All rights reserved. */

grammar Modula2;

/* M2R10 grammar in ANTLR EBNF notation -- status May 1, 2010 */


options {

// *** enforce strict LL(1) ***

	k = 1; backtrack = no;
}


// ---------------------------------------------------------------------------
// T O K E N   S Y M B O L S
// ---------------------------------------------------------------------------
// 41 reserved words, 14 pragma words

tokens {
	
// *** Reserved Words, 41 tokens ***

	ALIAS          = 'ALIAS';
	AND            = 'AND';
	ARRAY          = 'ARRAY';
	ASSOCIATIVE    = 'ASSOCIATIVE';
	BEGIN          = 'BEGIN';
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
// 65 productions

// *** Compilation Units ***

// production #1
compilationUnit :	
	programModule | definitionOfModule | implementationOfModule
	;

// production #2
programModule :
	MODULE moduleId ( '[' priority ']' )? ';'
	importList* block moduleId '.'
	;

// production #3
definitionOfModule :
	DEFINITION MODULE moduleId ';'
	importList* definition*
	END moduleId '.'
	;

// production #4
implementationOfModule :
	IMPLEMENTATION programModule
	;

// alias
moduleId : ident ;

// alias
priority : constExpression ;

// *** Import Lists, Blocks, Declarations, Definitions ***

// production #5
importList :
	( FROM moduleId IMPORT ( identList | '*' ) |
	IMPORT ident '+'? ( ',' ident '+'? )* ) ';'
	;

// production #6
block :
	declaration*
	( BEGIN statementSequence )? END
	;

// production #7
declaration :
	CONST ( constantDeclaration ';' )* |
	TYPE ( ident '=' type ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureDeclaration ';'
	;

// production #8
definition :
	CONST ( ( '[' ident ']' )? constantDeclaration ';' )* |
	TYPE ( ident '=' ( type | opaqueType ) ';' )* |
	VAR ( variableDeclaration ';' )* |
	procedureHeader ';'
	;

// *** Constant Declarations ***

// production #9
constantDeclaration :	
	ident '=' constExpression
	;

// *** Type Declarations ***

// production #10
type :
	( ALIAS OF )? namedType | anonymousType | enumerationType | setType
	;

// alias
namedType : qualident ;

// production #11
opaqueType :
	OPAQUE ( '(' semanticType ')' | recordType )?
	;

// "A-Type", "S-Type", "Z-Type", "R-Type", "C-Type", "V-Type"
semanticType : string ;

// production #12
anonymousType :
	arrayType | recordType | pointerType | procedureType
	;

// production #13
enumerationType :
	'(' ( ( '+' namedType ) | ident ) ( ',' ( ( '+' namedType ) | ident ) )* ')'
	;

// production #14
arrayType :
	( ARRAY arrayIndex ( ',' arrayIndex )* | ASSOCIATIVE ARRAY )
	OF ( namedType | recordType | procedureType )
	;

// alias
arrayIndex : ordinalConstExpression ;

// alias
ordinalConstExpression : constExpression ;

// production #15
recordType :
	RECORD ( '(' ( semanticType | baseType ) ')' )? fieldListSequence? END
	;

// alias
baseType : ident ;

// production #16
fieldListSequence :
	fieldList ( ';' fieldList )*
	;

// production #17
fieldList :
	identList ':'
	( namedType | arrayType | recordType | procedureType )
	;

// production #18
setType :
	SET ( OF ( namedType | '(' identList ')' ) | '[' constExpression ']' )
	;

// production #19
pointerType :
	POINTER TO CONST? namedType
	;

// production #20
procedureType :
	PROCEDURE
	( '(' formalTypeList ')' )?
	( ':' returnedType )?
	;

// production #21
formalTypeList :
	attributedFormalType ( ',' attributedFormalType )*
	;

// production #22
attributedFormalType :
	( CONST | VAR {})? formalType	
	;

// production #23
formalType :
	( ARRAY OF )? namedType
	;

// alias
returnedType : namedType ;

// *** Variable Declarations ***

// production #24
variableDeclaration :
	ident ( '[' machineAddress ']' | ',' identList )?
	':' ( namedType | anonymousType )
	;

// alias
machineAddress : constExpression ;

// *** Procedure Declarations ***

// production #25
procedureDeclaration :
	procedureHeader ';' block ident
	;

// production #26
procedureHeader :
	PROCEDURE
	( '[' ( boundToOperator | ident ) ']' )?
	( '(' ident ':' receiverType ')' )?
	ident ( '(' formalParamList ')' )? ( ':' returnedType )?
	;

// production #27
boundToOperator :
	DIV | MOD | IN | FOR | TO | FROM |
	':=' | '::' | '.' | '!' | '+' | '-' | '*' | '/' | '=' | '<' | '>'
	{} // make ANTLRworks display separate branches
	;

// alias
receiverType : ident ;

// production #28
formalParamList :
	formalParams ( ';' formalParams )*
	;

// production #29
formalParams :
	simpleFormalParams | variadicFormalParams
    ;

// production #30
simpleFormalParams :
	( CONST | VAR {})? identList ':' formalType
	;

// production #31
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

// production #32
statement :
	( assignmentOrProcedureCall | ifStatement | caseStatement |
	  whileStatement | repeatStatement | loopStatement |
	  forStatement | RETURN expression? | EXIT )?
	;

// production #33
statementSequence :
	statement ( ';' statement )*
	;

// production #34
assignmentOrProcedureCall :
	designator
	( ':=' expression | '++' | '--' | actualParameters )?
	;

// production #35
ifStatement :
	IF expression THEN statementSequence
	( ELSIF expression THEN statementSequence )*
	( ELSE statementSequence )?
	END
	;

// production #36
caseStatement :
	CASE expression OF case ( '|' case )*
	( ELSE statementSequence )?
	END
	;

// production #37
case :
	caseLabelList ':' statementSequence
	;

// production #38
caseLabelList :
	caseLabels ( ',' caseLabels )*
	;

// production #39
caseLabels :
	constExpression ( '..' constExpression )?
	;

// production #40
whileStatement :
	WHILE expression DO statementSequence END
	;

// production #41
repeatStatement :
	REPEAT statementSequence UNTIL expression
	;

// production #42
loopStatement :
	LOOP statementSequence END
	;

// production #43
forStatement :
	FOR ident
	( ':=' expression TO expression ( BY constExpression )? | IN expression )
	DO statementSequence END
	;

// *** Expressions ***

// production #44
constExpression :
	simpleConstExpr ( relation simpleConstExpr | '::' namedType )?
	;

// production #45
relation :
	'=' | '#' | '<' | '<=' | '>' | '>=' | IN
	{} // make ANTLRworks display separate branches
	;

// production #46
simpleConstExpr :
	( '+' | '-' {})? constTerm ( addOperator constTerm )*
	;

// production #47
addOperator :
	'+' | '-' | OR
	{} // make ANTLRworks display separate branches
	;

// production #48
constTerm :
	constFactor ( mulOperator constFactor )*
	;

// production #49
mulOperator :
	'*' | '/' | DIV | MOD | AND | '&'
	{} // make ANTLRworks display separate branches
	;

// production #50
constFactor :
	number | string | qualident | constStructuredValue |
	'(' constExpression ')' | ( NOT | '~' {}) constFactor
	;

// production #51
designator :
	qualident designatorTail?
	;

// production #52
designatorTail :
	( ( '[' expressionList ']' | '^' ) ( '.' ident )* )+
	;

// production #53
expressionList :
	expression ( ',' expression )*
	;

// production #54
expression :
	simpleExpression ( relation simpleExpression | '::' namedType )?
	;

// production #55
simpleExpression :
	( '+' | '-' {})? term ( addOperator term )*
	;

// production #56
term :
	factor ( mulOperator factor )*
	;

// production #57
factor :
	number |
	string |
	structuredValue |
	designatorOrProcedureCall |
	'(' expression ')' | ( NOT | '~' {}) factor
	;

// production #58
designatorOrProcedureCall :
	qualident ( designatorTail? actualParameters? )
	;

// production #59
actualParameters :
	'(' expressionList? ')'
	;

// *** Value Constructors ***

// production #60
constStructuredValue :
	'{' ( constValueComponent ( ',' constValueComponent )* )? '}'	
	;

// production #61
constValueComponent :
	constExpression ( ( BY | '..' {}) constExpression  )?
	;

// production #62
structuredValue :
	'{' ( valueComponent ( ',' valueComponent )* )? '}'	
	;

// production #63
valueComponent :
	expression ( ( BY | '..' {}) constExpression )?
	;

// *** Identifiers ***

// production #64
qualident :
	ident ( '.' ident )*
	;

// production #65
identList :
	ident ( ',' ident )*
	;

// alias
ident :	IDENT ;

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
	( conditionalPragma | consoleMessagePragma | codeGenerationPragma |	
	  implementationDefinedPragma )
	'*>'
	;

// production #2
conditionalPragma :
	( IF | ELSIF {}) constExpression | ELSE | ENDIF
	;

// production #3
consoleMessagePragma :
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
pragmaName : ident ;


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
