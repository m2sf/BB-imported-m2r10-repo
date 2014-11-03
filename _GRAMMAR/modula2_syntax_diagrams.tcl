#!/usr/bin/wish
#
# Syntax diagram generator for Modula-2 (R10), status Oct 31, 2014
#
# This script is derived from the SQLite project's bubble-generator script.
# It is quite possibly the only such tool that can wrap-around diagrams so
# that they do not go off-page when inserting them into an ordinary A4  or
# US letter document. Thanks to the folks at the SQLite project for making
# their script available to the public.
#
# The present version of the script was cleaned up,  enhanced,  documented
# and modified by B.Kowarsch to become accessible to those unfamiliar with
# TCL/TK, and in particular to generate syntax diagrams for Modula-2 (R10).
# It is located at http://modula2.net/resources/modula2_syntax_diagrams.tcl
#
# Ideally the design would have been changed such that the script can read
# grammars from a text file in EBNF notation.  Ideally,  this script would
# have been rewritten in a more readable language,  Modula-2 for instance.
# Due to time constraints,  these tasks had to be left for some other time
# or for somebody else to do.  In the meantime  the documentation embedded
# herein should suffice  even for those unfamiliar with TCL  to modify the
# script to generate diagrams for their own grammars.
#
# THIS SOFTWARE COMES WITHOUT ANY WARRANTY OF ANY KIND. USE IS STRICTLY AT
# THE RISK OF THE USER.  PERSONS WHO HAVE A  LEGAL RIGHT TO SUE AUTHORS OF
# NO-WARRANTY-FREE-OF-CHARGE OPEN SOURCE SOFTWARE  ARE  SPECIFICALLY *NOT*
# GIVEN ANY PERMISSION TO USE THIS SOFTWARE.  THE BOTTOM LINE IS : YOU MAY
# USE THIS SOFTWARE WITHOUT PERMISSION ANYWAY,  BUT YOU  *CANNOT*  SUE THE
# AUTHORS FOR DAMAGES  BECAUSE  IF YOUR GOVERNMENT  GRANTS YOU SUCH RIGHTS
# THEN YOU DID  *NOT*  HAVE PERMISSION TO USE THIS SOFTWARE TO BEGIN WITH.
# THUS, NO MATTER WHAT THE CIRCUMSTANCES,  THE RISK IS ALWAYS YOURS ALONE.


#
# Top-level displays
#
toplevel .bb
canvas .c -bg white
pack .c -side top -fill both -expand 1
wm withdraw .

#  
# ===========================================================================
# D O C U M E N T A T I O N
# ===========================================================================
#
# The grammar is encoded as a nested TCL list structure of the general form:
#
#   { production1 { ... } production2 { ... } ... }
#
# Production rules can be translated from (ANTLR) EBNF to TCL list items as
# follows:
#
# Simple term
#
#   production : term ;
#   => production { line term }
#
# Sequence and group
#
#   production : term1 term2 ;
#   => production { line term1 term2 }
#
# Alternative
#
#   production : term1 | term2 ;
#   => production { or term1 term2 }
#
# Optional term
#
#   production : term? ;
#   => production { opt term }
#   => production { optx term }
#
#  opt renders the bypass line in the main path
#  optx renders the term in the main path
#
# Terms that occur one or more times
#
#   production : term+ ;
#   => production { loop { line term } {} }
#
# Terms that occur zero or more times
#
#   production : term* ;
#   => production { loop {} { nil term } }
#
# Causing diagrams to wrap-around
#
#   production : term1 /* wrap here */ term2 /* wrap here */ term3 ;
#   => production { stack {line term1} {line term2} {line term3} }
#
# Rendering of terminals, non-terminals and tokens
#
#   names starting with a lowercase letter are rendered as non-terminals
#   names starting with a uppercase letter are rendered as terminals
#   names preceded by a / are rendered as tokens with / stripped off
#   any other characters or character sequences are rendered as tokens
#
# Special names for tokens that TCL cannot handle verbatim
#
#   BACKSLASH is rendered as \
#   SINGLE_QUOTE is rendered as '
#   DOUBLE_QUOTE is rendered as "
#   LEFT_BRACE is rendered as {
#   RIGHT_BRACE is rendered as }
#
# Rendering parameters
#
#   LWIDTH - line width
#   TERMFONT - font/size/style used to render terminals
#   NONTERMFONT - font/size/style used to render non-terminals
#   TOKENFONT - font/size/style used to render tokens
#   RADIUS - turn radius for arcs
#   HSEP - horizontal separation
#   VSEP - vertical separation
#   LWIDTH -line width
#
# Pre-requisites
#
#   TCL and TK need to be installed
#
# Running the script
#
#   the most fool-proof method to run this script is to call the wish shell
#   with the name of the script as an argument:
#
#   $ wish modula2_syntax_diagrams.tcl
#
#   in the window that appears, click on the top button "Draw All Diagrams"
#
#   Diagrams will be written into postscript files in the working directory
#  
# ===========================================================================
#

# ===========================================================================
# Modula-2 grammar
# ===========================================================================
#  To reuse this diagram generator for other languages, replace the following
#  section with a definition of the grammar of the target language.
#
#  Do NOT add comments or blank lines within a production rule's definition!
#

# ---------------------------------------------------------------------------
# Non-Terminal Symbols
# ---------------------------------------------------------------------------
#
set non_terminals {}

# (1) Compilation Unit
lappend non_terminals compilationUnit {
  or
    {line {optx IMPLEMENTATION} programModule}
    {line definitionModule}
    {line blueprint}
}

# (2) Program or Implementation Module
lappend non_terminals programModule {
  line MODULE moduleIdent ;
  {loop nil {nil importList nil}} block moduleIdent .
}

# (2.1) Module Identifier
lappend non_terminals moduleIdent {
  line Ident
}

# (3) Definition Module
lappend non_terminals definitionModule {
  stack
    {line DEFINITION MODULE moduleIdent}
    {line {optx [ blueprintToObey ]} {optx FOR typeToExtend} ;}
    {line {loop nil {nil importList nil}} {loop nil {nil definition nil}}
      END moduleIdent .}
}

# (3.1) Blueprint to Obey
lappend non_terminals blueprintToObey {
  line blueprintIdent
}

# (3.2) Blueprint Identifier
lappend non_terminals blueprintIdent {
  line Ident
}

# (3.3) Type to Extend
lappend non_terminals typeToExtend {
  line Ident
}

# (4) Blueprint
lappend non_terminals blueprint {
  stack
    {line
      BLUEPRINT blueprintIdent}
    {line
      {optx [ blueprintToRefine ]} {optx FOR blueprintForTypeToExtend} ;}
    {line
      {optx REFERENTIAL identList ;} moduleTypeRequirement ;}
    {line
      {loop nil {nil constOrTypeOrProcRequirement ;}}
      END blueprintIdent .}
}

# (4.1) Blueprint to Refine
lappend non_terminals blueprintToRefine {
  line blueprintIdent
}

# (4.2) Blueprint for Type to Extend
lappend non_terminals blueprintForTypeToExtend {
  line blueprintIdent
}

# (5) Module Type Requirement
lappend non_terminals moduleTypeRequirement {
  line MODULE TYPE {
    or
      {line = {or OPAQUE RECORD *} {optx {loop {line := protoLiteral} |}}}
      {line : /NIL}
    }
}

# (5.1) Proto Literal
lappend non_terminals protoLiteral {
  or simpleProtoLiteral structuredProtoLiteral 
}

# (5.2) Simple Proto Literal
lappend non_terminals simpleProtoLiteral {
  line Ident 
}

# (6) Structured Proto Literal
lappend non_terminals structuredProtoLiteral {
  line LBRACE {
    or
      {line ARGLIST {optx itemCount} OF
        {or simpleProtoLiteral {line LBRACE protoLiteralList RBRACE}}}
      protoLiteralList
  } RBRACE
}

# (6.1) Proto Literal List
lappend non_terminals protoLiteralList {
  line identList 
}

# (6.2) Item Count
lappend non_terminals itemCount {
  line constIdent
}

# (6.3) Item Count
lappend non_terminals constIdent {
  line Ident
}

# (7) Constant Or Type Or Procedure Requirement
lappend non_terminals constOrTypeOrProcRequirement {
  line {optx {optx NOT} boolConstIdent ->}
  {or constRequirement procedureRequirement {line TYPE = procedureType}}
}

# (8) Constant Requirement
lappend non_terminals constRequirement {
  line CONST {
    or
      {line [ constBindableProperty ]
        {or simpleConstRequirement {line : /NIL} nil}}
      simpleConstRequirement
    }
}

# (8.1) Constant-Bindable Property
lappend non_terminals constBindableProperty {
  or DESCENDING ConstBindableIdent
}

# (9) Simple Constant Requirement
lappend non_terminals simpleConstRequirement {
  line Ident {
    or
      {line = constExpression}
      {line : predefOrRefTypeIdent}
    }
}

# (9.1) Constant Expression
lappend non_terminals constExpression {
  line expression
}

# (9.2) Predefined Or Referential Type Identifier
lappend non_terminals predefOrRefTypeIdent {
  line Ident
}

# (10) Procedure Requirement
lappend non_terminals procedureRequirement {
  line PROCEDURE {
    or
      {line [ procBindableEntity ] {or procedureSignature {line : /NIL} nil}}
      {line {optx restrictedExport} procedureSignature}
    }
}

# (10.1) Restricted Export
lappend non_terminals restrictedExport {
  line *
}

# (10.2) Procedure-Bindable Entity
lappend non_terminals procBindableEntity {
  or + - * / = < > :: := .. IN DIV MOD FOR ProcBindableIdent
}

# (11) Import List
lappend non_terminals importList {
  line {or libGenDirective importDirective} ;
}

# (12) Library Generation Directive
lappend non_terminals libGenDirective {
  line GENLIB libIdent FROM template FOR templateParamList END
}

# (12.1) Library Identifier
lappend non_terminals libIdent {
  line Ident
}

# (12.2) Template
lappend non_terminals template {
  line Ident
}

# (13) Template Parameter List
lappend non_terminals templateParamList {
  loop {line placeholder = replacement} ;
}

# (13.1) Placeholder
lappend non_terminals placeholder {
  line Ident
}

# (13.2) Replacement
lappend non_terminals replacement {
  line StringLiteral
}

# (14) Import Directive
lappend non_terminals importDirective {
  or
    {line IMPORT {loop {line moduleIdent {optx reExport}} ,}}
    {line FROM moduleOrEnumIdent IMPORT {
      or
        {line identList}
        {line *}
    }
  }
}

# (14.1) Re-Export
lappend non_terminals reExport {
  line +
}

# (14.2) Re-Export
lappend non_terminals moduleOrEnumIdent {
  line Ident
}

# (15) Block
lappend non_terminals block {
  line {loop nil {nil declaration nil}}
  {optx BEGIN statementSequence} END
}

# (16) Definition
lappend non_terminals definition {
  line {
    or
      {line CONST {loop {line constDefinition ;} {}} }
      {line TYPE {loop {line Ident = typeOrOpaqueType ;} {}} }
      {line VAR {loop {line variableDeclaration ;} {}} }
      {line procedureHeader ;}
  }
}

# (16.1) Type Or Opaque Type
lappend non_terminals typeOrOpaqueType {
  or type OPAQUE
}

# (17) Declaration
lappend non_terminals declaration {
  line {
    or
      {line CONST {loop {line Ident = constExpression ;} {}} }
      {line TYPE {loop {line Ident = type ;} {}} }
      {line VAR {loop {line variableDeclaration ;} {}} }
      {line procedureHeader ; block Ident ;}
  }
}

# (18) Const Definition
lappend non_terminals constDefinition {
  line {optx [ constBindableProperty ]} Ident = constExpression
}

# (19) Variable Declaration
lappend non_terminals variableDeclaration {
  line identList : {optx range OF} typeIdent
}


# (20) Type
lappend non_terminals type {
  line {
    or
      {line {optx {or ALIAS SET range} OF} typeIdent}
      {line enumType}
      {line arrayType}
      {line recordType}
      {line pointerType}
      {line procedureType}
  }
}

# (20.1) Type Identifier
lappend non_terminals typeIdent {
  line qualident
}

# (21) Range
lappend non_terminals range {
  line [ {opt >} constExpression .. {opt <} constExpression ]
}

# (22) Enumeration Type
lappend non_terminals enumType {
  line ( {optx + enumBaseType ,} identList )
}

# (22.1) Enumeration Base Type
lappend non_terminals enumBaseType {
  line enumTypeIdent
}

# (22.2) Enumeration Type Identifier
lappend non_terminals enumTypeIdent {
  line typeIdent
}

# (23) Array Type
lappend non_terminals arrayType {
  line ARRAY {loop componentCount ,} OF typeIdent
}

# (23.1) Component Count
lappend non_terminals componentCount {
  line constExpression
}

# (24) Record Type
lappend non_terminals recordType {
  line RECORD
    {or
      {line {loop fieldList ;} {opt indeterminateField}}
      {line ( recBaseType ) {loop fieldList ;}}
    }
  END
}

# (24.1) Record Base Type
lappend non_terminals recBaseType {
  line recTypeIdent
}

# (24.2) Record Type Identifier
lappend non_terminals recTypeIdent {
  line typeIdent
}

# (25) Field List
lappend non_terminals fieldList {
  line {optx restrictedExport} variableDeclaration
}

# (26) Indeterminate Field Declaration
lappend non_terminals indeterminateField {
  line INDETERMINATE Ident : ARRAY discriminantFieldIdent OF typeIdent
}

# (26.1) Discriminant Field Identifier
lappend non_terminals discriminantFieldIdent {
  line Ident
}

# (27) Pointer Type
lappend non_terminals pointerType {
  line POINTER TO {optx CONST} typeIdent
}

# (28) Procedure Type
lappend non_terminals procedureType {
  line PROCEDURE {optx ( formalTypeList )} {optx : returnedType}
}

# (28.1) Returned Type
lappend non_terminals returnedType {
  line typeIdent
}

# (29) Formal Type List
lappend non_terminals formalTypeList {
  loop {or attributedFormalType variadicFormalType} ,
}

# (30) Attributed Formal Type
lappend non_terminals attributedFormalType {
  line {or {} CONST VAR} simpleFormalType
}

# (31) Simple Formal Type
lappend non_terminals simpleFormalType {
  line {optx /CAST} {optx ARRAY OF} typeIdent
}

# (32) Variadic Formal Type
lappend non_terminals variadicFormalType {
  stack
    {line ARGLIST {optx numberOfArgumentsToPass} OF}
    {line
      {or attributedFormalType
        {line LEFT_BRACE {loop attributedFormalType ,} RIGHT_BRACE}}
         {optx {line | variadicTerminator}}}
}

# (33) Procedure Header
lappend non_terminals procedureHeader {
  line PROCEDURE {or {line [ procBindableEntity ]} restrictedExport nil}
  procedureSignature
}

# (34) Procedure Signature
lappend non_terminals procedureSignature {
  line Ident {optx ( formalParamList )} {optx : returnedType}
}

# (35) Formal Parameter List
lappend non_terminals formalParamList {
  loop {or simpleFormalParams variadicFormalParams} ;
}

# (36) Simple Formal Parameters
lappend non_terminals simpleFormalParams {
  line {or {} CONST VAR} identList : simpleFormalType
}

# (37) Variadic Formal Parameters
lappend non_terminals variadicFormalParams {
  stack
    {line ARGLIST {optx numberOfArgumentsToPass} OF}
    {line
      {or simpleFormalType
        {line LEFT_BRACE {loop simpleFormalParams ;} RIGHT_BRACE}}
         {optx {line | variadicTerminator}}}
}

# (37.1) Number Of Arguments To Pass
lappend non_terminals numberOfArgumentsToPass {
  line constExpression
}

# (37.2) Variadic Terminator
lappend non_terminals variadicTerminator {
  line constExpression
}

# (38) Statement
lappend non_terminals statement {
  line {
    or
      assignmentOrProcedureCall
      ifStatement
      caseStatement
      whileStatement
      repeatStatement
      loopStatement
      forStatement
      {line RETURN {optx expression}}
      EXIT
  }
}

# (39) Statement Sequence
lappend non_terminals statementSequence {
  loop statement ;
}

# (40) Assignment Or Procedure Call
lappend non_terminals assignmentOrProcedureCall {
  line designator {
    or
      {}
      {line := expression}
      {line incOrDecSuffix}
      {line actualParameters}
  }
}

# (40.1) Increment Or Decrement Suffix
lappend non_terminals incOrDecSuffix {
  or
    {line ++}
    {line --}
}

# (41) IF Statement
lappend non_terminals ifStatement {
  stack
    {line IF expression THEN statementSequence}
    {optx {loop {line ELSIF expression THEN statementSequence} {nil}}}
    {line {optx ELSE statementSequence} END}
}

# (42) CASE Statement
lappend non_terminals caseStatement {
  stack
    {line CASE expression OF}
    {line {loop {line | case} {}} {optx ELSE statementSequence} END}
}

# (43) Case
lappend non_terminals case {
  line {loop caseLabels ,} : statementSequence
}

# (44) Case Labels
lappend non_terminals caseLabels {
  line constExpression {optx .. constExpression}
}

# (45) WHILE Statement
lappend non_terminals whileStatement {
  line WHILE expression DO statementSequence END
}

# (46) REPEAT Statement
lappend non_terminals repeatStatement {
  line REPEAT statementSequence UNTIL expression
}

# (47) LOOP Statement
lappend non_terminals loopStatement {
  line LOOP statementSequence END
}

# (48) FOR Statement
lappend non_terminals forStatement {
  stack
    {line FOR {optx DESCENDING} controlVariable}
    {line IN {or designator {line range OF typeIdent}}}
    {line DO statementSequence END}
}

# (48.1) Control Variable
lappend non_terminals controlVariable {
  line Ident
}

# (49) Designator
lappend non_terminals designator {
  line qualident {optx designatorTail}
}

# (50) Designator Tail
lappend non_terminals designatorTail {
  line {
    loop {
      line {
        or
          {line [ exprListOrSlice ]}
          ^
      }
      {optx {loop {line . Ident} {}}}
    }
  }
}

# (51) Expression List Or Slice
lappend non_terminals exprListOrSlice {
  line expression {
    optx {
      or
        {loop {line , expression} nil}
        {line .. expression}
    }
  }
}

# (52) Expression
lappend non_terminals expression {
  line simpleExpression {optx relOp simpleExpression}
}

# (52.1) Relational Operator
lappend non_terminals relOp {
  or
    = # < <= > >= == IN
}

# (53) Simple Expression
lappend non_terminals simpleExpression {
  line {or {} + -} {loop term addOp}
}

# (53.1) Add Operator
lappend non_terminals addOp {
  line {or + - OR}
}

# (54) Term
lappend non_terminals term {
  loop factorOrNegation mulOp
}

# (54.1) Multiply Operator
lappend non_terminals mulOp {
  line {or * / DIV MOD AND}
}

# (55) factorOrNegation
lappend non_terminals factorOrNegation {
  line {optx NOT} factor
}

# (56) Factor
lappend non_terminals factor {
  line simpleFactor {optx :: typeIdent}
}

# (57) Simple Factor
lappend non_terminals simpleFactor {
  line {
    or
      NumericLiteral
      StringLiteral
      structuredValue
      designatorOrFunctionCall
      {line ( expression )}
    }
}

# (58) Designator Or Function Call
lappend non_terminals designatorOrFunctionCall {
  line designator {optx actualParameters}
}

# (59) Actual Parameters
lappend non_terminals actualParameters {
  line ( {optx expressionList} )
}

# (60) Expression List
lappend non_terminals expressionList {
  loop expression ,
}

# (61) Structured Value
lappend non_terminals structuredValue {
  line LBRACE {loop valueComponent ,} RBRACE
}

# (62) Value Component
lappend non_terminals valueComponent {
  or
    {line constExpression {optx {or BY ..} constExpression}}
    {line runtimeExpression}
}

# (62.1) Runtime Expression
lappend non_terminals runtimeExpression {
  line expression
}

# (63) Qualified Identifier
lappend non_terminals qualident {
  loop Ident .
}

# (64) Identifier List
lappend non_terminals identList {
  loop Ident ,
}

# ---------------------------------------------------------------------------
# Terminal Symbols for Optional Language Extensions
# ---------------------------------------------------------------------------
#

# Architecture Specific Implementation Module Selection

# Replacement for #2
lappend non_terminals langExtn_programModule {
  line MODULE moduleIdent {optx ( arch ) } ;
  {loop nil {nil importList nil}} block moduleIdent .
}

# Architecture
lappend non_terminals langExtn_arch {
  Ident
}

# Register Mapping Facility

# Replacement for #29
lappend non_terminals langExtn_simpleFormalType {
  line {optx /CAST} {optx ARRAY OF} typeIdent {optx regAttribute}
}

# Register Mapping Attribute
lappend non_terminals langExtn_regAttribute {
  line IN REG {or registerNumber registerMnemonic}
}

# Register Number
lappend non_terminals langExtn_registerNumber {
  constExpression
}

# Register Mnemonic
lappend non_terminals langExtn_registerMnemonic {
  qualident
}

# Symbolic Assembly Inline Facility

# Replacement for #36
lappend non_terminals langExtn_statement {
  line {
    or
      assignmentOrProcedureCall
      ifStatement
      caseStatement
      whileStatement
      repeatStatement
      loopStatement
      forStatement
      assemlyBlock
      {line RETURN {optx expression}}
      EXIT
  }
}

# Assembly Block
lappend non_terminals langExtn_assemblyBlock {
  line ASM assemblySourceCode END
}


# ---------------------------------------------------------------------------
# Terminal Symbols
# ---------------------------------------------------------------------------
#
set terminals {}

# (1a) Reserved Words
lappend terminals ReservedWords1 {
  or
    ALIAS AND ARRAY ARGLIST BEGIN BLUEPRINT BY CASE CONST DEFINITION
    DESCENDING DIV DO ELSE ELSIF END EXIT FOR FROM GENLIB IF
    IMPLEMENTATION IMPORT
}

# (1b) Reserved Words
lappend terminals ReservedWords2 {
  or
    IN INDETERMINATE LOOP MOD MODULE NOT OF OPAQUE OR POINTER PROCEDURE
    RECORD REFERENTIAL REPEAT RETURN SET THEN TO TYPE UNTIL VAR WHILE
}

# (2) Identifier
lappend terminals Ident {
  line IdentLeadChar {optx IdentTail}
}

# (2.1) Identifier Lead Character
lappend terminals IdentLeadChar {
  or Letter _ $
}

# (2.2) Identifier Tail
lappend terminals IdentTail {
  loop {or IdentLeadChar Digit} {}
}

# (2.3) Constant-Bindable Identifier
lappend terminals ConstBindableIdent {
  or /NIL /TLIMIT /TSIGNED /TBASE /TPRECISION /TMINEXP /TMAXEXP
}

# (2.4) Procedure-Bindable Identifier
lappend terminals ProcBindableIdent {
  or
    /ABS /NEG /DUP /COPY /COUNT /LENGTH /NEW /RETAIN /RELEASE /CONCAT
    /STORE /RETRIEVE /INSERT /REMOVE /SUBSET /READ /WRITE /WRITEF
    /TMIN /TMAX /SXF /VAL
}

# (3) Numeric Literal
lappend terminals NumericLiteral {
  or
    {line 0 {
      or
        {}
        RealNumberTail
        {line /b Base2DigitSeq }
        {line /x Base16DigitSeq }
        {line /u Base16DigitSeq }
      }}
    {line 1..9 {optx DecimalNumberTail}}
}

# (3.1) Decimal Number Tail
lappend terminals DecimalNumberTail {
  or
   {line {optx WIDE_SINGLE_QUOTE} DigitSeq {optx RealNumberTail}}
   RealNumberTail
}

# Digit Separator
# lappend terminals DigitSep {
#   line SINGLE_QUOTE
# }

# (3.2) Real Number Tail
lappend terminals RealNumberTail {
  line . DigitSeq {optx /e {or {} + -} DigitSeq }
}

# (3.3) Digit Sequence
lappend terminals DigitSeq {
  loop DigitGroup SINGLE_QUOTE
}

# (3.3b) Digit Group
lappend terminals DigitGroup {
  loop Digit {}
}

# (3.4) Base-2 Digit Sequence
lappend terminals Base2DigitSeq {
  loop Base2DigitGroup SINGLE_QUOTE
}

# (3.4b) Base-2 Digit Group
lappend terminals Base2DigitGroup {
  loop Base2Digit {}
}

# (3.5) Base-16 Digit Sequence
lappend terminals Base16DigitSeq {
  loop Base16DigitGroup SINGLE_QUOTE
}

# (3.5b) Base-16 Digit Group
lappend terminals Base16DigitGroup {
  loop Base16Digit {}
}

# (3.6) Digit
lappend terminals Digit {
  or Base2Digit 2 3 4 5 6 7 8 9
}

# (3.7) Base-16 Digit
lappend terminals Base16Digit {
  or Digit /A /B /C /D /E /F
}

# (3.8) Base-2 Digit
lappend terminals Base2Digit {
  or 0 1
}

# (4) String Literal
lappend terminals StringLiteral {
  or SingleQuotedString DoubleQuotedString
}

# (4.1) Single Quoted String
lappend terminals SingleQuotedString {
  line SINGLE_QUOTE
    {optx {loop {or QuotableCharacter DOUBLE_QUOTE} {}}}
  SINGLE_QUOTE
}

# (4.2) Double Quoted String
lappend terminals DoubleQuotedString {
  line DOUBLE_QUOTE
    {optx {loop {or QuotableCharacter SINGLE_QUOTE} {}}}
  DOUBLE_QUOTE
}

# (4.3) Quotable Character
lappend terminals QuotableCharacter {
  or Digit Letter Space NonAlphaNumQuotable EscapedCharacter
}

# (4.4) Letter
lappend terminals Letter {
  or /A..Z /a..z 
}

# (4.5) Space
# CONST Space = CHR(32);

# (4.6) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable {
  or ! # $ % & ( ) * + , - . / : ; < = > ? @ [ ] ^ _ ` LBRACE | RBRACE ~
}

# (4.7) Escaped Character
lappend terminals EscapedCharacter {
  line BACKSLASH {or /n /t BACKSLASH}
}


# ---------------------------------------------------------------------------
# Ignore Symbols
# ---------------------------------------------------------------------------
#
set ignore_symbols {}

# (1) Whitespace
lappend ignore_symbols Whitespace {
  or Space ASCII_TAB
}

# (1.1) ASCII_TAB
# CONST ASCII_TAB = CHR(8);

# (2) Line Comment
lappend ignore_symbols LineComment {
  line // {optx {loop CommentCharacter {}}} EndOfLine
}

# (3) Block Comment
lappend ignore_symbols BlockComment {
  line (* {optx {loop {or CommentCharacter BlockComment EndOfLine} {}}} *)
}

# (3.1) Comment Character
lappend ignore_symbols CommentCharacter {
  or Digit Letter Whitespace NonAlphaNumQuotable
  BACKSLASH SINGLE_QUOTE DOUBLE_QUOTE
}

# (4) End-Of-Line Marker
lappend ignore_symbols EndOfLine {
  or
    {line ASCII_LF}
    {line ASCII_CR {optx ASCII_LF}}
}

# (4.1) ASCII_LF
# CONST ASCII_LF = CHR(10);

# (4.2) ASCII_CR
# CONST ASCII_CR = CHR(13);

# (5) UTF8 BOM
# CONST UTF8_BOM = { 0uEF, 0uBB, 0uBF };

# ---------------------------------------------------------------------------
# Pragma Grammar
# ---------------------------------------------------------------------------
#
set pragmas {}

# (1) Pragma
lappend pragmas pragma {
  line <* pragmaBody *>
}

# (1.1) Pragma
lappend pragmas pragmaBody {
  or
    pragmaMSG
    pragmaIF
    procAttrPragma
    pragmaPTW
    pragmaFORWARD
    pragmaENCODING
    pragmaALIGN
    pragmaPADBITS
    pragmaPURITY
    variableAttrPragma
    pragmaDEPRECATED
    pragmaGENERATED
    pragmaADDR
    pragmaFFI
    pragmaFFIDENT
    implDefinedPragma
}

# (2) Body Of Compile Time Message Pragma
lappend pragmas pragmaMSG {
  line MSG = messageMode :
    {loop compileTimeMsgComponent ,}
}

# (2.1) Message Mode
lappend pragmas messageMode {
  or INFO WARN ERROR FATAL
}

# (3) Compile Time Message Component
lappend pragmas compileTimeMsgComponent {
  line {
    or
      StringLiteral
      constQualident
      {line @ {or ALIGN ENCODING implDefinedPragmaSymbol}}
  }
}

# (3.1) Constant Qualified Identifier
lappend pragmas constQualident {
  line qualident
}

# (3.2) Implementation Defined Pragma Symbol
lappend pragmas implDefinedPragmaSymbol {
  line Ident
}

# (4) Body Of Conditional Compilation Pragma
lappend pragmas pragmaIF {
  or
    {line {or IF ELSIF} inPragmaExpression}
    ELSE
    ENDIF
}

# (5) Body Of Procedure Attribute Pragma
lappend pragmas procAttrPragma {
  or INLINE NOINLINE NORETURN
}

# (6) Body Of Promise-To-Write Pragma
lappend pragmas pragmaPTW {
  line PTW
}

# (7) Body Of Forward Declaration Pragma
lappend pragmas pragmaFORWARD {
  line FORWARD {or {line TYPE identList} procedureHeader}
}

# (8) Body Of Character Encoding Pragma
lappend pragmas pragmaENCODING {
  line ENCODING = {or `ASCII `UTF8} {opt : codePointSampleList}
}

# (9) Code Point Sample List
lappend pragmas codePointSampleList {
  loop {line quotedCharacter = CharCodeLiteral} ,
}

# (9.1) Quoted Character
lappend pragmas quotedCharacter {
  line StringLiteral
}

# (9.2) Character Code Literal
lappend pragmas charCodeLiteral {
  line NumericLiteral
}

# (10) Body Of Memory Alignment Pragma
lappend pragmas pragmaALIGN {
  line ALIGN = inPragmaExpression
}

# (11) Body Of Bit Padding Pragma
lappend pragmas pragmaPADBITS {
  line PADBITS = inPragmaExpression
}

# (12) Body Of Purity Attribute Pragma
lappend pragmas pragmaPURITY {
  line PURITY = inPragmaExpression
}

# (13) Body Of Variable Attribute Pragma
lappend pragmas variableAttrPragma {
  or SINGLEASSIGN LOWLATENCY VOLATILE
}

# (14) Body Of Deprecation Pragma
lappend pragmas pragmaDEPRECATED {
  line DEPRECATED
}

# (15) Body Of Generation Timestamp Pragma
lappend pragmas pragmaGENERATED {
  line GENERATED FROM template , datestamp , timestamp
}

# (15.1) Date Stamp
lappend pragmas datestamp {
  line year - month - day
}

# (15.2) Time Stamp
lappend pragmas timestamp {
  line hours : minutes : seconds + timezone
}

# (15.3) year, month, day, hours, minutes, seconds, timezone
lappend pragmas year_month_day_etc {
  line wholeNumber
}

# (16) Body Of Memory Mapping Pragma
lappend pragmas pragmaADDR {
  line ADDR = inPragmaExpression
}

# (17) Body Of Foreign Function Interface Pragma
lappend pragmas pragmaFFI {
  line FFI = {or `C `Fortran `CLR `JVM }
}

# (18) Body Of Foreign Function Identifier Mapping Pragma
lappend pragmas pragmaFFIDENT {
  line FFIDENT = StringLiteral
}

# (19) Body of Implementation Defined Pragma
lappend pragmas implDefinedPragma {
  line implDefinedPragmaSymbol {optx = inPragmaExpression}
    | messageMode
}

# (20) In-Pragma Expression
lappend pragmas inPragmaExpression {
  line inPragmaSimpleExpr {optx inPragmaRelOp inPragmaSimpleExpr}
}

# (20.1) In-Pragma Relational Operator
lappend pragmas inPragmaRelOp {
  or = # < <= > >=
}

# (21) In-Pragma Simple Expression
lappend pragmas inPragmaSimpleExpr {
  line {or {} + -} {loop inPragmaTerm addOp}
}

# (22) In-Pragma Term
lappend pragmas inPragmaTerm {
  loop inPragmaFactor inPragmaMulOp
}

# (22.1) In-Pragma Multiply Operator
lappend pragmas inPragmaMulOp {
  or * DIV MOD AND
}

# (23) In-Pragma Factor
lappend pragmas inPragmaFactor {
  line {optx NOT} inPragmaSimpleFactor
}

# (24) In-Pragma Simple Factor
lappend pragmas inPragmaSimpleFactor {
  or
    wholeNumber
    constQualident
    inPragmaCompileTimeFunctionCall
    {line ( inPragmaExpression )}
}

# (24.1) Whole Number
lappend pragmas wholeNumber {
  line NumericLiteral
}

# (25) In-Pragma Compile Time Function Call
lappend pragmas inPragmaCompileTimeFunctionCall {
  line qualident ( {loop inPragmaExpression ,} ) 
}


# ---------------------------------------------------------------------------
# Alias Diagrams
# ---------------------------------------------------------------------------
#
set aliases {}

# Alias For Identifier
lappend aliases AliasForIdent {
  line Ident
}

# Alias For Blueprint Identifier
lappend aliases AliasForBlueprintIdent {
  line blueprintIdent
}

# Alias For Qualified Identifier
lappend aliases AliasForQualident {
  line qualident
}

# Alias For Number
lappend aliases AliasForNumericLiteral {
  line NumericLiteral
}

# Alias For Whole Number
lappend aliases AliasForWholeNumber {
  line wholeNumber
}

# Alias For String
lappend aliases AliasForStringLiteral {
  line StringLiteral
}

# Alias For Constant Expression
lappend aliases AliasForConstExpr {
  line constExpression
}

# Alias For Expression
lappend aliases AliasForExpression {
  line expression
}

# Alias For Type Identifier
lappend aliases AliasForTypeIdent {
  line typeIdent
}


# ---------------------------------------------------------------------------
# Legend Diagrams
# ---------------------------------------------------------------------------
#
set legend {}

# Legend -- Reserved Word
lappend legend legendReservedWord {
  line RESERVED
}

# Legend -- Terminal Symbol, Example 1
lappend legend legendTerminal1 {
  line #
}

# Legend -- Terminal Symbol, Example 2
lappend legend legendTerminal2 {
  line Terminal
}

# Legend -- Identifier
lappend legend legendIdentifier {
  line /IDENTIFIER
}

# Legend -- Non-Terminal Symbol
lappend legend legendNonTerminal {
  line nonTerminal
}

# end Modula-2 grammar


#  
# ===========================================================================
# C O D E   S T A R T S   H E R E
# ===========================================================================
#


# ---------------------------------------------------------------------------
# Draw the button box
# ---------------------------------------------------------------------------
#
set bn 0
set b .bb.b$bn
wm title .bb "Modula-2"
button $b -text "Draw All Diagrams" -command {draw_all_graphs}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Draw Non-Terminals" -command {draw_graphs $non_terminals}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Draw Terminals" -command {draw_graphs $terminals}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Draw Pragmas" -command {draw_graphs $pragmas}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Draw Ignore Symbols" -command {draw_graphs $ignore_symbols}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Draw Aliases" -command {draw_graphs $aliases}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Draw Legend" -command {draw_graphs $legend}
pack $b -side top -fill x -expand 0 -pady 0
incr bn
set b .bb.b$bn
button $b -text "Quit" -command {exit}
pack $b -side top -fill x -expand 0 -pady {0 14}


# ---------------------------------------------------------------------------
# L a y o u t - P a r a m e t e r s
# ---------------------------------------------------------------------------
#
set TERMFONT {Helvetica 12 bold}  ;# default terminal font
set NONTERMFONT {Helvetica 12}    ;# default non-terminal font
set TOKENFONT {Monaco 12 bold}    ;# default token font
set STRINGFONT {Courier 12 bold}  ;# default string font
set RADIUS 9                      ;# default turn radius
set HSEP 15                       ;# horizontal separation
set VSEP 7                        ;# vertical separation
set LWIDTH 3                      ;# line width
set DPI 96                        ;# dots per inch for gif


set tagcnt 0 ;# tag counter - don't modify this


# ---------------------------------------------------------------------------
# Draw a right-hand turn around.  Approximately a ")"
# ---------------------------------------------------------------------------
#
proc draw_right_turnback {tag x y0 y1} {
  global RADIUS
  global LWIDTH
  if {$y0 + 2*$RADIUS < $y1} {
    set xr0 [expr {$x-$RADIUS}]
    set xr1 [expr {$x+$RADIUS}]
    .c create arc $xr0 $y0 $xr1 [expr {$y0+2*$RADIUS}] \
          -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
    set yr0 [expr {$y0+$RADIUS}]
    set yr1 [expr {$y1-$RADIUS}]
    if {abs($yr1-$yr0)>$RADIUS*2} {
      set half_y [expr {($yr1+$yr0)/2}]
      .c create line $xr1 $yr0 $xr1 $half_y -width $LWIDTH -tags $tag -arrow last
      .c create line $xr1 $half_y $xr1 $yr1 -width $LWIDTH -tags $tag
    } else {
      .c create line $xr1 $yr0 $xr1 $yr1 -width $LWIDTH -tags $tag
    }
    .c create arc $xr0 [expr {$y1-2*$RADIUS}] $xr1 $y1 \
          -width $LWIDTH -start 0 -extent -90 -tags $tag -style arc
  } else { 
    set r [expr {($y1-$y0)/2.0}]
    set x0 [expr {$x-$r}]
    set x1 [expr {$x+$r}]
    .c create arc $x0 $y0 $x1 $y1 \
          -width $LWIDTH -start 90 -extent -180 -tags $tag -style arc
  }
} ;# end draw_right_turnback


# ---------------------------------------------------------------------------
# Draw a left-hand turn around.  Approximatley a "("
# ---------------------------------------------------------------------------
#
proc draw_left_turnback {tag x y0 y1 dir} {
  global RADIUS
  global LWIDTH
  if {$y0 + 2*$RADIUS < $y1} {
    set xr0 [expr {$x-$RADIUS}]
    set xr1 [expr {$x+$RADIUS}]
    .c create arc $xr0 $y0 $xr1 [expr {$y0+2*$RADIUS}] \
          -width $LWIDTH -start 90 -extent 90 -tags $tag -style arc
    set yr0 [expr {$y0+$RADIUS}]
    set yr1 [expr {$y1-$RADIUS}]
    if {abs($yr1-$yr0)>$RADIUS*3} {
      set half_y [expr {($yr1+$yr0)/2}]
      if {$dir=="down"} {
        .c create line $xr0 $yr0 $xr0 $half_y -width $LWIDTH -tags $tag -arrow last
        .c create line $xr0 $half_y $xr0 $yr1 -width $LWIDTH -tags $tag
      } else {
        .c create line $xr0 $yr1 $xr0 $half_y -width $LWIDTH -tags $tag -arrow last
        .c create line $xr0 $half_y $xr0 $yr0 -width $LWIDTH -tags $tag
      }
    } else {
      .c create line $xr0 $yr0 $xr0 $yr1 -width $LWIDTH -tags $tag
    }
    # .c create line $xr0 $yr0 $xr0 $yr1 -width $LWIDTH -tags $tag
    .c create arc $xr0 [expr {$y1-2*$RADIUS}] $xr1 $y1 \
          -width $LWIDTH -start 180 -extent 90 -tags $tag -style arc
  } else { 
    set r [expr {($y1-$y0)/2.0}]
    set x0 [expr {$x-$r}]
    set x1 [expr {$x+$r}]
    .c create arc $x0 $y0 $x1 $y1 \
          -width $LWIDTH -start 90 -extent 180 -tags $tag -style arc
  }
} ;# end draw_left_turnback


# ---------------------------------------------------------------------------
# Draw a bubble containing $txt. 
# ---------------------------------------------------------------------------
#
proc draw_bubble {txt} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  if {$txt=="nil"} {
    .c create line 0 0 1 0 -width $LWIDTH -tags $tag
    return [list $tag 1 0]
  } elseif {$txt=="bullet"} {
    .c create oval 0 -3 6 3 -width $LWIDTH -tags $tag
    return [list $tag 6 0]
  }
# check for special symbols
  set isQuotedString 0
  if {$txt=="SPACE"} {
    set label "' '"
  } elseif {$txt=="BSLASH" || $txt=="BACKSLASH"} {
    set label "\\"
  } elseif {$txt=="SQUOTE" || $txt=="SINGLE_QUOTE"} {
    set label "\'"
  } elseif {$txt=="WIDE_SINGLE_QUOTE"} {
    set label " \' "
  } elseif {$txt=="DQUOTE" || $txt=="DOUBLE_QUOTE"} {
    set label "\""
  } elseif {$txt=="LBRACE" || $txt=="LEFT_BRACE"} {
    set label "\{"
  } elseif {$txt=="RBRACE" || $txt=="RIGHT_BRACE"} {
    set label "\}"
  } elseif {$txt=="`ASCII"} {
    set label "\"ASCII\""
    set isQuotedString 1
  } elseif {$txt=="`UTF8"} {
    set label "\"UTF8\""
    set isQuotedString 1
  } elseif {$txt=="`C"} {
    set label "\"C\""
    set isQuotedString 1
  } elseif {$txt=="`Fortran"} {
    set label "\"Fortran\""
    set isQuotedString 1
  } elseif {$txt=="`CLR"} {
    set label "\"CLR\""
    set isQuotedString 1
  } elseif {$txt=="`JVM"} {
    set label "\"JVM\""
    set isQuotedString 1
  } else {
    set label $txt
  }
# initialise symbol flags
  set isNonTerminal 0
  set isTerminal 0
  set isToken 0
# determine type of symbol
  if {[regexp {^[A-Z]} $label]} {
    set isTerminal 1
    set font $::TERMFONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^[a-z]} $label]} {
    set isNonTerminal 1
    set font $::NONTERMFONT
    set label "  $label  "
    set baseline 0
 } else {
    set isToken 1
    if {$isQuotedString} {
      set font $::STRINGFONT
    } else {
      set font $::TOKENFONT
    }
    set baseline [expr {$LWIDTH/2}]
    if {[regexp {^/[a-zA-Z]} $label]} {
      set label [string range $label 1 end]
    }
  }
  set id1 [.c create text 0 $baseline -anchor c -text $label -font $font -tags $tag]
# lassign [.c bbox $id1] x0 y0 x1 y1 # to do: replace all foreach with lassign
  foreach {x0 y0 x1 y1} [.c bbox $id1] break
# move parentheses, brackets, braces and underscore up by one pixel
  if {$label in {( ) [ ] \{ \} _ }} { .c move $id1 0 -1 }
# move the asterisk down by one pixel
  if {$label=="*"} { .c move $id1 0 1 }
  set h [expr {$y1-$y0+$LWIDTH}]
  set rad [expr {($h+1)/2}]
  if {$isNonTerminal} {
    set top [expr {$y0-$LWIDTH}]
    set btm [expr {$y1+$LWIDTH}]
    set left [expr {$x0-$LWIDTH}]
    set right [expr {$x1+$LWIDTH}]
    .c create rect $left $top $right $btm -width $LWIDTH -tags $tag
  } else {
    set top [expr {$y0-$LWIDTH}]
    set btm [expr {$y1+1}]
    set left [expr {$x0+$LWIDTH}]
    set right [expr {$x1-$LWIDTH}]
    if {$left>$right} {
      set left [expr {($x0+$x1)/2}]
      set right $left
    }
    .c create arc [expr {$left-$rad}] $top [expr {$left+$rad}] $btm \
         -width $LWIDTH -start 90 -extent 180 -style arc -tags $tag
    .c create arc [expr {$right-$rad}] $top [expr {$right+$rad}] $btm \
         -width $LWIDTH -start -90 -extent 180 -style arc -tags $tag
    if {$left<$right} {
      .c create line $left $top $right $top -width $LWIDTH -tags $tag
      .c create line $left $btm $right $btm -width $LWIDTH -tags $tag
    }
  }
  foreach {x0 y0 x1 y1} [.c bbox $tag] break
  set width [expr {$x1-$x0}]
  .c move $tag [expr {-$x0}] 0

  # Entry is always 0 0
  # Return:  TAG EXIT_X EXIT_Y
  #
  return [list $tag $width 0]
} ;# end draw_bubble


# ---------------------------------------------------------------------------
# Draw a sequence of terms from left to write.
# ---------------------------------------------------------------------------
# Each element of $lx describes a single term.
#
proc draw_line {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt

  set sep $::HSEP
  set exx 0
  set exy 0
  foreach term $lx {
    set m [draw_diagram $term]
    foreach {t texx texy} $m break
    if {$exx>0} {
      set xn [expr {$exx+$sep}]
      .c move $t $xn $exy
      .c create line [expr {$exx-1}] $exy $xn $exy \
         -tags $tag -width $LWIDTH -arrow last
      set exx [expr {$xn+$texx}]
    } else {
      set exx $texx
    }
    set exy $texy
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  if {$exx==0} {	
    set exx [expr {$sep*2}]
    .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag -arrow last
    .c create line $sep 0 $exx 0 -width $LWIDTH -tags $tag
    set exx $sep
  }
  return [list $tag $exx $exy]
} ;# end draw_line


# ---------------------------------------------------------------------------
# Draw a sequence of terms from right to left.
# ---------------------------------------------------------------------------
#
proc draw_backwards_line {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt

  set sep $::HSEP
  set exx 0
  set exy 0
  set lb {}
  set n [llength $lx]
  for {set i [expr {$n-1}]} {$i>=0} {incr i -1} {
    lappend lb [lindex $lx $i]
  }
  foreach term $lb {
    set m [draw_diagram $term]
    foreach {t texx texy} $m break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    set w [expr {$tx1-$tx0}]
    if {$exx>0} {
      set xn [expr {$exx+$sep}]
      .c move $t $xn 0
      .c create line $exx $exy $xn $exy -tags $tag -width $LWIDTH -arrow first
      set exx [expr {$xn+$texx}]
    } else {
      set exx $texx
    }
    set exy $texy
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  if {$exx==0} {
    .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag
    set exx $sep
  }
  return [list $tag $exx $exy]
} ;# end draw_backwards_line


# ---------------------------------------------------------------------------
# Draw a sequence of terms from top to bottom.
# ---------------------------------------------------------------------------
#
proc draw_stack {indent lx} {
  global tagcnt RADIUS VSEP LWIDTH
  incr tagcnt
  set tag x$tagcnt

  set sep [expr {$VSEP*2}]
  set btm 0
  set n [llength $lx]
  set i 0
  set next_bypass_y 0

  foreach term $lx {
    set bypass_y $next_bypass_y
    if {$i>0 && $i<$n && [llength $term]>1 &&
        ([lindex $term 0]=="opt" || [lindex $term 0]=="optx")} {
      set bypass 1
      set term "line [lrange $term 1 end]"
    } else {
      set bypass 0
      set next_bypass_y 0
    }
    set m [draw_diagram $term]
    foreach {t exx exy} $m break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    if {$i==0} {
      set btm $ty1
      set exit_y $exy
      set exit_x $exx
    } else {
      set enter_y [expr {$btm - $ty0 + $sep*2 + 2}]
      if {$bypass} {set next_bypass_y [expr {$enter_y - $RADIUS}]}
      set enter_x [expr {$sep + $indent}]
      set back_y [expr {$btm + $sep + 1}]
      if {$bypass_y>0} {
         set mid_y [expr {($bypass_y+$RADIUS+$back_y)/2}]
         .c create line $bypass_x $bypass_y $bypass_x $mid_y \
            -width $LWIDTH -tags $tag -arrow last
         .c create line $bypass_x $mid_y $bypass_x [expr {$back_y+$RADIUS}] \
             -tags $tag -width $LWIDTH
      }
      .c move $t $enter_x $enter_y
      set e2 [expr {$exit_x + $sep}]
      .c create line $exit_x $exit_y $e2 $exit_y \
            -width $LWIDTH -tags $tag
      draw_right_turnback $tag $e2 $exit_y $back_y
      set e3 [expr {$enter_x-$sep}]
      set bypass_x [expr {$e3-$RADIUS}]
      set emid [expr {($e2+$e3)/2}]
      .c create line $e2 $back_y $emid $back_y \
                 -width $LWIDTH -tags $tag -arrow last
      .c create line $emid $back_y $e3 $back_y \
                 -width $LWIDTH -tags $tag
      set r2 [expr {($enter_y - $back_y)/2.0}]
      draw_left_turnback $tag $e3 $back_y $enter_y down
      .c create line $e3 $enter_y $enter_x $enter_y \
                 -arrow last -width $LWIDTH -tags $tag
      set exit_x [expr {$enter_x + $exx}]
      set exit_y [expr {$enter_y + $exy}]
    }
    .c addtag $tag withtag $t
    .c dtag $t $t
    set btm [lindex [.c bbox $tag] 3]
    incr i
  }
  if {$bypass} {
    set fwd_y [expr {$btm + $sep + 1}]
    set mid_y [expr {($next_bypass_y+$RADIUS+$fwd_y)/2}]
    set descender_x [expr {$exit_x+$RADIUS}]
    .c create line $bypass_x $next_bypass_y $bypass_x $mid_y \
        -width $LWIDTH -tags $tag -arrow last
    .c create line $bypass_x $mid_y $bypass_x [expr {$fwd_y-$RADIUS}] \
        -tags $tag -width $LWIDTH
    .c create arc $bypass_x [expr {$fwd_y-2*$RADIUS}] \
                  [expr {$bypass_x+2*$RADIUS}] $fwd_y \
        -width $LWIDTH -start 180 -extent 90 -tags $tag -style arc
    .c create arc [expr {$exit_x-$RADIUS}] $exit_y \
                  $descender_x [expr {$exit_y+2*$RADIUS}] \
        -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
    .c create arc $descender_x [expr {$fwd_y-2*$RADIUS}] \
                  [expr {$descender_x+2*$RADIUS}] $fwd_y \
        -width $LWIDTH -start 180 -extent 90 -tags $tag -style arc
    set exit_x [expr {$exit_x+2*$RADIUS}]
    set half_x [expr {($exit_x+$indent)/2}]
    .c create line [expr {$bypass_x+$RADIUS}] $fwd_y $half_x $fwd_y \
        -width $LWIDTH -tags $tag -arrow last
    .c create line $half_x $fwd_y $exit_x $fwd_y \
        -width $LWIDTH -tags $tag
    .c create line $descender_x [expr {$exit_y+$RADIUS}] \
                   $descender_x [expr {$fwd_y-$RADIUS}] \
        -width $LWIDTH -tags $tag -arrow last
    set exit_y $fwd_y
  }
  set width [lindex [.c bbox $tag] 2]
  return [list $tag $exit_x $exit_y]
} ;# end draw_stack


# ---------------------------------------------------------------------------
# Draw a loop
# ---------------------------------------------------------------------------
#
proc draw_loop {forward back} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::HSEP
  set vsep $::VSEP
  if {$back in {. , ; |}} {
    set vsep 0
  } elseif {$back=="SINGLE_QUOTE"} {
    set vsep 0
  } elseif {$back=="nil"} {
    set vsep [expr {$vsep/2}]
  }

  foreach {ft fexx fexy} [draw_diagram $forward] break
  foreach {fx0 fy0 fx1 fy1} [.c bbox $ft] break
  set fw [expr {$fx1-$fx0}]
  foreach {bt bexx bexy} [draw_backwards_line $back] break
  foreach {bx0 by0 bx1 by1} [.c bbox $bt] break
  set bw [expr {$bx1-$bx0}]
  set dy [expr {$fy1 - $by0 + $vsep}]
  .c move $bt 0 $dy
  set biny $dy
  set bexy [expr {$dy+$bexy}]
  set by0 [expr {$dy+$by0}]
  set by1 [expr {$dy+$by1}]

  if {$fw>$bw} {
    if {$fexx<$fw && $fexx>=$bw} {
      set dx [expr {($fexx-$bw)/2}]
      .c move $bt $dx 0
      set bexx [expr {$dx+$bexx}]
      .c create line 0 $biny $dx $biny -width $LWIDTH -tags $bt
      .c create line $bexx $bexy $fexx $bexy -width $LWIDTH -tags $bt -arrow first
      set mxx $fexx
    } else {
      set dx [expr {($fw-$bw)/2}]
      .c move $bt $dx 0
      set bexx [expr {$dx+$bexx}]
      .c create line 0 $biny $dx $biny -width $LWIDTH -tags $bt
      .c create line $bexx $bexy $fx1 $bexy -width $LWIDTH -tags $bt -arrow first
      set mxx $fexx
    }
  } elseif {$bw>$fw} {
    set dx [expr {($bw-$fw)/2}]
    .c move $ft $dx 0
    set fexx [expr {$dx+$fexx}]
    .c create line 0 0 $dx $fexy -width $LWIDTH -tags $ft -arrow last
    .c create line $fexx $fexy $bx1 $fexy -width $LWIDTH -tags $ft
    set mxx $bexx
  }
  .c addtag $tag withtag $bt
  .c addtag $tag withtag $ft
  .c dtag $bt $bt
  .c dtag $ft $ft
  .c move $tag $sep 0
  set mxx [expr {$mxx+$sep}]
  .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag
  draw_left_turnback $tag $sep 0 $biny up
  draw_right_turnback $tag $mxx $fexy $bexy
  foreach {x0 y0 x1 y1} [.c bbox $tag] break
  set exit_x [expr {$mxx+$::RADIUS}]
  .c create line $mxx $fexy $exit_x $fexy -width $LWIDTH -tags $tag
  return [list $tag $exit_x $fexy]
} ;# end draw_loop


# ---------------------------------------------------------------------------
# Draw a top-loop
# ---------------------------------------------------------------------------
#
proc draw_toploop {forward back} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::VSEP
  set vsep [expr {$sep/2}]

  foreach {ft fexx fexy} [draw_diagram $forward] break
  foreach {fx0 fy0 fx1 fy1} [.c bbox $ft] break
  set fw [expr {$fx1-$fx0}]
  foreach {bt bexx bexy} [draw_backwards_line $back] break
  foreach {bx0 by0 bx1 by1} [.c bbox $bt] break
  set bw [expr {$bx1-$bx0}]
  set dy [expr {-($by1 - $fy0 + $vsep)}]
  .c move $bt 0 $dy
  set biny $dy
  set bexy [expr {$dy+$bexy}]
  set by0 [expr {$dy+$by0}]
  set by1 [expr {$dy+$by1}]

  if {$fw>$bw} {
    set dx [expr {($fw-$bw)/2}]
    .c move $bt $dx 0
    set bexx [expr {$dx+$bexx}]
    .c create line 0 $biny $dx $biny -width $LWIDTH -tags $bt
    .c create line $bexx $bexy $fx1 $bexy -width $LWIDTH -tags $bt -arrow first
    set mxx $fexx
  } elseif {$bw>$fw} {
    set dx [expr {($bw-$fw)/2}]
    .c move $ft $dx 0
    set fexx [expr {$dx+$fexx}]
    .c create line 0 0 $dx $fexy -width $LWIDTH -tags $ft
    .c create line $fexx $fexy $bx1 $fexy -width $LWIDTH -tags $ft
    set mxx $bexx
  }
  .c addtag $tag withtag $bt
  .c addtag $tag withtag $ft
  .c dtag $bt $bt
  .c dtag $ft $ft
  .c move $tag $sep 0
  set mxx [expr {$mxx+$sep}]
  .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag
  draw_left_turnback $tag $sep 0 $biny down
  draw_right_turnback $tag $mxx $fexy $bexy
  foreach {x0 y0 x1 y1} [.c bbox $tag] break
  .c create line $mxx $fexy $x1 $fexy -width $LWIDTH -tags $tag
  return [list $tag $x1 $fexy]
} ;# end draw_toploop


# ---------------------------------------------------------------------------
# Draw alternative branches
# ---------------------------------------------------------------------------
#
proc draw_or {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::VSEP
  set vsep [expr {$sep/2}]
  set n [llength $lx]
  set i 0
  set mxw 0
  foreach term $lx {
    set m($i) [set mx [draw_diagram $term]]
    set tx [lindex $mx 0]
    foreach {x0 y0 x1 y1} [.c bbox $tx] break
    set w [expr {$x1-$x0}]
    if {$i>0} {set w [expr {$w+20+2*$LWIDTH-1}]}  ;# extra space for arrowheads
    if {$w>$mxw} {set mxw $w}
    incr i
  }

  set x0 0                        ;# entry x
  set x1 $sep                     ;# decender 
  set x2 [expr {$sep*2}]          ;# start of choice
  set xc [expr {$mxw/2}]          ;# center point
  set x3 [expr {$mxw+$x2}]        ;# end of choice
  set x4 [expr {$x3+$sep}]        ;# accender
  set x5 [expr {$x4+$sep}]        ;# exit x

  for {set i 0} {$i<$n} {incr i} {
    foreach {t texx texy} $m($i) break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    set w [expr {$tx1-$tx0}]
    set dx [expr {($mxw-$w)/2 + $x2}]
    if {$w>10 && $dx>$x2+10} {set dx [expr {$x2+10}]}
    .c move $t $dx 0
    set texx [expr {$texx+$dx}]
    set m($i) [list $t $texx $texy]
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    if {$i==0} {
      if {$dx>$x2} {set ax last} {set ax none}
      .c create line 0 0 $dx 0 -width $LWIDTH -tags $tag -arrow $ax
      .c create line $texx $texy [expr {$x5+1}] $texy -width $LWIDTH -tags $tag
      set exy $texy
      .c create arc -$sep 0 $sep [expr {$sep*2}] \
         -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
      set btm $ty1
    } else {
      set dy [expr {$btm - $ty0 + $vsep}]
      if {$dy<2*$sep} {set dy [expr {2*$sep}]}
      .c move $t 0 $dy
      set texy [expr {$texy+$dy}]
      if {$dx>$x2} {
        .c create line $x2 $dy $dx $dy -width $LWIDTH -tags $tag -arrow last
        if {$dx<$xc-2} {set ax last} {set ax none}
        .c create line $texx $texy $x3 $texy -width $LWIDTH -tags $tag -arrow $ax
      }
      set y1 [expr {$dy-2*$sep}]
      .c create arc $x1 $y1 [expr {$x1+2*$sep}] $dy \
          -width $LWIDTH -start 180 -extent 90 -style arc -tags $tag
      set y2 [expr {$texy-2*$sep}]
      .c create arc [expr {$x3-$sep}] $y2 $x4 $texy \
          -width $LWIDTH -start 270 -extent 90 -style arc -tags $tag
      if {$i==$n-1} {
        .c create arc $x4 $exy [expr {$x4+2*$sep}] [expr {$exy+2*$sep}] \
           -width $LWIDTH -start 180 -extent -90 -tags $tag -style arc
        .c create line $x1 [expr {$dy-$sep}] $x1 $sep -width $LWIDTH -tags $tag
        .c create line $x4 [expr {$texy-$sep}] $x4 [expr {$exy+$sep}] \
               -width $LWIDTH -tags $tag
      }
      set btm [expr {$ty1+$dy}]
    }
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  return [list $tag $x5 $exy]   
} ;# end draw_or


# ---------------------------------------------------------------------------
# Draw a tail-branch
# ---------------------------------------------------------------------------
#
proc draw_tail_branch {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::VSEP
  set vsep [expr {$sep/2}]
  set n [llength $lx]
  set i 0
  foreach term $lx {
    set m($i) [set mx [draw_diagram $term]]
    incr i
  }

  set x0 0                        ;# entry x
  set x1 $sep                     ;# decender 
  set x2 [expr {$sep*2}]          ;# start of choice

  for {set i 0} {$i<$n} {incr i} {
    foreach {t texx texy} $m($i) break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    set dx [expr {$x2+10}]
    .c move $t $dx 0
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    if {$i==0} {
      .c create line 0 0 $dx 0 -width $LWIDTH -tags $tag -arrow last
      .c create arc -$sep 0 $sep [expr {$sep*2}] \
         -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
      set btm $ty1
    } else {
      set dy [expr {$btm - $ty0 + $vsep}]
      if {$dy<2*$sep} {set dy [expr {2*$sep}]}
      .c move $t 0 $dy
      if {$dx>$x2} {
        .c create line $x2 $dy $dx $dy -width $LWIDTH -tags $tag -arrow last
      }
      set y1 [expr {$dy-2*$sep}]
      .c create arc $x1 $y1 [expr {$x1+2*$sep}] $dy \
          -width $LWIDTH -start 180 -extent 90 -style arc -tags $tag
      if {$i==$n-1} {
        .c create line $x1 [expr {$dy-$sep}] $x1 $sep -width $LWIDTH -tags $tag
      }
      set btm [expr {$ty1+$dy}]
    }
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  return [list $tag 0 0]
} ;# end draw_tail_branch


# ---------------------------------------------------------------------------
# Draw a single diagram body
# ---------------------------------------------------------------------------
#
proc draw_diagram {spec} {
  set n [llength $spec]
  if {$n==1} {
    return [draw_bubble $spec]
  }
  if {$n==0} {
    return [draw_bubble nil]
  }
  set cmd [lindex $spec 0]
  if {$cmd=="line"} {
    return [draw_line [lrange $spec 1 end]]
  }
  if {$cmd=="stack"} {
    return [draw_stack 0 [lrange $spec 1 end]]
  }
  if {$cmd=="indentstack"} {
    return [draw_stack $::HSEP [lrange $spec 1 end]]
  }
  if {$cmd=="loop"} {
    return [draw_loop [lindex $spec 1] [lindex $spec 2]]
  }
  if {$cmd=="toploop"} {
    return [draw_toploop [lindex $spec 1] [lindex $spec 2]]
  }
  if {$cmd=="or"} {
    return [draw_or [lrange $spec 1 end]]
  }
  if {$cmd=="opt"} {
    set args [lrange $spec 1 end]
    if {[llength $args]==1} {
      return [draw_or [list nil [lindex $args 0]]]
    } else {
      return [draw_or [list nil "line $args"]]
    }
  }
  if {$cmd=="optx"} {
    set args [lrange $spec 1 end]
    if {[llength $args]==1} {
      return [draw_or [list [lindex $args 0] nil]]
    } else {
      return [draw_or [list "line $args" nil]]
    }
  }
  if {$cmd=="tailbranch"} {
    # return [draw_tail_branch [lrange $spec 1 end]]
    return [draw_or [lrange $spec 1 end]]
  }
  error "unknown operator: $cmd"
} ;# end draw_diagram


# ---------------------------------------------------------------------------
# Draw a single production
# ---------------------------------------------------------------------------
#
proc draw_graph {name spec {do_xv 1}} {
  .c delete all
  wm deiconify .
  wm title . $name
  draw_diagram "line bullet [list $spec] bullet"
  foreach {x0 y0 x1 y1} [.c bbox all] break
  .c move all [expr {2-$x0}] [expr {2-$y0}]
  foreach {x0 y0 x1 y1} [.c bbox all] break
  .c config -width $x1 -height $y1
  update
  .c postscript -file $name.ps -width [expr {$x1+2}] -height [expr {$y1+2}]
#
#  uncomment to enable GIF output (this may not work on all systems) ...
#
#  global DPI
#  exec convert -density ${DPI}x$DPI -antialias $name.ps $name.gif
#  if {$do_xv} {
#    exec xv $name.gif &
#  }
#
} ;# end draw_graph


# ---------------------------------------------------------------------------
#  Draw group of productions
# ---------------------------------------------------------------------------
#
proc draw_graphs {group} {
  set f [open all.html w]
  foreach {name graph} $group {
    if {[regexp {^X-} $name]} continue
    puts $f "<h3>$name:</h3>"
    puts $f "<img src=\"$name.gif\">"
    draw_graph $name $graph 0
    set img($name) 1
    set children($name) {}
    set parents($name) {}
  }
  close $f
  set order {}
  foreach {name graph} $group {
    lappend order $name
    unset -nocomplain v
    walk_graph_extract_names $group v
    unset -nocomplain v($name)
    foreach x [array names v] {
      if {![info exists img($x)]} continue
      lappend children($name) $x
      lappend parents($x) $name
    }
  }
  set f [open syntax_linkage.tcl w]
  foreach name [lsort [array names img]] {
    set cx [lsort $children($name)]
    set px [lsort $parents($name)]
    puts $f [list set syntax_linkage($name) [list $cx $px]]
  }
  puts $f [list set syntax_order $order]
  close $f
  wm withdraw .
} ;# end draw_graphs


# ---------------------------------------------------------------------------
#  Draw all productions
# ---------------------------------------------------------------------------
#
proc draw_all_graphs {} {
  global non_terminals
  global terminals
  global pragmas
  global ignore_symbols
  global aliases
  global legend
  draw_graphs $non_terminals
  draw_graphs $terminals
  draw_graphs $pragmas
  draw_graphs $ignore_symbols
  draw_graphs $aliases
  draw_graphs $legend
} ;# end draw_all_graphs


# ---------------------------------------------------------------------------
# Obtain the names of all productions
# ---------------------------------------------------------------------------
#
proc walk_graph_extract_names {graph varname} {
  upvar 1 $varname v
  foreach x $graph {
    set n [llength $x]
    if {$n>1} {
      walk_graph_extract_names $x v
    } elseif {[regexp {^[a-z]} $x]} {
      set v($x) 1
    }
  }
} ;# end walk_graph_extract_names

#
# END OF FILE