#!/usr/bin/wish
#
# Syntax diagram generator for Modula-2 (R10), status Mar 1, 2015
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
#   Symbols are rendered according to their category:
#   (1) reserved words, names in all uppercase letters
#   (2) reserved identifier, names in all uppercase letters preceded by /
#   (3) other terminals, mixed case names with a leading uppercase letter
#   (4) non-terminals, mixed case names with a leading lowercase letter
#   (5) single letter tokens, a single letter or a range eg. a..z / A..Z
#   (6) special symbol tokens, any other characters or character sequences
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
#   RES_WORD_FONT - font/size/style used to render reserved words
#   RES_IDENT_FONT - font/size/style used to render reserved identifiers
#   TERM_FONT - font/size/style used to render any other terminals
#   NON_TERM_FONT - font/size/style used to render non-terminals
#   TOKEN_FONT - font/size/style used to render tokens
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
  {loop nil {nil importList ;}} block moduleIdent .
}

# (2.1) Module Identifier
lappend non_terminals moduleIdent {
  line Ident
}

# (2.2) Import List
lappend non_terminals importList {
  or libGenDirective importDirective
}

# (3) Definition Module
lappend non_terminals definitionModule {
  stack
    {line DEFINITION MODULE moduleIdent}
    {line {optx [ blueprintToObey ]} {optx FOR typeToExtend} ;}
    {line {loop nil {nil importList ;}} {loop nil {nil definition nil}}
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
      {optx REFERENTIAL identList ;}
      MODULE TYPE = {or moduleTypeSpec NONE} ;}
    {line
      {loop nil {nil requirement ;}}
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

# (5) Identifier List
lappend non_terminals identList {
  loop Ident ,
}

# (6) Module Type Specification
lappend non_terminals moduleTypeSpec {
  line {or OPAQUE RECORD} {optx ; propertySpec} {optx ; literalSpec}
}

# (7) Property Specification
lappend non_terminals propertySpec {
  line /TPROPERTIES = determinedProperties {optx | propertiesToDetermine}
}

# (7.1) Determined Properties
lappend non_terminals determinedProperties {
  line identList 
}

# (7.2) Properties To Determine
lappend non_terminals propertiesToDetermine {
  loop {line Ident ?} ,
}

## (7.2) Properties To Determine
#lappend non_terminals propertiesToDetermine {
#  line identList 
#}

# (8) Literal Specification
lappend non_terminals literalSpec {
  line /TLITERAL = {loop protoLiteral |} 
}

# (8.1) Proto Literal
lappend non_terminals protoLiteral {
  or simpleProtoLiteral structuredProtoLiteral 
}

# (8.2) Simple Proto Literal
lappend non_terminals simpleProtoLiteral {
  line literalOrReferential
}

# (8.3) Literal Or Referential Identifier
lappend non_terminals literalOrReferential {
  line Ident 
}

# (9) Structured Proto Literal
lappend non_terminals structuredProtoLiteral {
  line LBRACE {
    or
      {line ARGLIST {optx reqValueCount} OF {
        or
          {line LBRACE {loop simpleProtoLiteral ,} RBRACE}
          simpleProtoLiteral}
        }
      {loop simpleProtoLiteral ,}
  } RBRACE
}

# (9.1) Required Value Count
lappend non_terminals reqValueCount {
  line {optx greaterThan} wholeNumber
}

# (9.2) Greater Than
lappend non_terminals greaterThan {
  line > 
}

# (9.3) Whole Number
lappend non_terminals wholeNumber {
  line NumberLiteral 
}

# (10) Requirement
lappend non_terminals requirement {
  line {optx condition ->}
  {or constRequirement procedureRequirement {line TYPE = procedureType}}
}

# (10.1) Condition
lappend non_terminals condition {
  line {optx NOT} boolConstIdent 
}

# (10.2) Boolean Constant Identifier
lappend non_terminals boolConstIdent {
  line Ident 
}

# (11) Constant Requirement
lappend non_terminals constRequirement {
  line CONST {
    or
      {line [ propertyToBindTo ]
        {or simpleConstRequirement {line = NONE} nil}}
      {line {optx constAttribute} simpleConstRequirement}
    }
}

# (11.1) Property To Bind To
lappend non_terminals propertyToBindTo {
  or memMgtProperty scalarProperty collectionProperty
}

# (11.2) Memory Management Property
lappend non_terminals memMgtProperty {
  or /TDYN /TREFC
}

# (11.3) Scalar Property
lappend non_terminals scalarProperty {
  or /TBASE /TPRECISION /TMINEXP /TMAXEXP
}

# (11.4) Collection Property
lappend non_terminals collectionProperty {
  or /TNIL /TLIMIT
}

# (11.5) Constant Attribute
lappend non_terminals constAttribute {
  or ancillaryConstant restrictedExport
}

# (11.6) Ancillary Constant
lappend non_terminals ancillaryConstant {
  line ~
}

# (11.7) Restricted Export
lappend non_terminals restrictedExport {
  line *
}

# (12) Simple Constant Requirement
lappend non_terminals simpleConstRequirement {
  line Ident {
    or
      {line = constExpression}
      {line : predefOrRefTypeIdent}
    }
}

# (12.1) Constant Expression
lappend non_terminals constExpression {
  line expression
}

# (12.2) Predefined Or Referential Type Identifier
lappend non_terminals predefOrRefTypeIdent {
  line Ident
}

# (13) Procedure Requirement
lappend non_terminals procedureRequirement {
  line PROCEDURE {
    or
      {line [ entityToBindTo ] {or procedureSignature {line = NONE} nil}}
      {line {optx restrictedExport} procedureSignature}
    }
}

# (13.1) Entity To Bind To
lappend non_terminals entityToBindTo {
  or bindableResWord bindableOperator bindableMacro
}

# (13.2) Bindable Reserved Word
lappend non_terminals bindableResWord {
  or
    NEW RETAIN RELEASE COPY FOR
}

# (13.3) Bindable Operator
lappend non_terminals bindableOperator {
  or
    + - * / BACKSLASH = < > *. :: IN DIV MOD unaryMinus
}

# (13.4) Unary Minus
lappend non_terminals unaryMinus {
  line + / -
}

# (13.5) Bindable Macro
lappend non_terminals bindableMacro {
  or
    /ABS /LENGTH /EXISTS /SUBSET /READ /READNEW /WRITE /WRITEF
    /TMIN /TMAX /SXF /VAL multiBindableMacro1 multiBindableMacro2
}

# (13.6) Multi-Bindable Macro 1
lappend non_terminals multiBindableMacro1 {
  line {or /COUNT /RETRIEVE} {optx bindingDifferentiator1}
}

# (13.7) Binding Differentiator 1
lappend non_terminals bindingDifferentiator1 {
  line | #
}

# (13.8) Multi-Bindable Macro 2
lappend non_terminals multiBindableMacro2 {
  line {or /STORE /INSERT /REMOVE} {optx bindingDifferentiator2}
}

# (13.9) Binding Differentiator 2
lappend non_terminals bindingDifferentiator2 {
  line | {or , # *}
}

# (14) Library Generation Directive
lappend non_terminals libGenDirective {
  line GENLIB libIdent FROM template
  FOR {loop {line placeholder = replacement} ;} END
}

# (14.1) Library Identifier
lappend non_terminals libIdent {
  line Ident
}

# (14.2) Template
lappend non_terminals template {
  line Ident
}

# (14.3) Placeholder
lappend non_terminals placeholder {
  line Ident
}

# (14.4) Replacement
lappend non_terminals replacement {
  or NumberLiteral StringLiteral ChevronText
}

# (15) Import Directive
lappend non_terminals importDirective {
  or
    {line IMPORT modulesToImport}
    {line FROM moduleOrEnumIdent IMPORT {or identifiersToImport importAll}}
}

# (15.1) Modules To Import
lappend non_terminals modulesToImport {
  line identifiersToImport
}

# (15.2) Module Or Enumeration Identifier
lappend non_terminals moduleOrEnumIdent {
  line Ident
}

# (15.3) Identifiers To Import
lappend non_terminals identifiersToImport {
  loop {line Ident {optx reExport}} ,
}

# (14.4) Re-Export
lappend non_terminals reExport {
  line +
}

# (15.5) Import All
lappend non_terminals importAll {
  line *
}

# (16) Block
lappend non_terminals block {
  line {loop nil {nil declaration nil}}
  {optx BEGIN statementSequence} END
}

# (17) Statement Sequence
lappend non_terminals statementSequence {
  loop statement ;
}

# (18) Definition
lappend non_terminals definition {
  line {
    or
      {line CONST {loop {line constDefinition ;} nil} }
      {line TYPE {loop {line Ident = {or OPAQUE type} ;} nil} }
      {line VAR {loop {line variableDeclaration ;} nil} }
      {line procedureHeader ;}
  }
}

# (19) Const Definition
lappend non_terminals constDefinition {
  line {or {line [ propertyToBindTo ]} restrictedExport nil}
    Ident = constExpression
}

# (20) Variable Declaration
lappend non_terminals variableDeclaration {
  line identList : {optx range OF} typeIdent
}

# (21) Declaration
lappend non_terminals declaration {
  line {
    or
      {line CONST {loop {line Ident = constExpression ;} nil} }
      {line TYPE {loop {line Ident = type ;} nil} }
      {line VAR {loop {line variableDeclaration ;} nil} }
      {line procedureHeader ; block Ident ;}
  }
}

# (22) Type
lappend non_terminals type {
  line {
    or
      {line typeIdent}
      {line derivedSubType}
      {line enumType}
      {line setType}
      {line arrayType}
      {line recordType}
      {line pointerType}
      {line procedureType}
  }
}

# (22.1) Type Identifier
lappend non_terminals typeIdent {
  line qualident
}

# (22.2) Derived Sub-Type
lappend non_terminals derivedSubType {
  or
    {line ALIAS OF typeIdent}
    {line range OF ordinalOrScalarType}
    {line CONST dynamicTypeIdent}
}

# (22.3) Ordinal Or Scalar Type
lappend non_terminals ordinalOrScalarType {
  line typeIdent
}

# (22.4) Dynamic Type Identifier
lappend non_terminals dynamicTypeIdent {
  line typeIdent
}

# (23) Range
lappend non_terminals range {
  line [ {optx greaterThan} constExpression .. {optx lessThan} constExpression ]
}

# (23.1) Greater Than
lappend non_terminals greatherThan {
  line >
}

# (23.2) Less Than
lappend non_terminals lessThan {
  line <
}

# (24) Enumeration Type
lappend non_terminals enumType {
  line ( {optx + enumTypeToExtend ,} identList )
}

# (24.1) Enumeration Type To Extend
lappend non_terminals enumTypeToExtend {
  line enumTypeIdent
}

# (24.2) Enumeration Type Identifier
lappend non_terminals enumTypeIdent {
  line typeIdent
}

# (25) Set Type
lappend non_terminals setType {
  line SET OF enumTypeIdent
}

# (26) Array Type
lappend non_terminals arrayType {
  line ARRAY {loop componentCount ,} OF typeIdent
}

# (26.1) Component Count
lappend non_terminals componentCount {
  line constExpression
}

# (27) Record Type
lappend non_terminals recordType {
  line RECORD
    {or
      {line {loop fieldList ;} {optx indeterminateField}}
      {line ( recTypeToExtend ) {loop fieldList ;}}
    }
  END
}

# (27.1) Record Type To Extend
lappend non_terminals recTypeToExtend {
  line typeIdent
}

# (27.2) Field List
lappend non_terminals fieldList {
  line {optx restrictedExport} variableDeclaration
}

# (28) Indeterminate Field Declaration
lappend non_terminals indeterminateField {
  line INDETERMINATE Ident : ARRAY discriminantFieldIdent OF typeIdent
}

# (28.1) Discriminant Field Identifier
lappend non_terminals discriminantFieldIdent {
  line Ident
}

# (29) Pointer Type
lappend non_terminals pointerType {
  line POINTER TO {optx CONST} typeIdent
}

# (30) Procedure Type
lappend non_terminals procedureType {
  line PROCEDURE {optx ( {loop formalType ,} )} {optx : returnedType}
}

# (30.1) Formal Type
lappend non_terminals formalType {
  or simpleFormalType attributedFormalType variadicFormalType
}

# (30.2) Returned Type
lappend non_terminals returnedType {
  line typeIdent
}

# (31) Simple Formal Type
lappend non_terminals simpleFormalType {
  or
    {line {optx ARRAY OF} typeIdent}
    castingFormalType
}

# (31.1) Casting Formal Type
lappend non_terminals castingFormalType {
  line /CAST {or {line ARRAY OF /OCTET} addressTypeIdent}
}

# (31.2) Address Type Identifier
lappend non_terminals addressTypeIdent {
  or /ADDRESS {line /UNSAFE . /ADDRESS}
}

# (32) Attributed Formal Type
lappend non_terminals attributedFormalType {
  line {or CONST VAR NEW} {or simpleFormalType simpleVariadicFormalType}
}

# (33) Simple Variadic Formal Type
lappend non_terminals simpleVariadicFormalType {
  line ARGLIST {optx numOfArgsToPass} OF
    simpleFormalType {optx {line | arglistTerminator}}
}

# (33.1) Number Of Arguments To Pass
lappend non_terminals numOfArgsToPass {
  line {optx greaterThan} constExpression
}

# (33.2) Variadic List Terminator
lappend non_terminals arglistTerminator {
  or /NIL minusOne wholeNumber constQualident
}

# (33.3) Minus One (Negative One)
lappend non_terminals minusOne {
  line - 1
}

# (33.4) Constant Qualified Identifier
lappend non_terminals constQualident {
  line qualident
}

# (34) Variadic Formal Type
lappend non_terminals variadicFormalType {
  stack
    {line ARGLIST {optx numOfArgsToPass} OF}
    {line {
      or
        {line LBRACE {loop nonVariadicFormalType ;} RBRACE}
        simpleFormalType
    } {optx | arglistTerminator}}
}

# (35) Non-Variadic Formal Type
lappend non_terminals nonVariadicFormalType {
  line {or CONST VAR NEW nil} simpleFormalType
}

# (36) Procedure Header
lappend non_terminals procedureHeader {
  line PROCEDURE {or {line [ entityToBindTo ]} restrictedExport nil}
  procedureSignature
}

# (37) Procedure Signature
lappend non_terminals procedureSignature {
  line Ident {optx ( {loop formalParams ;} )} {optx : returnedType}
}

# (38) Formal Parameters
lappend non_terminals formalParams {
  or
    {line identList : {or simpleFormalType variadicFormalParams}}
    attributedFormalParams
}

# (39) Attributed Formal Parameters
lappend non_terminals attributedFormalParams {
  line {or CONST VAR NEW} identList :
    {or simpleFormalType simpleVariadicFormalType}
}

# (40) Variadic Formal Parameters
lappend non_terminals variadicFormalParams {
  stack
    {line ARGLIST {optx numOfArgsToPass} OF}
    {line {
      or
        {line LBRACE {loop nonVariadicFormalParams ;} RBRACE}
        simpleFormalType
    } {optx {line | arglistTerminator}}}
}

# (41) Non-Variadic Formal Parameters
lappend non_terminals nonVariadicFormalParams {
  line {or CONST VAR NEW nil} identList : simpleFormalType
}

# (42) Qualified Identifier
lappend non_terminals qualident {
  loop Ident .
}

# (43) Statement
lappend non_terminals statement {
  line {
    or
      memMgtOperation
      updateOrProcCall
      ifStatement
      caseStatement
      loopStatement
      whileStatement
      repeatStatement
      forStatement
      {line RETURN {optx expression}}
      EXIT
  }
}

# (44) Memory Management Operation
lappend non_terminals memMgtOperation {
  or
    {line NEW designator {or {line OF initSize} {line := initValue} nil}}
    {line RETAIN designator}
    {line RELEASE designator}
}

# (44.1) Initial Size
lappend non_terminals initSize {
  line expression
}

# (44.2) Initial Value
lappend non_terminals initValue {
  line expression
}

# (45) Update Or Procedure Call
lappend non_terminals updateOrProcCall {
  or
    {line designator {
      or
        {line := expression}
        {line incOrDecSuffix}
        {line actualParameters}
        nil
      }
    }
    {line COPY designator := expression}
}

# (45.1) Increment Or Decrement Suffix
lappend non_terminals incOrDecSuffix {
  or
    {line ++}
    {line --}
}

# (46) IF Statement
lappend non_terminals ifStatement {
  stack
    {line IF boolExpression THEN statementSequence}
    {optx {loop {line ELSIF boolExpression THEN statementSequence} nil}}
    {line {optx ELSE statementSequence} END}
}

# (46.1) Boolean Expression
lappend non_terminals boolExpression {
  line expression
}

# (47) CASE Statement
lappend non_terminals caseStatement {
  line CASE expression OF
    {loop {line | case} nil} {optx ELSE statementSequence} END
}

# (48) Case
lappend non_terminals case {
  line {loop caseLabels ,} : statementSequence
}

# (49) Case Labels
lappend non_terminals caseLabels {
  line constExpression {optx .. constExpression}
}

# (50) LOOP Statement
lappend non_terminals loopStatement {
  line LOOP statementSequence END
}

# (51) WHILE Statement
lappend non_terminals whileStatement {
  line WHILE boolExpression DO statementSequence END
}

# (52) REPEAT Statement
lappend non_terminals repeatStatement {
  line REPEAT statementSequence UNTIL boolExpression
}

# (53) FOR Statement
lappend non_terminals forStatement {
  stack
    {line FOR controlVariable {optx ascendOrDescend}}
    {line IN {or designator {line range OF ordinalType}}
      DO statementSequence END}
}

# (53.1) Control Variable
lappend non_terminals controlVariable {
  line Ident
}

# (53.2) Ascend Or Descend
lappend non_terminals ascendOrDescend {
  line incOrDecSuffix
}

# (53.3) Ordinal Type
lappend non_terminals ordialType {
  line typeIdent
}

# (54) Designator
lappend non_terminals designator {
  line qualident {optx designatorTail}
}

# (55) Designator Tail
lappend non_terminals designatorTail {
  line {
    loop {
      line {
        or
          {line [ exprListOrSlice ]}
          ^
      }
      {optx {loop {line . Ident} nil}}
    }
  }
}

# (56) Expression List Or Slice
lappend non_terminals exprListOrSlice {
  line expression {
    optx {
      or
        {loop {line , expression} nil}
        {line .. {optx expression}}
    }
  }
}

# (57) Expression
lappend non_terminals expression {
  line simpleExpression {optx operL1 simpleExpression}
}

# (57.1) Level-1 Operator
lappend non_terminals operL1 {
  or
    = # < <= > >= IN identityOp arrayConcatOp
}

# (57.2) Identity Operator
lappend non_terminals identityOp {
  line ==
}

# (57.3) Array Concatenation Operator
lappend non_terminals arrayConcatOp {
  line +>
}

# (58) Simple Expression
lappend non_terminals simpleExpression {
  line {or + - nil} {loop term operL2}
}

# (58.1) Level-2 Operator
lappend non_terminals operL2 {
  line {or + - OR}
}

# (59) Term
lappend non_terminals term {
  loop factorOrNegation operL3
}

# (59.1) Level-3 Operator
lappend non_terminals operL3 {
  line {or * / DIV MOD AND setDiffOp dotProductOp}
}

# (59.2) Set Difference Operator
lappend non_terminals setDiffOp {
  line BACKSLASH
}

# (59.3) Dot Product Operator
lappend non_terminals dotProductOp {
  line *.
}

# (60) Factor Or Negation
lappend non_terminals factorOrNegation {
  line {optx NOT} factorOrTypeConv
}

# (61) Factor Or Type Conversion
lappend non_terminals factorOrTypeConv {
  line factor {optx :: typeIdent}
}

# (62) Factor
lappend non_terminals factor {
  line {
    or
      NumberLiteral
      StringLiteral
      structuredValue
      {line ( expression )}
      {line designator {optx actualParameters}}
    }
}

# (63) Actual Parameters
lappend non_terminals actualParameters {
  line ( {optx expressionList} )
}

# (64) Expression List
lappend non_terminals expressionList {
  loop expression ,
}

# (65) Structured Value
lappend non_terminals structuredValue {
  line LBRACE {loop valueComponent ,} RBRACE
}

# (66) Value Component
lappend non_terminals valueComponent {
  or
    {line constExpression {optx {or BY ..} constExpression}}
    {line runtimeExpression}
}

# (66.1) Runtime Expression
lappend non_terminals runtimeExpression {
  line expression
}

# ---------------------------------------------------------------------------
# Non-Terminal Symbols for Optional Language Extensions
# ---------------------------------------------------------------------------
#

# Architecture Specific Implementation Module Selection

# Replacement for #2
lappend non_terminals langExtn_programModule {
  line MODULE moduleIdent {optx ( arch ) } ;
  {loop nil {nil importList ;}} block moduleIdent .
}

# Architecture
lappend non_terminals langExtn_arch {
  line Ident
}

# Register Mapping Facility

# Replacement for #31
lappend non_terminals langExtn_simpleFormalType {
  or
    {line typeIdent {optx regAttribute}}
    {line ARRAY OF typeIdent}
    {line castingFormalType}
}

# Replacement for #31.1
lappend non_terminals langExtn_castingFormalType {
  line /CAST {
    or
      {line ARRAY OF /OCTET}
      {line addressTypeIdent {optx regAttribute}}
    }
}

# Register Mapping Attribute
lappend non_terminals langExtn_regAttribute {
  line IN REG {or registerNumber registerMnemonic}
}

# Register Number
lappend non_terminals langExtn_registerNumber {
  line constExpression
}

# Register Mnemonic
lappend non_terminals langExtn_registerMnemonic {
  line qualident
}

# Symbolic Assembly Inline Facility

# Replacement for #43
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
      assemblyBlock
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

# ------------------
# (1) Reserved Words
# ------------------
set res_words {}

# (1.1) ALIAS
lappend res_words ALIAS {
  line ALIAS
}

# (1.2) AND
lappend res_words AND {
  line AND
}

# (1.3) ARGLIST
lappend res_words ARGLIST {
  line ARGLIST
}

# (1.4) ARRAY
lappend res_words ARRAY {
  line ARRAY
}

# (1.5) BEGIN
lappend res_words BEGIN {
  line BEGIN
}

# (1.6) BLUEPRINT
lappend res_words BLUEPRINT {
  line BLUEPRINT
}

# (1.7) BY
lappend res_words BY {
  line BY
}

# (1.8) CASE
lappend res_words CASE {
  line CASE
}

# (1.9) CONST
lappend res_words CONST {
  line CONST
}

# (1.10) COPY
lappend res_words COPY {
  line COPY
}

# (1.11) DEFINITION
lappend res_words DEFINITION {
  line DEFINITION
}

# (1.12) DIV
lappend res_words DIV {
  line DIV
}

# (1.13) DO
lappend res_words DO {
  line DO
}

# (1.14) ELSE
lappend res_words ELSE {
  line ELSE
}

# (1.15) ELSIF
lappend res_words ELSIF {
  line ELSIF
}

# (1.16) END
lappend res_words END {
  line END
}

# (1.17) EXIT
lappend res_words EXIT {
  line EXIT
}

# (1.18) FOR
lappend res_words FOR {
  line FOR
}

# (1.19) FROM
lappend res_words FROM {
  line FROM
}

# (1.20) GENLIB
lappend res_words GENLIB {
  line GENLIB
}

# (1.21) IF
lappend res_words IF {
  line IF
}

# (1.22) IMPLEMENTATION
lappend res_words IMPLEMENTATION {
  line IMPLEMENTATION
}

# (1.23) IMPORT
lappend res_words IMPORT {
  line IMPORT
}

# (1.24) IN
lappend res_words IN {
  line IN
}

# (1.25) INDETERMINATE
lappend res_words INDETERMINATE {
  line INDETERMINATE
}

# (1.26) LOOP
lappend res_words LOOP {
  line LOOP
}

# (1.27) MOD
lappend res_words MOD {
  line MOD
}

# (1.28) MODULE
lappend res_words MODULE {
  line MODULE
}

# (1.29) NEW
lappend res_words NEW {
  line NEW
}

# (1.30) NONE
lappend res_words NONE {
  line NONE
}

# (1.31) NOT
lappend res_words NOT {
  line NOT
}

# (1.32) OF
lappend res_words OF {
  line OF
}

# (1.33) OPAQUE
lappend res_words OPAQUE {
  line OPAQUE
}

# (1.34) OR
lappend res_words OR {
  line OR
}

# (1.35) POINTER
lappend res_words POINTER {
  line POINTER
}

# (1.36) PROCEDURE
lappend res_words PROCEDURE {
  line PROCEDURE
}

# (1.37) RECORD
lappend res_words RECORD {
  line RECORD
}

# (1.38) REFERENTIAL
lappend res_words REFERENTIAL {
  line REFERENTIAL
}

# (1.39) RELEASE
lappend res_words RELEASE {
  line RELEASE
}

# (1.40) REPEAT
lappend res_words REPEAT {
  line REPEAT
}

# (1.41) RETAIN
lappend res_words RETAIN {
  line RETAIN
}

# (1.42) RETURN
lappend res_words RETURN {
  line RETURN
}

# (1.43) SET
lappend res_words SET {
  line SET
}

# (1.44) THEN
lappend res_words THEN {
  line THEN
}

# (1.45) TO
lappend res_words TO {
  line TO
}

# (1.46) TYPE
lappend res_words TYPE {
  line TYPE
}

# (1.47) UNTIL
lappend res_words UNTIL {
  line UNTIL
}

# (1.48) VAR
lappend res_words VAR {
  line VAR
}

# (1.49) WHILE
lappend res_words WHILE {
  line WHILE
}

# ------------------------
# (2) Dual-Use Identifiers
# ------------------------
set res_idents {}

# (2.1) ABS
lappend res_idents ABS {
  line /ABS
}

# (2.2) ADDRESS
lappend res_idents ADDRESS {
  line /ADDRESS
}

# (2.3) CAST
lappend res_idents CAST {
  line /CAST
}

# (2.4) COUNT
lappend res_idents COUNT {
  line /COUNT
}

# (2.5) EXISTS
lappend res_idents EXISTS {
  line /EXISTS
}

# (2.6) INSERT
lappend res_idents INSERT {
  line /INSERT
}

# (2.7) LENGTH
lappend res_idents LENGTH {
  line /LENGTH
}

# (2.8) NIL
lappend res_idents /NIL {
  line /NIL
}

# (2.9) OCTET
lappend res_idents /OCTET {
  line /OCTET
}

# (2.10) READ
lappend res_idents /READ {
  line /READ
}

# (2.11) READNEW
lappend res_idents READNEW {
  line /READNEW
}

# (2.12) REMOVE
lappend res_idents REMOVE {
  line /REMOVE
}

# (2.13) RETRIEVE
lappend res_idents RETRIEVE {
  line /RETRIEVE
}

# (2.14) STORE
lappend res_idents STORE {
  line /STORE
}

# (2.15) SUBSET
lappend res_idents SUBSET {
  line /SUBSET
}

# (2.16) SXF
lappend res_idents SXF {
  line /SXF
}

# (2.17) TBASE
lappend res_idents TBASE {
  line /TBASE
}

# (2.18) TDYN
lappend res_idents TDYN {
  line /TDYN
}

# (2.19) TLIMIT
lappend res_idents TLIMIT {
  line /TLIMIT
}

# (2.20) TLITERAL
lappend res_idents TLITERAL {
  line /TLITERAL
}

# (2.21) TMAX
lappend res_idents TMAX {
  line /TMAX
}

# (2.22) TMAXEXP
lappend res_idents TMAXEXP {
  line /TMAXEXP
}

# (2.23) TMIN
lappend res_idents TMIN {
  line /TMIN
}

# (2.24) TMINEXP
lappend res_idents TMINEXP {
  line /TMINEXP
}

# (2.25) TNIL
lappend res_idents TNIL {
  line /TNIL
}

# (2.26) TPRECISION
lappend res_idents TPRECISION {
  line /TPRECISION
}

# (2.27) TPROPERTIES
lappend res_idents TPROPERTIES {
  line /TPROPERTIES
}

# (2.28) TREFC
lappend res_idents TREFC {
  line /TREFC
}

# (2.29) UNSAFE
lappend res_idents UNSAFE {
  line /UNSAFE
}

# (2.30) VAL
lappend res_idents VAL {
  line /VAL
}

# (2.31) WRITE
lappend res_idents WRITE {
  line /WRITE
}

# (2.32) WRITEF
lappend res_idents WRITEF {
  line /WRITEF
}


# -------------------
# (3) Special Symbols
# -------------------
set res_symbols {}

# (3.1) Double Quote
lappend res_symbols DoubleQuote {
  line DOUBLE_QUOTE
}

# (3.2) Octothorpe
lappend res_symbols Octothorpe {
  line #
}

# (3.3) Single Quote
lappend res_symbols SingleQuote {
  line SINGLE_QUOTE
}

# (3.4) Left Parenthesis
lappend res_symbols LeftParen {
  line (
}

# (3.5) Right Parenthesis
lappend res_symbols RightParen {
  line )
}

# (3.6) Asterisk
lappend res_symbols Asterisk {
  line *
}

# (3.7) Plus
lappend res_symbols Plus {
  line +
}

# (3.8) Comma
lappend res_symbols Comma {
  line ,
}

# (3.9) Minus
lappend res_symbols Minus {
  line -
}

# (3.10) Dot
lappend res_symbols Dot {
  line .
}

# (3.11) Slash
lappend res_symbols Slash {
  line /
}

# (3.12) Colon
lappend res_symbols Colon {
  line :
}

# (3.13) Semicolon
lappend res_symbols Semicolon {
  line ;
}

# (3.14) Less Than
lappend res_symbols LessThan {
  line <
}

# (3.15) Equal
lappend res_symbols Equal {
  line =
}

# (3.16) Greater Than
lappend res_symbols GreaterThan {
  line >
}

# (3.17) At
lappend res_symbols At {
  line @
}

# (3.18) Left Bracket
lappend res_symbols LeftBracket {
  line [
}

# (3.19) Right Bracket
lappend res_symbols RightBracket {
  line ]
}

# (3.20) Backslash
lappend res_symbols Backslash {
  line BACKSLASH
}

# (3.21) Caret
lappend res_symbols Caret {
  line ^
}

# (3.22) Left Brace
lappend res_symbols LeftBrace {
  line LBRACE
}

# (3.23) Vertical Bar
lappend res_symbols VerticalBar {
  line |
}

# (3.24) Right Brace
lappend res_symbols RightBrace {
  line RBRACE
}

# (3.24) Tilde
lappend res_symbols Tilde {
  line ~
}

# (3.25) Question Mark
lappend res_symbols QuestionMark {
  line ?
}

# (3.26) Open Comment
lappend res_symbols OpenComment {
  line (*
}

# (3.27) Close Comment
lappend res_symbols CloseComment {
  line *)
}

# (3.28) Dot Product
lappend res_symbols DotProduct {
  line *.
}

# (3.29) Close Pragma
lappend res_symbols ClosePragma {
  line *>
}

# (3.30) Double Plus
lappend res_symbols DoublePlus {
  line ++
}

# (3.31) Array Concat
lappend res_symbols ArrayConcat {
  line +>
}

# (3.32) Double Minus
lappend res_symbols DoubleMinus {
  line --
}

# (3.33) Right Arrow
lappend res_symbols RightArrow {
  line ->
}

# (3.34) Double Dot
lappend res_symbols DoubleDot {
  line ..
}

# (3.35) Double Slash
lappend res_symbols DoubleSlash {
  line //
}

# (3.36) Double Colon
lappend res_symbols DoubleColon {
  line ::
}

# (3.37) Assign
lappend res_symbols Assign {
  line :=
}

# (3.38) Identity
lappend res_symbols Identity {
  line ==
}

# (3.39) Less Or Equal
lappend res_symbols LessOrEqual {
  line <=
}

# (3.40) Open Pragma
lappend res_symbols OpenPragma {
  line <*
}

# (3.41) Greater Or Equal
lappend res_symbols GreaterOrEqual {
  line >=
}

# --------------
# (4) Identifier
# --------------
lappend terminals Ident {
  line {
    or
    Letter
    {line {loop ForeignIdentChar nil} {loop LetterOrDigit nil}}
  }
  {loop nil IdentTailChar}
}

# (4.1) Foreign Identifier Character
lappend terminals ForeignIdentChar {
  or _ $
}

# (4.2) Letter Or Digit
lappend terminals LetterOrDigit {
  or Letter Digit
}

# (4.3) Identifier Tail Character
lappend terminals IdentTailChar {
  or LetterOrDigit ForeignIdentChar
}

# (4.4) Standard Library Identifier
lappend terminals StdLibIdent {
  line Letter {loop nil LetterOrDigit}
}

# ------------------
# (5) Number Literal
# ------------------
lappend terminals NumberLiteral {
  or
    {line 0 {
      or
        RealNumberTail
        {line b Base2DigitSeq}
        {line x Base16DigitSeq}
        {line u Base16DigitSeq}
        nil
      }}
    {line 1..9 {optx DecimalNumberTail}}
}

# (5.1) Decimal Number Tail
lappend terminals DecimalNumberTail {
  or
   {line {optx SINGLE_QUOTE} DigitSeq {optx RealNumberTail}}
   RealNumberTail
}

# Digit Separator
# lappend terminals DigitSep {
#   line SINGLE_QUOTE
# }

# (5.2) Real Number Tail
lappend terminals RealNumberTail {
  line . DigitSeq {optx e {or + - nil} DigitSeq }
}

# (5.3) Digit Sequence
lappend terminals DigitSeq {
  loop DigitGroup SINGLE_QUOTE
}

# (5.3b) Digit Group
lappend terminals DigitGroup {
  loop Digit nil
}

# (5.4) Base-16 Digit Sequence
lappend terminals Base16DigitSeq {
  loop Base16DigitGroup SINGLE_QUOTE
}

# (5.4b) Base-16 Digit Group
lappend terminals Base16DigitGroup {
  loop Base16Digit nil
}

# (5.5) Base-2 Digit Sequence
lappend terminals Base2DigitSeq {
  loop Base2DigitGroup SINGLE_QUOTE
}

# (5.5b) Base-2 Digit Group
lappend terminals Base2DigitGroup {
  loop Base2Digit nil
}

# (5.6) Digit
lappend terminals Digit {
  or Base2Digit 2 3 4 5 6 7 8 9
}

# (5.7) Base-16 Digit
lappend terminals Base16Digit {
  or Digit A B C D E F
}

# (5.8) Base-2 Digit
lappend terminals Base2Digit {
  or 0 1
}

# ------------------
# (6) String Literal
# ------------------
lappend terminals StringLiteral {
  or SingleQuotedString DoubleQuotedString
}

# (6.1) Single Quoted String
lappend terminals SingleQuotedString {
  line SINGLE_QUOTE
    {optx {loop {or QuotableCharacter DOUBLE_QUOTE} nil}}
  SINGLE_QUOTE
}

# (6.2) Double Quoted String
lappend terminals DoubleQuotedString {
  line DOUBLE_QUOTE
    {optx {loop {or QuotableCharacter SINGLE_QUOTE} nil}}
  DOUBLE_QUOTE
}

# (6.3) Quotable Character
lappend terminals QuotableCharacter {
  or Digit Letter Space NonAlphaNumQuotable EscapedCharacter
}

# (6.4) Letter
lappend terminals Letter {
  or A..Z a..z 
}

# (6.5) Space
# CONST Space = CHR(32);

# (6.6a) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable1 {
  or ! # $ % & ( ) * + ,
}

# (6.6b) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable2 {
  or - . / : ; < = > ? @
}

# (6.6c) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable3 {
  or [ ] ^ _ ` LBRACE | RBRACE ~
}

# (6.7) Escaped Character
lappend terminals EscapedCharacter {
  line BACKSLASH {or n t BACKSLASH}
}

# ---------------------------------
# (7) Chevron Delimited Source Text
# ---------------------------------
lappend terminals ChevronText {
  line <<
    {optx {loop {or QuotableCharacter SINGLE_QUOTE DOUBLE_QUOTE} nil}}
  >>
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
  line // {optx {loop CommentCharacter nil}} EndOfLine
}

# (3) Block Comment
lappend ignore_symbols BlockComment {
  line (* {optx {loop {or CommentCharacter BlockComment EndOfLine} nil}} *)
}

# (3.1) Comment Character
lappend ignore_symbols CommentCharacter {
  or Digit Letter Whitespace NonAlphaNumQuotable
  BACKSLASH SINGLE_QUOTE DOUBLE_QUOTE EndOfLine
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

# (1.1) Pragma Body
lappend pragmas pragmaBody {
  or
    pragmaMSG
    pragmaIF
    procDeclAttrPragma
    pragmaPTW
    pragmaFORWARD
    pragmaENCODING
    pragmaALIGN
    pragmaPADBITS
    pragmaPURITY
    varDeclAttrPragma
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

# (2.2) Compile Time Message Component
lappend pragmas compileTimeMsgComponent {
  line {
    or
      StringLiteral
      constQualident
      {line @ valuePragma}
  }
}

# (2.3) Value Pragma
lappend pragmas valuePragma {
  or ALIGN ENCODING implDefinedPragmaSymbol
}

# (2.4) Constant Qualified Identifier
lappend pragmas constQualident {
  line qualident
}

# (2.5) Implementation Defined Pragma Symbol
lappend pragmas implDefinedPragmaSymbol {
  line Ident
}

# (3) Body Of Conditional Compilation Pragma
lappend pragmas pragmaIF {
  or
    {line {or IF ELSIF} inPragmaExpression}
    ELSE
    ENDIF
}

# (4) Body Of Procedure Declaration Attribute Pragma
lappend pragmas procDeclAttrPragma {
  or INLINE NOINLINE NORETURN
}

# (5) Body Of Promise-To-Write Pragma
lappend pragmas pragmaPTW {
  line PTW
}

# (6) Body Of Forward Declaration Pragma
lappend pragmas pragmaFORWARD {
  line FORWARD {or {line TYPE identList} procedureHeader}
}

# (7) Body Of Character Encoding Pragma
lappend pragmas pragmaENCODING {
  line ENCODING = {or `ASCII `UTF8} {optx : codePointSampleList}
}

# (7.1) Code Point Sample List
lappend pragmas codePointSampleList {
  loop {line quotedCharacter = CharCodeLiteral} ,
}

# (7.2) Quoted Character
lappend pragmas quotedCharacter {
  line StringLiteral
}

# (7.3) Character Code Literal
lappend pragmas charCodeLiteral {
  line NumberLiteral
}

# (8) Body Of Memory Alignment Pragma
lappend pragmas pragmaALIGN {
  line ALIGN = inPragmaExpression
}

# (9) Body Of Bit Padding Pragma
lappend pragmas pragmaPADBITS {
  line PADBITS = inPragmaExpression
}

# (10) Body Of Purity Attribute Pragma
lappend pragmas pragmaPURITY {
  line PURITY = inPragmaExpression
}

# (11) Body Of Variable Declaration Attribute Pragma
lappend pragmas variableAttrPragma {
  or SINGLEASSIGN LOWLATENCY VOLATILE
}

# (12) Body Of Deprecation Pragma
lappend pragmas pragmaDEPRECATED {
  line DEPRECATED
}

# (13) Body Of Generation Timestamp Pragma
lappend pragmas pragmaGENERATED {
  line GENERATED FROM template , datestamp , timestamp
}

# (13.1) Date Stamp
lappend pragmas datestamp {
  line year - month - day
}

# (13.2) Time Stamp
lappend pragmas timestamp {
  line hours : minutes : seconds + timezone
}

# (13.3) year, month, day, hours, minutes, seconds, timezone
lappend pragmas year_month_day_etc {
  line wholeNumber
}

# (14) Body Of Memory Mapping Pragma
lappend pragmas pragmaADDR {
  line ADDR = inPragmaExpression
}

# (15) Body Of Foreign Function Interface Pragma
lappend pragmas pragmaFFI {
  line FFI = {or `C `Fortran `CLR `JVM }
}

# (16) Body Of Foreign Function Identifier Mapping Pragma
lappend pragmas pragmaFFIDENT {
  line FFIDENT = StringLiteral
}

# (17) Body of Implementation Defined Pragma
lappend pragmas implDefinedPragma {
  line implDefinedPragmaSymbol {optx = inPragmaExpression}
    | messageMode
}

# (18) In-Pragma Expression
lappend pragmas inPragmaExpression {
  line inPragmaSimpleExpr {optx inPragmaOperL1 inPragmaSimpleExpr}
}

# (18.1) In-Pragma Level-1 Operator
lappend pragmas inPragmaOperL1 {
  or = # < <= > >=
}

# (19) In-Pragma Simple Expression
lappend pragmas inPragmaSimpleExpr {
  line {or + - nil} {loop inPragmaTerm inPragmaOperL2}
}

# (19.1) In-Pragma Level-2 Operator
lappend pragmas inPragmaOperL2 {
  or + - OR
}

# (20) In-Pragma Term
lappend pragmas inPragmaTerm {
  loop inPragmaFactorOrNegation inPragmaOperL3
}

# (20.1) In-Pragma Level-3 Operator
lappend pragmas inPragmaOperL3 {
  or * DIV MOD AND
}

# (21) In-Pragma Factor Or Negation
lappend pragmas inPragmaFactorOrNegation {
  line {optx NOT} inPragmaFactor
}

# (22) In-Pragma Factor
lappend pragmas inPragmaFactor {
  or
    wholeNumber
    constQualident
    {line ( inPragmaExpression )}
    inPragmaCompileTimeFunctionCall
}

# (23) In-Pragma Compile Time Function Call
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

# Menu: All Diagrams
#
button $b -text "Draw All Diagrams" -command {draw_all_graphs}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Non-Terminals
#
incr bn
set b .bb.b$bn
button $b -text "Draw Non-Terminals" -command {draw_graphs $non_terminals}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Terminals
#
incr bn
set b .bb.b$bn
button $b -text "Draw Terminals" -command {draw_graphs $terminals}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Reserved Words
#
incr bn
set b .bb.b$bn
button $b -text "Draw Reserved Words" -command {draw_graphs $res_words}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Dual-Use Identifiers
#
incr bn
set b .bb.b$bn
button $b -text "Draw Dual-Use Identifiers" -command {draw_graphs $res_idents}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Special Symbols
#
incr bn
set b .bb.b$bn
button $b -text "Draw Special Symbols" -command {draw_graphs $res_symbols}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Pragmas
#
incr bn
set b .bb.b$bn
button $b -text "Draw Pragmas" -command {draw_graphs $pragmas}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Ignore Symbols
#
incr bn
set b .bb.b$bn
button $b -text "Draw Ignore Symbols" -command {draw_graphs $ignore_symbols}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Aliases
#
incr bn
set b .bb.b$bn
button $b -text "Draw Aliases" -command {draw_graphs $aliases}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Legend
#
incr bn
set b .bb.b$bn
button $b -text "Draw Legend" -command {draw_graphs $legend}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Quit
#
incr bn
set b .bb.b$bn
button $b -text "Quit" -command {exit}
pack $b -side top -fill x -expand 0 -pady {0 14}


# ---------------------------------------------------------------------------
# L a y o u t - P a r a m e t e r s
# ---------------------------------------------------------------------------
#
set RES_WORD_FONT {Helvetica 12 bold};           # reserved word font
set RES_IDENT_FONT {Helvetica 12 bold italic};   # reserved identifier font
set TERM_FONT {Helvetica 12 bold};               # font for other terminals
set NON_TERM_FONT {Helvetica 12};                # non-terminal font
set TOKEN_FONT {Monaco 12 bold};                 # special symbol token font
set STRING_FONT {Courier 12 bold};               # quoted string font
set RADIUS 9;                                    # default turn radius
set HSEP 15;                                     # horizontal separation
set VSEP 7;                                      # vertical separation
set LWIDTH 3;                                    # line width
set DPI 96;                                      # dots per inch for GIFs

set tagcnt 0; # tag counter - don't modify this


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
# special name replacements
  set isQuotedString 0
  if {$txt=="SPACE"} {
    set label "' '"
  } elseif {$txt=="BACKSLASH"} {
    set label "\\"
  } elseif {$txt=="SINGLE_QUOTE"} {
    set label "\'"
  } elseif {$txt=="DOUBLE_QUOTE"} {
    set label "\""
  } elseif {$txt=="LBRACE" || $txt=="LEFT_BRACE"} {
    set label "\{"
  } elseif {$txt=="RBRACE" || $txt=="RIGHT_BRACE"} {
    set label "\}"
  } else {
    set label $txt
  }
# initialise symbol flags
  set isReservedIdent 0
  set isNonTerminal 0
  set isTerminal 0
  set isToken 0
# determine type of symbol
  if {[regexp {^[A-Z][A-Z]+$} $label]} {
    # reserved word
    set isTerminal 1
    set isReservedWord 1
    set font $::RES_WORD_FONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^/[A-Z][A-Z]+$} $label]} {
    set label [string range $label 1 end]
    # reserved identifier
    set isTerminal 1
    set isReservedIdent 1
    set font $::RES_IDENT_FONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^[A-Z][a-z0-9][a-zA-Z0-9]*$} $label]} {
    # other terminal
    set isTerminal 1
    set font $::TERM_FONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^[a-z][a-zA-Z0-9]+$} $label]} {
    # non-terminal
    set isNonTerminal 1
    set font $::NON_TERM_FONT
    set label "  $label  "
    set baseline 0
 } elseif {[regexp {^`[a-zA-Z0-9]+$} $label]} {
    # quoted string literal
    set label [string range $label 1 end]
    set isToken 1
    set font $::STRING_FONT
    set label " \"$label\" "
    set baseline [expr {$LWIDTH/2}]
 } elseif {[regexp {^[a-zA-Z]$} $label]} {
    # single letter token
    set isToken 1
    set font $::TOKEN_FONT
    set baseline [expr {$LWIDTH/2}]
 } elseif {[regexp {^[a-zA-Z0-9]\.\.[a-zA-Z0-9]$} $label]} {
    # letter or digit range
    set isToken 1
    set font $::TOKEN_FONT
    set baseline [expr {$LWIDTH/2}]
 } else {
    # anything else is a token
    set isToken 1
    set font $::TOKEN_FONT
    set baseline [expr {$LWIDTH/2}]
  }
  set id1 [.c create text 0 $baseline -anchor c -text $label -font $font -tags $tag]
# lassign [.c bbox $id1] x0 y0 x1 y1 # to do: replace all foreach with lassign
  foreach {x0 y0 x1 y1} [.c bbox $id1] break
# move parentheses, brackets, braces and underscore up by one pixel
  if {$label in {( ) [ ] \{ \} _ }} { .c move $id1 0 -1 }
# move the asterisk down by one pixel
  if {$label=="*"} { .c move $id1 0 1 }
# move label left by one pixel if font is italic
  set slantAttr [font actual $font -slant]
  if {$slantAttr eq "italic"} { .c move $id1 -1 0 }
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
  global res_words
  global res_idents
  global res_symbols
  global pragmas
  global ignore_symbols
  global aliases
  global legend
  draw_graphs $non_terminals
  draw_graphs $terminals
  draw_graphs $res_words
  draw_graphs $res_idents
  draw_graphs $res_symbols
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