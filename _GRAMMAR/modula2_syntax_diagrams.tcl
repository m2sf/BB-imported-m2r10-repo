#!/usr/bin/wish
#
# Syntax diagram generator for Modula-2 (R10), status Oct 15, 2012
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
#   set all_graphs { production1 { ... } production2 { ... } ... }
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
#   with the name of the script as an argument: $ wish modula2.tcl
#
#   in the window that appears, click on the top button "Draw All Diagrams"
#
#   Diagrams will be written into postscript files in the working directory
#  
# ===========================================================================
#

# ---------------------------------------------------------------------------
# Modula-2 grammar
# ---------------------------------------------------------------------------
#  replace this with your own grammar, do not add comments or blank lines!
#
#  to do: build the grammar list one by one production to be able to insert
#  comments for documentation before each production rule, using lappend:
#
#  set grammar {}
#  lappend grammar production1 { ... }
#  lappend grammar production2 { ... }
#  etc etc
#
set all_graphs {
  compilationUnit {
    or
      {line prototype}
      {line definitionOfModule}
      {line IMPLEMENTATION programModule}
  }
  prototype {
    stack
      {line PROTOTYPE prototypeId {opt [ requiredConformance ]} ;}
      {line {opt PLACEHOLDERS identList ;} requiredTypeDefinition}
      {line {loop nil {nil ; requiredBinding}} END prototypeId .}
  }
  prototypeId {
    line Ident
  }
  requiredConformance {
    line Ident
  }
  programModule {
    line MODULE moduleId ;
    {loop nil {nil importList nil}} block moduleId .
  }
  moduleId {
    line Ident
  }
  definitionOfModule {
    stack
      {line DEFINITION MODULE moduleId {opt [ requiredConformance ]} ;}
      {line {loop nil {nil importList nil}} {loop nil {nil definition nil}}
        END moduleId .}
  }
  requiredTypeDefinition {
      line TYPE = {loop permittedTypeDef |} {opt := {loop protoliteral |}}
  }
  requiredTypeDefinitionTEST {
      line TYPE = {loop permittedTypeDef ,} {opt := {loop protoliteral ,}}
  }
  permittedTypeDef {
    or RECORD { line OPAQUE {optx RECORD}}
  }
  protoliteral {
    or simpleProtoliteral structuredProtoliteral
  }
  simpleProtoliteral {
    or /CHAR /INTEGER /REAL
  }
  structuredProtoliteral {
    line LBRACE {
      or
        {line VARIADIC OF {loop simpleProtoliteral ,}}
        {line structuredProtoliteral {loop {line , structuredProtoliteral} {}}}
    }
    RBRACE
  }
  requiredBinding {
    line {
      or
        {line CONST [ constBindableIdent ] : pervasiveType}
        {line procedureHeader}
    }
  }
  constBindableIdent {
    or /TSEG /TEXP
  }
  importList {
    line {
      or
        {line FROM moduleId IMPORT {
          or
            {line identList}
            {line CAST {opt , identList}}
            {line *}}}
        {line IMPORT {loop {line Ident {opt +}} ,}}
    } ;
  }
  block {
    line {loop nil {nil declaration nil}}
    {opt BEGIN statementSequence} END
  }
  declaration {
    line {
      or
        {line CONST {loop {line constantDeclaration ;} {}} }
        {line TYPE {loop {line Ident = type ;} {}} }
        {line VAR {loop {line variableDeclaration ;} {}} }
        {line procedureDeclaration ;}
    }
  }
  definition {
    line {
      or
        {line CONST {loop {line {opt [ bindableIdent ]} constantDeclaration ;} {}}}
        {line TYPE {loop {line Ident = {or type {line OPAQUE {optx recordType}}} ;} {}}}
        {line VAR {loop {line variableDeclaration ;} {}} }
        {line procedureHeader ;}
    }
  }
  constantDeclaration {
    line Ident = constExpression
  }
  type {
    line {
      or
        {line {opt {or ALIAS range} OF} namedType}
        {line enumerationType}
        {line arrayType}
        {line recordType}
        {line setType}
        {line pointerType}
        {line procedureType}
    }
  }
  range {
    line [ constExpression .. constExpression ]
  }
  enumerationType {
    line ( {loop {or {line + namedType} Ident} ,} )
  }
  arrayType {
    line {
      or
        {line ARRAY {loop constComponentCount ,}}
        {line ASSOCIATIVE ARRAY}
    }
    OF namedType
  }
  recordType {
    line RECORD {optx ( baseType )} {loop fieldList ;} END
  }
  fieldList {
   line Ident {
     or
       {line , identList :}
       {line : {optx ARRAY discriminantField OF}}
   }
   namedType
  }
  setType {
    line SET OF {
      or
        {line namedEnumType}
        {line ( identList )}
    }
  }
  pointerType {
    line POINTER TO {opt CONST} namedType
  }
  procedureType {
    line PROCEDURE {optx ( formalTypeList )} {optx : returnedType}
  }
  formalTypeList {
    loop formalType ,
  }
  formalType {
    or
      {line attributedFormalType}
      {line variadicFormalType}
  }
  attributedFormalType {
    line {or {} CONST VAR} simpleFormalType
  }
  simpleFormalType {
    line {opt {opt CAST} ARRAY OF} namedType
  }
  variadicFormalType {
    line VARIADIC OF {
      or
        {line attributedFormalType}
        {line LEFT_BRACE {loop attributedFormalType ,} RIGHT_BRACE}
    }
  }
  variableDeclaration {
      line identList : {optx ARRAY constComponentCount OF} namedType
  }
  procedureDeclaration {
    line procedureHeader ; block Ident
  }
  procedureHeader {
    stack
      {line PROCEDURE
        {optx {line [ bindableEntity ]} } }
      {line Ident {optx ( formalParamList )} {optx : returnedType}}
  }
  bindableEntity {
    or
      DIV MOD IN FOR DESCENDING :: := ? ! ~ + - * / = < > bindableIdent
  }
  bindableIdent {
    or
      /TMIN /TMAX /TLIMIT /ABS /NEG /ODD /COUNT /LENGTH
      /NEW /DISPOSE /RETAIN /RELEASE /SXF /VAL
  }
  formalParamList {
    loop formalParams ;
  }
  formalParams {
    line {or simpleFormalParams variadicFormalParams}
  }
  simpleFormalParams {
    line {or {} CONST VAR} identList : simpleFormalType
  }
  variadicFormalParams {
    stack
      {line VARIADIC
        {or {} {line [ variadicTerminator ]} }}
      {line OF
        {or simpleFormalType
          {line LEFT_BRACE {loop simpleFormalParams ;} RIGHT_BRACE}}}
  }
  variadicCounter {
    line Ident
  }
  variadicTerminator {
    line constExpression
  }
  statement {
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
  statementSequence {
    loop statement ;
  }
  assignmentOrProcedureCall {
    line designator {
      or
        {}
        {line := expression}
        {line ++}
        {line --}
        {line actualParameters}
    }
  }
  ifStatement {
    stack
      {line IF expression THEN statementSequence}
      {optx {loop {line ELSIF expression THEN statementSequence} {nil}}}
      {line {optx ELSE statementSequence} END}
  }
  caseStatement {
    stack
      {line CASE expression OF}
      {line {loop case |} {optx ELSE statementSequence} END}
  }
  altCaseStatement {
    stack
      {line CASE expression OF}
      {line {loop {line | case} {}} {optx ELSE statementSequence} END}
  }
  case {
    line {loop caseLabels ,} : statementSequence
  }
  caseLabels {
    line constExpression {optx .. constExpression}
  }
  whileStatement {
    line WHILE expression DO statementSequence END
  }
  repeatStatement {
    line REPEAT statementSequence UNTIL expression
  }
  loopStatement {
    line LOOP statementSequence END
  }
  forStatement {
    stack
      {line FOR {opt DESCENDING} controlVariable}
      {line IN {or designator {line range OF namedType}}}
      {line DO statementSequence END}
  }
  controlVariable {
    line Ident
  }
  constExpression {
    line simpleConstExpr {optx relation simpleConstExpr}
  }
  relation {
    or
      = # < <= > >= IN
  }
  simpleConstExpr {
    line {or {} + -} {loop constTerm addOp}
  }
  addOp {
    line {or + - OR}
  }
  constTerm {
    loop constFactor mulOp
  }
  mulOp {
    line {or * / DIV MOD AND}
  }
  constFactor {
    line {or
            {line {or
                    Number String
                    constQualident constStructuredValue
                    {line ( constExpression )}
                    {line CAST ( namedType , constExpression )}
                   } :: namedType }
            {line NOT constFactor}
          }
  }
  designator {
    line qualident {optx designatorTail}
  }
  designatorTail {
    line {
      loop {
        line {
          or
            {line [ expressionList ]}
            ^
        }
        {optx {loop {line . Ident} {}}}
      }
    }
  }
  designatorTail2 {
    line {
      toploop {
        line {
          or
            {line [ expressionList ]}
            ^
        }
        {optx {loop {line . Ident} {}}}
      }
    }
  }
  expressionList {
    loop expression ,
  }
  expression {
    line simpleExpression {optx relation simpleExpression}
  }
  simpleExpression {
    line {or {} + -} {loop term addOp}
  }
  term {
    loop factor mulOp
  }
  factor {
    line {or
            {line {or
                    Number String
                    structuredValue designatorOrProcedureCall
                    {line ( expression )}
                    {line CAST ( namedType , constExpression )}
                   } :: namedType }
            {line NOT factor}
          }
  }
  designatorOrProcedureCall {
    line qualident {optx designatorTail} {optx actualParameters}
  }
  actualParameters {
    line ( {optx expressionList} )
  }
  constStructuredValue {
    line LBRACE {loop constValueComponent ,} RBRACE
  }
  constValueComponent {
    line constExpression
      {optx {or BY ..} constExpression}
  }
  structuredValue {
    line LBRACE {loop valueComponent ,} RBRACE
  }
  valueComponent {
    line expression
      {optx BY constExpression}
  }
  qualident {
    loop Ident .
  }
  identList {
    loop Ident ,
  }
  constQualident {
    line qualident
  }
  compileTimeMessagePragma {
    line <* MSG = {or INFO WARN ERROR FATAL} :
      {loop compileTimeMsgComponent ,} *>
  }
  compileTimeMsgComponent {
    line {
      or
        String
        ConstQualident
        {line ? {or ALIGN ENCODING implDefinedPragmaName}}
    }
  }
  conditionalPragma {
    line <* {
      or
        {line {or IF ELSIF} inPragmaExpression}
        ELSE
        ENDIF
    } *>
  }
  pragmaENCODING {
    line <* ENCODING = {or `ASCII `UTF8} {opt : codePointSampleList} *>
  }
  codePointSampleList {
    loop {line quotedCharacterLiteral = characterCodeLiteral} ,
  }
  pragmaGENLIB {
    line <* GENLIB moduleName FROM template : templateParamList *>
  }
  templateParamList {
    loop {line placeholder = replacement} ,
  }
  pragmaFFI {
    line <* FFI = {or `C `Fortran } *>
  }
  pragmaINLINE {
    line <* {or INLINE NOINLINE} *>
  }
  pragmaALIGN {
    line <* ALIGN = inPragmaExpression *>
  }
  pragmaPADBITS {
    line <* PADBITS = inPragmaExpression *>
  }
  pragmaADDR {
    line <* ADDR = inPragmaExpression *>
  }
  pragmaREG {
    line <* REG = {or registerNumber registerMnemonic} *>
  }
  pragmaVOLATILE {
    line <* VOLATILE *>
  }
  implementationDefinedPragma {
    line <* implDefinedPragmaName {optx = inPragmaExpression} *>
  }
  implDefinedPragmaName {
    line Ident
  }
  inPragmaExpression {
    line inPragmaSimpleExpr {optx inPragmaRelation inPragmaSimpleExpr}
  }
  inPragmaRelation {
    or = # < <= > >=
  }
  inPragmaSimpleExpr {
    line {or {} + -} {loop inPragmaTerm addOp}
  }
  inPragmaTerm {
    loop inPragmaFactor inPragmaMulOp
  }
  inPragmaMulOp {
    or * DIV MOD AND
  }
  inPragmaFactor {
    or
      wholeNumber
      constQualident
      inPragmaPervasiveOrMacroCall
      {line ( inPragmaExpression )}
      {line NOT inPragmaFactor}
  }
  inPragmaPervasiveCallGeneric {
    line Ident ( {loop inPragmaExpression ,} ) 
  }
  inPragmaPervasiveOrMacroCall {
    or
      {line {or /ABS /ODD /ORD /LENGTH /TMIN /TMAX /TSIZE /TLIMIT /EXP2 /HASH}
        ( inPragmaExpression )}
      {line {or /MIN /MAX} ( {loop inPragmaExpression ,} )}
  }
  pragmaFORWARD {
    line <* FORWARD {or {line TYPE Ident} procedureHeader} *>
  }
  Ident {
    line {or _ $ Letter} {optx {loop {or _ $ Letter Digit} {}}}
  }
  Number {
    line {
      or        
        {line {loop Digit {}}
          {opt . {loop Digit {}} {opt /E {or {} + -} {loop Digit {}} }}}
        {line {loop Base2Digit {}} /B}
        {line 0b {loop Base2Digit {}}}
        {line Digit {loop Base16Digit {}} {or /H /U}}
        {line {or 0x 0u} {loop LowercaseBase16Digit {}}}
    }
  }
  String {
    or
      {line SINGLE_QUOTE
        {loop {or nil PrintableCharacter EscapeSequence DOUBLE_QUOTE} {}}
       SINGLE_QUOTE}
      {line DOUBLE_QUOTE
        {loop {or nil PrintableCharacter EscapeSequence SINGLE_QUOTE} {}}
       DOUBLE_QUOTE}
  }
  Letter {
    or /A..Z /a..z 
  }
  Digit {
    line {or 0 1 2 3 4 5 6 7 8 9}
  }
  Base2Digit {
    or 0 1
  }
  Base16Digit {
    or Digit /A /B /C /D /E /F
  }
  LowercaseBase16Digit {
    or Digit /a /b /c /d /e /f
  }
  Character {
    or Digit Letter Space
       ! # $ % & ( ) * + , - . / : ; < = > ? @ [ ] ^ _ `
       LBRACE | RBRACE ~
       EscapeSequence
  }
  Space {
    line SPACE
  }
  EscapeSequence {
    line BACKSLASH {or 0 /n /r /t BACKSLASH SINGLE_QUOTE DOUBLE_QUOTE}
  }
  Whitespace {
    or SPACE ASCII_TAB
  }
  Comment {
    or SingleLineComment MultiLineComment
  }
  SingleLineComment {
    line // {loop {or nil AnyPrintableCharacter} {}} END_OF_LINE
  }
  MultiLineComment {
    line (* {loop {or nil AnyPrintableCharacter MultiLineComment END_OF_LINE} {}} *)
  }
  AnyPrintableCharacter {
    or GraphicCharacter Whitespace
  }
  END_OF_LINE {
    or
      {line ASCII_LF {optx ASCII_CR}}
      {line ASCII_CR {optx ASCII_LF}}
  }
  AliasForIdent {
    line Ident
  }
  AliasForQualident {
    line qualident
  }
  AliasForNumber {
    line Number
  }
  AliasForWholeNumber {
    line wholeNumber
  }
  AliasForString {
    line String
  }
  AliasForConstExpr {
    line constExpression
  }
  legendReservedWord {
    line RESERVED
  }
  legendTerminal1 {
    line #
  }
  legendTerminal2 {
    line Terminal
  }
  legendIdentifier {
    line /IDENTIFIER
  }
  legendNonTerminal {
    line nonTerminal
  }
}
# end Modula-2 grammar


#  
# ===========================================================================
# C O D E   S T A R T S   H E R E
# ===========================================================================
#


# ---------------------------------------------------------------------------
# Draw the button bar
# ---------------------------------------------------------------------------
#
set bn 0
set b .bb.b$bn
button $b -text "Draw All Diagrams" -command {draw_all_graphs}
pack $b -side top -fill x -expand 1 -pady 0
#
incr bn
foreach {name graph} $all_graphs {
  incr bn
  set b .bb.b$bn
  button $b -text $name -command [list draw_graph $name $graph] -pady 0
  pack $b -side top -fill x -expand 1 -pady 0
}


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
#  Draw all productions
# ---------------------------------------------------------------------------
#
proc draw_all_graphs {} {
  global all_graphs
  set f [open all.html w]
  foreach {name graph} $all_graphs {
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
  foreach {name graph} $all_graphs {
    lappend order $name
    unset -nocomplain v
    walk_graph_extract_names $graph v
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