%% (c) 2015 by B.Kowarsch & R.Sutcliffe. All Rights Reserved.

%% DCG Grammar for Modula-2 R10


%% Grammar Predicates

:-discontiguous(production/1).
:-discontiguous(terminal/1).
:-discontiguous(alias/1).
:-discontiguous(fragment/1).


%% N O N - T E R M I N A L S

%% (1) Compilation Unit

production(compilationUnit).

compilationUnit --> ['IMPLEMENTATION'], programModule.
compilationUnit --> programModule.
compilationUnit --> definitionModule.
compilationUnit --> blueprint.


%% (2) Program Module

production(programModule).

programModule -->
  ['MODULE'], moduleIdent, semicolon,
  (importList, semicolon ; []), block, moduleIdent, dot.

%% (2.1) Module Identifier

alias(moduleIdent).

moduleIdent --> ident.

%% (2.2) Import List

fragment(importList).

importList --> importListItem.
importList --> importListItem, semicolon, importList.

fragment(importListItem).

importListItem --> (libGenDirective ; importDirective).


%% (3) Definition Module

production(definitionModule).

definitionModule -->
  ['DEFINITION'], ['MODULE'], moduleIdent,
  (lBracket, blueprintToObey, rBracket ; []),
  (['FOR'], typeToExtend, semicolon ; []),
  (importList, semicolon ; []), (definition ; []),
  ['END'], moduleIdent, dot.

%% (3.1) Blueprint To Obey

alias(blueprintToObey).

blueprintToObey --> blueprintIdent.

%% (3.2) Blueprint Identifier

alias(blueprintIdent).

blueprintIdent --> ident.

%% (3.3) Type To Extend

alias(typeToExtend).

typeToExtend --> ident.


%% (4) Blueprint

production(blueprint).

blueprint -->  
  ['BLUEPRINT'], blueprintIdent,
  (lBracket, blueprintToRefine, rBracket, semicolon ; []),
  (['FOR'], blueprintForTypeToExtend, semicolon ; []),
  (['REFERENTIAL'], identList, semicolon ; []),
  ['MODULE'], ['TYPE'], equal, (moduleTypeSpec ; ['NONE']),
  (requirementList ; []), ['END'], dot. 

fragment(requirementList).

requirementList --> requirement, semicolon.
requirementList --> requirement, requirementList, semicolon.

%% (4.1) Blueprint To Refine

alias(blueprintToRefine).

blueprintToRefine --> blueprintIdent.

%% (4.2) Blueprint For Type To Extend

alias(blueprintForTypeToExtend).

blueprintForTypeToExtend --> ident.


%% (5) Identifier List

production(identList).

identList --> ident.
identList --> ident, comma, identList.


%% (6) Module Type Specification

production(moduleTypeSpec).

moduleTypeSpec -->
  (['OPAQUE'] ; ['RECORD']),
  (semicolon, propertySpec ; []), (semicolon, literalSpec ; []).


%% (7) Property Specification

production(propertySpec).

propertySpec --> ['TPROPERTIES'], equal, propertySpecTail.

fragment(propertySpecTail).

propertySpecTail --> determinedProperties.
propertySpecTail --> determinedProperties, verticalBar, propertiesToDetermine.

%% (7.1) Determined Properties

alias(determinedProperties).

determinedProperties --> identList.

%% (7.2) Properties To Determine

fragment(propertiesToDetermine).

propertiesToDetermine --> ident, questionMark.
propertiesToDetermine --> ident, questionMark, comma, propertiesToDetermine.


%% (8) Literal Specification

production(literalSpec).

literalSpec --> ['TLITERAL'], equal, protoLiteralList.

fragment(protoLiteralList).

protoLiteralList --> protoLiteral.
protoLiteralList --> protoLiteral, verticalBar, protoLiteralList.

%% (8.1) Proto Literal

fragment(protoLiteral).

protoLiteral --> simpleProtoLiteral.
protoLiteral --> structuredProtoLiteral.

%% (8.2) Simple Proto Literal

fragment(simpleProtoLiteral).

simpleProtoLiteral --> literalOrReferential.

%% (8.3) Literal Identifier Or Referential Identifier

alias(literalOrReferential).

literalOrReferential --> ident.


%% (9) Structured Proto Literal

production(structuredProtoLiteral).

structuredProtoLiteral -->
  lBrace, (variadicProtoLiteralList ; simpleProtoLiteralList), rBrace.

fragment(variadicProtoLiteralList).

variadicProtoLiteralList -->
  ['ARGLIST'], (reqValueCount ; []), ['OF'],
  (lBrace, simpleProtoLiteralList, rBrace ; simpleProtoLiteral).

fragment(simpleProtoLiteralList).

simpleProtoLiteralList --> simpleProtoLiteral.
simpleProtoLiteralList --> simpleProtoLiteral, comma, simpleProtoLiteralList.

%% (9.1) Required Value Count

fragment(reqValueCount).

reqValueCount --> wholeNumber.
reqValueCount --> greaterThan, wholeNumber.

%% (9.2) Greater Than

alias(greaterThan).

greaterThan --> greater.

%% (9.3) Whole Number

alias(wholeNumber).

wholeNumber --> numberLiteral.


%% (10) Requirement

production(requirement).

requirement -->
  (condition, minusArrow ; []),
  (constRequirement ; procedureRequirement ; ['TYPE'], equal, procedureType).

%% (10.1) Condition

fragment(condition).

condition --> (['NOT']; []), boolConstIdent.

%% (10.1) Boolean Constant Identifier

alias(boolConstIdent).

boolConstIdent --> ident.


%% (11) Constant Requirement

production(constRequirement).

constRequirement --> ['CONST'], constRequirementTail.

fragment(constRequirementTail).

constRequirementTail -->
  lBracket, propertyToBindTo, rBracket,
  (simpleConstRequirement ; equal, ['NONE'] ; []).

constRequirementTail --> 
  (constAttribute ; []), simpleConstRequirement.

%% (11.1) Property To Bind To

fragment(propertyToBindTo).

propertyToBindTo -->  memMgtProperty.
propertyToBindTo -->  scalarProperty.
propertyToBindTo -->  collectionProperty.

%% (11.2) Memory Management Property

fragment(memMgtProperty).

memMgtProperty --> ['TDYN'].
memMgtProperty --> ['TREFC'].

%% (11.3) Scalar Property

fragment(scalarProperty).

scalarProperty --> ['TBASE'].
scalarProperty --> ['TPRECISION'].
scalarProperty --> ['TMINEXP'].
scalarProperty --> ['TMAXEXP'].

%% (11.4) Collection Property

fragment(collectionProperty).

collectionProperty --> ['TNIL'].
collectionProperty --> ['TLIMIT'].

%% (11.5) Constant Attribute

fragment(constAttribute).

constAttribute -->  ancillaryConstant.
constAttribute -->  restrictedExport.

%% (11.5) Constant Attribute

fragment(ancillaryConstant).

alias --> minus.

%% (11.6) Restricted Export

alias(restrictedExport).

restrictedExport --> asterisk.


%% (12) Simple Constant Requirement

production(simpleConstRequirement).

simpleConstRequirement -->
  ident, (equal, constExpression ; colon, predefOrRefTypeIdent).

%% (12.1) Constant Expression

alias(constExpression).

constExpression --> expression.

%% (12.2) Predefined Or Referential Type Identifier

alias(predefOrRefTypeIdent).

predefOrRefTypeIdent --> ident.


%% (13) Procedure Requirement

production(procedureRequirement).

procedureRequirement --> ['PROCEDURE'], procedureRequirementTail.

fragment(procedureRequirementTail).

procedureRequirementTail -->
  lBracket, entityToBindTo, rBracket,
  (procedureSignature ; equal, ['NONE'] ; []).

procedureRequirementTail -->
  (restrictedExport ; []), procedureSignature.

%% (13.1) Entity To Bind To

fragment(entityToBindTo).

entityToBindTo -->  bindableResWord.
entityToBindTo -->  bindableOperator.
entityToBindTo -->  bindableMacro.

%% (13.2) Bindable Reserved Word

fragment(bindableResWord).

bindableResWord --> ['NEW'].
bindableResWord --> ['RETAIN'].
bindableResWord --> ['RELEASE'].
bindableResWord --> ['COPY'].
bindableResWord --> ['FOR'].

%% (13.3) Bindable Operator

fragment(bindableOperator).

bindableOperator --> plus.
bindableOperator --> minus.
bindableOperator --> asterisk.
bindableOperator --> slash.
bindableOperator --> backslash.
bindableOperator --> equal.
bindableOperator --> less.
bindableOperator --> greater.
bindableOperator --> starDot.
bindableOperator --> colonColon.
bindableOperator --> ['IN'].
bindableOperator --> ['DIV'].
bindableOperator --> ['MOD'].
bindableOperator --> unaryMinus.

%% (13.4) Unary Minus

fragment(unaryMinus).

unaryMinus --> plus, slash, minus.

%% (13.5) Bindable Macro

fragment(bindableMacro).

bindableMacro --> ['ABS'].
bindableMacro --> ['LENGTH'].
bindableMacro --> ['EXISTS'].
bindableMacro --> ['SUBSET'].
bindableMacro --> ['READ'].
bindableMacro --> ['READNEW'].
bindableMacro --> ['WRITE'].
bindableMacro --> ['WRITEF'].
bindableMacro --> ['TMIN'].
bindableMacro --> ['TMAX'].
bindableMacro --> ['SXF'].
bindableMacro --> ['VAL'].
bindableMacro --> multiBindableMacro1.
bindableMacro --> multiBindableMacro2.

%% (13.6) Multi-Bindable Macro 1

fragment(multiBindableMacro1).

multiBindableMacro1 -->
  (['COUNT'] ; ['RETRIEVE']), (bindingDifferentiator1 ; []).

%% (13.7) Binding Differentiatior 1

fragment(bindingDifferentiator1).

bindingDifferentiator1 --> verticalBar, octothorpe.

%% (13.8) Multi-Bindable Macro 2

fragment(multiBindableMacro2).

multiBindableMacro2 -->
  (['STORE'] ; ['INSERT'] ; ['REMOVE']), (bindingDifferentiator2 ; []).

%% (13.9) Binding Differentiatior 2

fragment(bindingDifferentiator2).

bindingDifferentiator2 --> verticalBar, (comma ; octothorpe ; asterisk).


%% (14) Library Generation Directive

production(libGenDirective).

libGenDirective --> 
  ['GENLIB'], libIdent, ['FROM'], template, ['FOR'], templateParams, ['END'].

fragment(templateParams).

templateParams --> placeholder, equal, replacement.
templateParams --> placeholder, equal, replacement, semicolon, templateParams.

%% (14.1) Library Identifier

alias(libIdent).

libIdent --> ident.

%% (14.2) Template

alias(template).

template --> ident.

%% (14.3) Placeholder

alias(placeholder).

placeholder --> ident.

%% (14.4) Replacement

fragment(replacement).

replacement --> numberLiteral.
replacement --> stringLiteral.
replacement --> chevronText.


%% (15) Import Directive

production(importDirective).

importDirective -->
  (qualifiedImport ; unqualifiedImport).

fragment(qualifiedImport).

qualifiedImport -->
  ['IMPORT'], modulesToImport.

fragment(unqualifiedImport).

unqualifiedImport -->
  ['FROM'], modulesOrEnumIdent, ['IMPORT'], (identifiersToImport ; importAll).

%% (15.1) Modules To Import

alias(modulesToImport).

modulesToImport --> identifiersToImport.

%% (15.2) Module Or Enumeration Identifier

alias(modulesOrEnumIdent).

modulesOrEnumIdent --> ident.

%% (15.3) Identifiers To Import

fragment(identifiersToImport).

identifiersToImport --> ident.
identifiersToImport --> ident, reExport.
identifiersToImport --> ident, reExport, comma, identifiersToImport.

%% (15.4) Re-Export

alias(reExport).

reExport --> plus.

%% (15.5) Re-Export

alias(importAll).

importAll --> asterisk.


%% (16) Block

production(block).

block -->
  (declarationList ; []),
  (['BEGIN'], statementSequence, ['END'] ; []), ['END'].

fragment(declarationList).

declarationList --> declaration.
declarationList --> declaration, declarationList.


%% (17) Statement Sequence

production(statementSequence).

statementSequence --> statement.
statementSequence --> statement, semicolon, statementSequence.


%% (18) Definition

production(definition).

definition --> ['CONST'], constDefinitionList.
definition --> ['TYPE'], typeDefinitionList.
definition --> ['VAR'], varDeclarationList.
definition --> procedureHeader, semicolon.

fragment(constDefinitionList).

constDefinitionList --> constDefinition, semicolon.
constDefinitionList --> constDefinition, semicolon, constDefinitionList.

fragment(typeDefinitionList).

typeDefinitionList --> typeDefinition, semicolon.
typeDefinitionList --> typeDefinition, semicolon, typeDefinitionList.

fragment(varDeclarationList).

varDeclarationList --> variableDeclaration, semicolon.
varDeclarationList --> variableDeclaration, semicolon, varDeclarationList.


%% (19) Constant Definition

production(constDefinition).

constDefinition -->
  (lBracket, propertyToBindTo, rBracket ; restrictedExport ; []),
  constDeclaration.

fragment(constDeclaration).

constDeclaration --> ident, equal, constExpression.


%% (20) Variable Declaration

production(variableDeclaration).

variableDeclaration -->
  identList, colon, (range, ['OF'] ; []), typeIdent.


%% (21) Declaration

production(declaration).

declaration --> ['CONST'], constDeclarationList.
declaration --> ['TYPE'], typeDeclarationList.
declaration --> ['VAR'], varDeclarationList.
declaration --> procedureHeader, semicolon, block, ident, semicolon.

fragment(constDeclarationList).

constDeclarationList --> constDeclaration, semicolon.
constDeclarationList --> constDeclaration, semicolon, constDeclarationList.

fragment(typeDeclarationList).

typeDeclarationList --> typeDeclaration, semicolon.
typeDeclarationList --> typeDeclaration, semicolon, typeDeclarationList.

fragment(typeDeclaration).

typeDeclaration --> ident, equal, type, semicolon.


%% (22) Type

production(type).

type --> typeIdent.
type --> derivedSubType.
type --> enumType.
type --> setType.
type --> arrayType.
type --> recordType.
type --> pointerType.
type --> procedureType.

%% (22.1) Type Identifier

alias(typeIdent).

typeIdent --> qualident.

%% (22.2) Derived Sub-Type

fragment(derivedSubType).

derivedSubType --> ['ALIAS'], ['OF'], typeIdent.
derivedSubType --> range, ['OF'], ordinalOrScalarType.
derivedSubType --> ['CONST'], dynamicTypeIdent.

%% (22.3) Ordinal Or Scalar Type

alias(ordinalOrScalarType).

ordinalOrScalarType --> typeIdent.

%% (22.4) Dynamic Type Identifier

alias(dynamicTypeIdent).

dynamicTypeIdent --> typeIdent.


%% (23) Range

production(range).

range -->
  lBracket, (greaterThan ; []), constExpression,
  dotDot, (lessThan ; []), constExpression, rBracket.

%% (23.1) Greater Than

%% see alias #9.2

%% (23.2) Less Than

alias(lessThan).

lessThan --> less.


%% (24) Enumeration Type

production(enumType).

enumType --> lParen, (plus, enumTypeToExtend ; []), identList, rParen.

%% (24.1) Enumeration Type To Extend

alias(enumTypeToExtend).

enumTypeToExtend --> enumTypeIdent.

%% (24.2) Enumeration Type Identifier

alias(enumTypeIdent).

enumTypeIdent --> typeIdent.


%% (25) Set Type

production(setType).

setType --> ['SET'], ['OF'], enumTypeIdent.


%% (26) Array Type

production(arrayType).

arrayType --> ['ARRAY'], (componentCountList ; []), ['OF'], typeIdent.

fragment(componentCountList).

componentCountList --> componentCount.
componentCountList --> componentCount, comma, componentCountList.

%% (26.1) Component Count

alias(componentCount).

componentCount --> constExpression.


%% (27) Record Type

production(recordType).

recordType -->
  ['RECORD'], fieldListSequence, (indeterminateField ; []), ['END'].
recordType -->
  ['RECORD'], lParen, recTypeToExtend, rParen, fieldListSequence, ['END'].

fragment(fieldListSequence).

fieldListSequence --> fieldList.
fieldListSequence --> fieldList, semicolon, fieldListSequence.

%% (27.1) Record Type To Extend

alias(recTypeToExtend).

recTypeToExtend --> typeIdent.

%% (27.2) Field List

fragment(fieldList).

fieldList --> (restrictedExport ; []), variableDeclaration.


%% (28) Indeterminate Field

production(indeterminateField).

indeterminateField --> 
  ['INDETERMINATE'], ident, colon,
  ['ARRAY'], discriminantFieldIdent, ['OF'], typeIdent.

%% (28.1) Discriminant Field Identifier

alias(discriminantFieldIdent).

discriminantFieldIdent --> ident.


%% (29) Pointer Type

production(pointerType).

pointerType -->
  ['POINTER'], ['TO'], (['CONST'] ; []), typeIdent.


%% (30) Procedure Type

production(procedureType).

procedureType -->
  ['PROCEDURE'],
  (lParen, formalTypeList, rParen ; []), (colon, returnedType ; []).

fragment(formalTypeList).

formalTypeList --> formalType.
formalTypeList --> formalType, comma, formalTypeList.

%% (30.1) Formal Type

fragment(formalType).

formalType --> simpleFormalType.
formalType --> attributedFormalType.
formalType --> variadicFormalType.

%% (30.2) Returned Type

alias(returnedType).

returnedType --> typeIdent.


%% (31) Simple Formal Type

production(simpleFormalType).

simpleFormalType --> (['ARRAY'], ['OF'] ; []), typeIdent.
simpleFormalType --> castingFormalType.

%% (31.1) Casting Formal Type

fragment(castingFormalType).

castingFormalType -->
  ['CAST'],
  (['ARRAY'], ['OF'], ['OCTET'] ; addressTypeIdent).

%% (31.2) Address Type Identifier

fragment(addressTypeIdent).

addressTypeIdent -->
  (['ADDRESS'] ; ['UNSAFE'], ['.'], ['ADDRESS']).


%% (32) Attributed Formal Type

production(attributedFormalType).

attributedFormalType -->
  (['CONST'] ; ['VAR'] ;['NEW']),
  (simpleFormalType, simpleVariadicFormalType).


%% (33) Simple Variadic Formal Type

production(simpleVariadicFormalType).

simpleVariadicFormalType -->
  ['ARGLIST'], (numOfArgsToPass ; []), ['OF'],
  simpleFormalType, (verticalBar, arglistTerminator ; []).
  
%% (33.1) Number Of Arguments To Pass

fragment(numOfArgsToPass).

numOfArgsToPass --> (greaterThan ; []), constExpression.

%% (33.2) Variadic Argument List Terminator

fragment(arglistTerminator).

arglistTerminator --> ['NIL'].
arglistTerminator --> minusOne.
arglistTerminator --> wholeNumber.
arglistTerminator --> constQualident.

%% (33.3) Minus One (Negative One)

fragment(minusOne).

minusOne --> minus, digitOne.

%% (33.4) Constant Qualified Identifier

alias(constQualident).

constQualident --> qualident.


%% (34) Variadic Formal Type

production(variadicFormalType).

variadicFormalType --> 
  ['ARGLIST'], (numOfArgsToPass ; []), ['OF'],
  (lBrace, nonVariadicFormalTypeList, rBrace ; simpleFormalType),
  (verticalBar, arglistTerminator ; []).


%% (35) Non-Variadic Formal Type

production(nonVariadicFormalType).

nonVariadicFormalType -->
  (['CONST'] ; ['VAR'] ; ['NEW'] ; []), simpleFormalType.


%% (36) Procedure Header

production(procedureHeader).

procedureHeader -->
  ['PROCEDURE'], (lBracket, entityToBindTo, rBracket ; restrictedExport ; []),
  procedureSignature.


%% (37) Procedure Signature

production(procedureSignature).

procedureSignature -->
  ident, (lParen, formalParamsList, rParen ; []), (colon, returnedType ; []). 

fragment(formalParamsList).

formalParamsList --> formalParams.
formalParamsList --> formalParams, semicolon, formalParamsList.


%% (38) Formal Parameters

production(formalParams).

formalParams -->
  identList, colon, (simpleFormalType ; variadicFormalParams).
  
formalParams --> attributedFormalParams.


%% (39) Attributed Formal Parameters

production(attributedFormalParams).

attributedFormalParams --> 
  (['CONST'], ['VAR'], ['NEW']),
  identList, colon, (simpleFormalType ; simpleVariadicFormalType).


%% (40) Variadic Formal Parameters

production(variadicFormalParams).

variadicFormalParams --> 
  ['ARGLIST'], (numOfArgsToPass ; []), ['OF'],
  (lBrace, nonVariadicFormalParamsList, rBrace ; simpleFormalType),
  (verticalBar, arglistTerminator ; []).

fragment(nonVariadicFormalParamsList).

nonVariadicFormalParamsList -->
  nonVariadicFormalParams.
nonVariadicFormalParamsList --> 
  nonVariadicFormalParams, semicolon, nonVariadicFormalParamsList.


%% (41) Non-Variadic Formal Parameters

production(nonVariadicFormalParams).

nonVariadicFormalParams --> 
  (['CONST'], ['VAR'], ['NEW'] ; []), identList, colon, simpleFormalType.


%% (42) Qualified Identifier

production(qualident).

qualident --> ident.
qualident --> ident, dot, qualident.


%% (43) Statement

production(statement).

statement --> memMgtOperation.
statement --> updateOrProcCall.
statement --> ifStatement.
statement --> caseStatement.
statement --> loopStatement.
statement --> whileStatement.
statement --> repeatStatement.
statement --> forStatement.
statement --> ['RETURN'], (expression ; []).
statement --> ['EXIT'].


%% (44) Memory Management Operation

production(memMgtOperation).

memMgtOperation -->
  ['NEW'], designator, (['OF'], initSize ; assign, initValue).
memMgtOperation -->
  (['RETAIN'] ; ['RELEASE']), designator.

%% (44.1) Initial Size

alias(initSize).

initSize --> expression.

%% (44.2) Initial Value

alias(initValue).

initValue --> expression.


%% (45) Update Or Procedure Call

production(updateOrProcCall).

updateOrProcCall -->
  designator, (assign, expression ; incOrDecSuffix ; actualParameters ; []).
updateOrProcCall -->
  ['COPY'], designator, assign, expression.

%% (45.1) Increment Or Decrement Suffix

fragment(incOrDecSuffix).

incOrDecSuffix --> plusPlus.
incOrDecSuffix --> minusMinus.


%% (46) IF statement

production(ifStatement).

ifStatement -->
  ['IF'], boolExpression, ['THEN'], statementSequence,
  (elsifBranchList ; []), (['ELSE'], statementSequence ; []), ['END'].

fragment(elsifBranchList).

elsifBranchList --> elsifBranch.
elsifBranchList --> elsifBranch, elsifBranchList.

fragment(elsifBranch).

elsifBranch --> ['ELSIF'], boolExpression, ['THEN'], statementSequence.


%% (46.1) Boolean Expression

alias(boolExpression).

boolExpression --> expression.


%% (47) CASE statement

production(caseStatement).

caseStatement -->
  ['CASE'], expression, ['OF'], caseList,
  (['ELSE'], statementSequence ; []), ['END'].

fragment(caseList).

caseList --> verticalBar, case.
caseList --> verticalBar, case, caseList.


%% (48) Case

production(case).

case --> caseLabelList, colon, statementSequence.

fragment(caseLabelList).

caseLabelList --> caseLabels.
caseLabelList --> caseLabels, comma, caseLabelList.


%% (49) Case Labels

production(caseLabels).

caseLabels --> constExpression, (dotDot, constExpression ; []).


%% (50) LOOP Statement

production(loopStatement).

loopStatement --> ['LOOP'], statementSequence, ['END'].


%% (51) WHILE Statement

production(whileStatement).

whileStatement --> 
  ['WHILE'], boolExpression, ['DO'], statementSequence, ['END'].


%% (52) REPEAT Statement

production(repeatStatement).

repeatStatement --> 
  ['REPEAT'], statementSeqence, ['UNTIL'], boolExpression, ['END'].


%% (53) FOR Statement

production(forStatement).

forStatement -->
  ['FOR'], controlVariable, (ascendOrDescend ; []),
  ['IN'], (designator ; range, ['OF'], ordinalType), 
  ['DO'], statementSequence, ['END'].

%% (53.1) Control Variable

alias(controlVariable).

controlVariable --> ident.

%% (53.2) Ascend Or Descend

alias(ascendOrDescend).

ascendOrDescend --> incOrDecSuffix.

%% (53.3) Ordinal Type

alias(ordinalType).

ordinalType --> typeIdent.


%% (54) Designator

production(designator).

designator --> qualident, (designatorTail ; []).


%% (55) Designator Tail

production(designatorTail).

designatorTail --> designatorTailBody.
designatorTail --> designatorTailBody, designatorTail.

designatorTailBody --> bracketExprOrCaret, (qualidentTail ; []).

fragment(bracketExprOrCaret).

bracketExprOrCaret --> (lBracket, exprListOrSlice, rBracket ; caret).

fragment(qualidentTail).

qualidentTail --> dot, ident.
qualidentTail --> dot, ident, qualidentTail.


%% (56) Expression List Or Slice

production(exprListOrSlice).

exprListOrSlice --> expression, (expressionListTail ; []).
exprListOrSlice --> expression, dotDot, (expression ; []).

fragment(expressionListTail).

expressionListTail --> comma, expression.
expressionListTail --> comma, expression, expressionListTail.


%% (57) Expression

production(expression).

expression --> simpleExpression.
expression --> simpleExpression, operL1, simpleExpression.

%% (57.1) Level-1 Operator

fragment(operL1).

operL1 --> equal.
operL1 --> octothorpe.
operL1 --> less.
operL1 --> notGreater.
operL1 --> greater.
operL1 --> notLess.
operL1 --> ['IN'].
operL1 --> identityOp.
operL1 --> arrayConcatOp.

%% (57.2) Identity Operator

alias(identityOp).

identityOp --> equalEqual.

%% (57.4) Array Concatenation Operator

alias(arrayConcatOp).

arrayConcatOp --> plusArrow.


%% (58) Simple Expression

production(simpleExpression).

simpleExpression --> termList.
simpleExpression --> sign, termList.

fragment(termList).

termList --> term.
termList --> term, operL2, termList.

fragment(sign).

sign --> plus.
sign --> minus.

%% (58.1) Level-2 Operator

fragment(operL2).

operL2 --> plus.
operL2 --> minus.
operL2 --> ['OR'].


%% (59) Term

production(term).

term --> factorOrNegation.
term --> factorOrNegation, operL3, term.

%% (59.1) Level-3 Operator

fragment(operL3).

operL3 --> asterisk.
operL3 --> slash.
operL3 --> ['DIV'].
operL3 --> ['MOD'].
operL3 --> ['AND'].
operL3 --> setDiffOp.
operL3 --> dotProductOp.

%% (59.2) Set Difference Operator

alias(setDiffOp).

setDiffOp --> backslash.

%% (59.3) Dot Product Operator

alias(dotProductOp).

dotProductOp --> starDot.


%% (60) Factor Or Negation

production(factorOrNegation).

factorOrNegation --> (['NOT'] ; []), factorOrTypeConv.


%% (61) Factor Or Type Conversion

production(factorOrTypeConv).

factorOrTypeConv --> factor, (['::'], typeIdent ; []).


%% (62) Factor

production(factor).

factor --> numberLiteral.
factor --> stringLiteral.
factor --> structuredValue.
factor --> lParen, expression, rParen.
factor --> designator, (actualParameters ; []).


%% (63) Actual Parameters

production(actualParameters).

actualParameters --> lParen, (expressionList ; []), rParen.


%% (64) Expression List

production(expressionList).

expressionList --> expression.
expressionList --> expression, comma, expressionList.


%% (65) Structured Value

production(structuredValue).

structuredValue --> lBrace, valueComponentList, rBrace.

fragment(valueComponentList).

valueComponentList --> valueComponent.
valueComponentList --> valueComponent, comma, valueComponentList.


%% (66) Value Component

production(valueComponent).

valueComponent --> constExpression, ((['BY'] ; dotDot ), constExpression ; []).
valueComponent --> runtimeExpression.

%% (66.1) Runtime Expression

alias(runtimeExpression).

runtimeExpression --> expression.


%% T E R M I N A L S

%% Special Symbols

terminal(dot).

dot --> ['.'].

terminal(comma).

comma --> [','].

terminal(colon).

colon --> [':'].

terminal(semicolon).

semicolon --> [';'].

terminal(verticalBar).

verticalBar --> ['|'].

terminal(caret).

caret --> ['^'].

terminal(dotDot).

dotDot --> ['..'].

terminal(assign).

assign --> [':='].

terminal(plusPlus).

plusPlus --> ['++'].

terminal(minusMinus).

minusMinus --> ['--'].

terminal(plus).

plus --> ['+'].

terminal(plusArrow).

plusArrow --> ['+>'].

terminal(minus).

minus --> ['-'].

terminal(minusArrow).

minusArrow --> ['->'].

terminal(asterisk).

asterisk --> ['*'].

terminal(starDot).

starDot --> ['*.'].

terminal(solidus).

slash --> ['/'].

terminal(backslash).

backslash --> ['\\'].

terminal(colonColon).

colonColon --> ['::'].

terminal(equal).

equal --> ['='].

terminal(octothorpe).

octothorpe --> ['#'].

alias(notEqual).

notEqual --> octothorpe.

terminal(greater).

greater --> ['>'].

terminal(notLess).

notLess --> ['>='].

terminal(less).

less --> ['<'].

terminal(notGreater).

notGreater --> ['<='].

terminal(equalEqual).

equalEqual --> ['=='].

terminal(questionMark).

questionMark --> ['?'].

terminal(tilde).

tilde --> ['~'].

terminal(lParen).

lParen --> ['('].

terminal(rParen).

rParen --> [')'].

terminal(lBracket).

lBracket --> ['['].

terminal(rBracket).

rBracket --> [']'].

terminal(lBrace).

lBrace --> ['{'].

terminal(rBrace).

rBrace --> ['}'].


%% Identifier

terminal(ident).

ident --> identHead, identTail.

fragment(identHead).

identHead --> letter.
identHead --> foreignIdentCharSequence, letterOrDigitSequence.

fragment(foreignIdentCharSequence).

foreignIdentCharSequence --> foreignIdentChar.
foreignIdentCharSequence --> foreignIdentChar, foreignIdentCharSequence.

fragment(letterOrDigitSequence).

letterOrDigitSequence --> letterOrDigit.
letterOrDigitSequence --> letterOrDigit, letterOrDigitSequence.

fragment(identTail).

identTail --> identTailChar.
identTail --> identTailChar, identTail.

fragment(foreignIdentChar).

foreignIdentChar --> ['_'].
foreignIdentChar --> ['$'].

fragment(letterOrDigit).

letterOrDigit --> letter.
letterOrDigit --> digit.

fragment(identTailChar).

identTailChar --> letterOrDigit, foreignIdentChar.


%% Number Literal

terminal(numberLiteral).

numberLiteral --> digitZero.
numberLiteral --> digitZero, realNumberTail.
numberLiteral --> digitZero, ['b'], base2DigitSeq.
numberLiteral --> digitZero, ['x'], base16DigitSeq.
numberLiteral --> digitZero, ['u'], base16DigitSeq.
numberLiteral --> digitOneToNine.
numberLiteral --> digitOneToNine, decimalNumberTail.

fragment(digitZero).

digitZero --> ['0'].

fragment(digitOne).

digitOne --> ['1'].

fragment(digitOneToNine).

digitOneToNine --> digitOne.
digitOneToNine --> ['2'].
digitOneToNine --> ['3'].
digitOneToNine --> ['4'].
digitOneToNine --> ['5'].
digitOneToNine --> ['6'].
digitOneToNine --> ['7'].
digitOneToNine --> ['8'].
digitOneToNine --> ['9'].

%% Decimal Number Tail

fragment(decimalNumberTail).

decimalNumberTail --> digitSeq.
decimalNumberTail --> digitSeq, realNumberTail.
decimalNumberTail --> singleQuote, digitSeq.
decimalNumberTail --> singleQuote, digitSeq, realNumberTail.
decimalNumberTail --> realNumberTail.

%% Real Number Tail

fragment(realNumberTail).

realNumberTail --> dot, digitSeq.
realNumberTail --> dot, digitSeq, exponent.

fragment(exponent).

exponent --> ['e'], digitSeq.
exponent --> ['e'], sign, digitSeq.

%% Digit Sequence

fragment(digitSeq).

digitSeq --> digitGroup.
digitSeq --> digitGroup, singleQuote, digitSeq.

%% Digit Group

fragment(digitGroup).

digitGroup --> digit.
digitGroup --> digit, digitGroup.

%% Digit

fragment(digit).

digit --> digitZero.
digit --> digitOneToNine.

%% Base-16 Digit Sequence

fragment(base16DigitSeq).

base16DigitSeq --> base16DigitGroup.
base16DigitSeq --> base16DigitGroup, singleQuote, base16DigitSeq.

%% Base-16 Digit Group

fragment(base16DigitGroup).

base16DigitGroup --> base16Digit.
base16DigitGroup --> base16Digit, base16DigitGroup.

%% Base-16 Digit

fragment(base16Digit).

base16Digit --> digit.
base16Digit --> ['A'].
base16Digit --> ['B'].
base16Digit --> ['C'].
base16Digit --> ['D'].
base16Digit --> ['E'].
base16Digit --> ['F'].

%% Base-2 Digit Sequence

fragment(base2DigitSeq).

base2DigitSeq --> base2DigitGroup.
base2DigitSeq --> base2DigitGroup, singleQuote, base2DigitSeq.

%% Base-2 Digit Group

fragment(base2DigitGroup).

base2DigitGroup --> base2Digit.
base2DigitGroup --> base2Digit, base2DigitGroup.

%% Base-2 Digit

fragment(base2Digit).

base2Digit --> digitZero.
base2Digit --> digitOne.


%% String Literal

terminal(stringLiteral).

stringLiteral --> singleQuotedString.
stringLiteral --> doubleQuotedString.

%% Single Quoted String

fragment(singleQuotedString).

singleQuotedString --> singleQuote, singleQuote.
singleQuotedString --> singleQuote, singleQuotedCharSeq, singleQuote.

fragment(singleQuotedCharSeq).

singleQuotedCharSeq --> singleQuotedChar.
singleQuotedCharSeq --> singleQuotedChar, singleQuotedCharSeq.

fragment(singleQuotedChar).

singleQuotedChar --> doubleQuote.
singleQuotedChar --> quotableCharacter.

%% Double Quoted String

fragment(doubleQuotedString).

doubleQuotedString --> doubleQuote, doubleQuote.
doubleQuotedString --> doubleQuote, doubleQuotedCharSeq, doubleQuote.

fragment(doubleQuotedCharSeq).

doubleQuotedCharSeq --> doubleQuotedChar.
doubleQuotedCharSeq --> doubleQuotedChar, doubleQuotedCharSeq.

fragment(doubleQuotedChar).

doubleQuotedChar --> singleQuote.
doubleQuotedChar --> quotableCharacter.

%% Quotable Character

fragment(quotableCharacter).

quotableCharacter --> digit.
quotableCharacter --> letter.
quotableCharacter --> space.
quotableCharacter --> nonAlphaNumQuotable.
quotableCharacter --> escapedCharacter.

%% Letter

fragment(letter).

letter --> ['a'].
letter --> ['b'].
letter --> ['c'].
letter --> ['d'].
letter --> ['e'].
letter --> ['f'].
letter --> ['g'].
letter --> ['h'].
letter --> ['i'].
letter --> ['j'].
letter --> ['k'].
letter --> ['l'].
letter --> ['m'].
letter --> ['n'].
letter --> ['o'].
letter --> ['p'].
letter --> ['q'].
letter --> ['r'].
letter --> ['s'].
letter --> ['t'].
letter --> ['u'].
letter --> ['v'].
letter --> ['w'].
letter --> ['x'].
letter --> ['y'].
letter --> ['z'].
letter --> ['A'].
letter --> ['B'].
letter --> ['C'].
letter --> ['D'].
letter --> ['E'].
letter --> ['F'].
letter --> ['G'].
letter --> ['H'].
letter --> ['I'].
letter --> ['J'].
letter --> ['K'].
letter --> ['L'].
letter --> ['M'].
letter --> ['N'].
letter --> ['O'].
letter --> ['P'].
letter --> ['Q'].
letter --> ['R'].
letter --> ['S'].
letter --> ['T'].
letter --> ['U'].
letter --> ['V'].
letter --> ['W'].
letter --> ['X'].
letter --> ['Y'].
letter --> ['Z'].

%% Space

fragment(space).

space --> [' '].

%% Non-Alphanumeric Quotable Character

fragment(nonAlphaNumQuotable).

nonAlphaNumQuotable --> ['!'].
nonAlphaNumQuotable --> ['#'].
nonAlphaNumQuotable --> ['$'].
nonAlphaNumQuotable --> ['%'].
nonAlphaNumQuotable --> ['&'].
nonAlphaNumQuotable --> ['('].
nonAlphaNumQuotable --> [')'].
nonAlphaNumQuotable --> ['*'].
nonAlphaNumQuotable --> ['+'].
nonAlphaNumQuotable --> [','].
nonAlphaNumQuotable --> ['-'].
nonAlphaNumQuotable --> ['.'].
nonAlphaNumQuotable --> ['/'].
nonAlphaNumQuotable --> [':'].
nonAlphaNumQuotable --> ['<'].
nonAlphaNumQuotable --> ['='].
nonAlphaNumQuotable --> ['>'].
nonAlphaNumQuotable --> ['?'].
nonAlphaNumQuotable --> ['@'].
nonAlphaNumQuotable --> ['['].
nonAlphaNumQuotable --> [']'].
nonAlphaNumQuotable --> ['^'].
nonAlphaNumQuotable --> ['_'].
nonAlphaNumQuotable --> ['`'].
nonAlphaNumQuotable --> ['{'].
nonAlphaNumQuotable --> ['|'].
nonAlphaNumQuotable --> ['}'].
nonAlphaNumQuotable --> ['~'].

%% Escaped Character

fragment(escapedCharacter).

escapedCharacter --> backslash, ['n'].
escapedCharacter --> backslash, ['t'].
escapedCharacter --> backslash, backslash.

%% Chevron Delimited Text

terminal(chevronText).

chevronText --> ['<<'], ['>>'].
chevronText --> ['<<'], chevronQuotedCharSeq, ['>>'].

fragment(chevronQuotedCharSeq).

chevronQuotedCharSeq --> chevronQuotedChar.
chevronQuotedCharSeq --> chevronQuotedChar, chevronQuotedCharSeq.

fragment(chevronQuotedChar).

chevronQuotedChar --> singleQuote.
chevronQuotedChar --> doubleQuote.
chevronQuotedChar --> quotableCharacter.

%% Character Synonyms

fragment(singleQuote).

singleQuote --> ['\''].

fragment(doubleQuote).

doubleQuote --> ['"'].


%% end of grammar