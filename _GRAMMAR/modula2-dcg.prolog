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

programModule --> ['MODULE'], moduleIdent, [';'], block, moduleIdent, ['.'].
programModule -->
  ['MODULE'], moduleIdent, [';'], importList, [';'], block, moduleIdent, ['.'].

%% (2.1) Module Identifier

alias(moduleIdent).

moduleIdent --> ident.

%% (2.2) Import List

fragment(importList).

importList --> libGenDirective.
importList --> libGenDirective, [';'], importList.
importList --> importDirective.
importList --> importDirective, [';'], importList.


%% (3) Definition Module

production(definitionModule).

definitionModule --> definitionModuleHeader, ['END'], moduleIdent, ['.'].

definitionModule -->  
  definitionModuleHeader,
  importList, ['END'], moduleIdent, ['.'].

definitionModule -->  
  definitionModuleHeader,
  definition, ['END'], moduleIdent, ['.'].

definitionModule -->  
  definitionModuleHeader,
  importList, definition, ['END'], moduleIdent, ['.'].

fragment(definitionModuleHeader).

definitionModuleHeader -->  ['DEFINITION'], ['MODULE'], moduleIdent.

definitionModuleHeader -->  
  ['DEFINITION'], ['MODULE'], moduleIdent,
  ['['], blueprintToObey, [']'].

definitionModuleHeader -->  
  ['DEFINITION'], ['MODULE'], moduleIdent,
  ['FOR'], typeToExtend, [';'].

definitionModuleHeader -->  
  ['DEFINITION'], ['MODULE'], moduleIdent,
  ['['], blueprintToObey, [']'], ['FOR'], typeToExtend, [';'].


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
  blueprintHeader,
  ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  ['END'], ['.'].

blueprint -->  
  blueprintHeader,
  ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  requirementList, ['END'], ['.'].

blueprint -->  
  blueprintHeader,
  referentials, ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  ['END'], ['.'].

blueprint -->  
  blueprintHeader,
  referentials, ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  requirementList, ['END'], ['.'].

fragment(moduleTypeSpecOrNone).

moduleTypeSpecOrNone --> moduleTypeSpec.
moduleTypeSpecOrNone --> ['NONE'].

%% Blueprint Header 

fragment(blueprintHeader).

blueprintHeader --> ['BLUEPRINT'], blueprintIdent, [';'].

blueprintHeader -->  
  ['BLUEPRINT'], blueprintIdent,
  ['['], blueprintToRefine, [']'], [';'].

blueprintHeader -->  
  ['BLUEPRINT'], blueprintIdent,
  ['['], blueprintToRefine, [']'], [';'],
  ['FOR'], blueprintForTypeToExtend, [';'].

blueprintHeader -->  
  ['BLUEPRINT'], blueprintIdent,
  ['FOR'], blueprintForTypeToExtend, [';'].

%% (4.1) Blueprint To Refine

alias(blueprintToRefine).

blueprintToRefine -->  blueprintIdent.

%% (4.2) Blueprint For Type To Extend

alias(blueprintForTypeToExtend).

blueprintForTypeToExtend -->  ident.


%% (5) Identifier List

production(identList).

identList -->  ident.
identList -->  ident, [','], identList.


%% (6) Module Type Specification

production(moduleTypeSpec).

moduleTypeSpec -->  moduleTypeSpecHeader.
moduleTypeSpec -->  moduleTypeSpecHeader, moduleTypeSpecTail.

fragment(moduleTypeSpecHeader).

moduleTypeSpecHeader --> ['OPAQUE'].
moduleTypeSpecHeader --> ['RECORD'].

fragment(moduleTypeSpecTail).

moduleTypeSpecTail -->  [';'], propertySpec.
moduleTypeSpecTail -->  [';'], literalSpec.
moduleTypeSpecTail -->  [';'], propertySpec, [';'], literalSpec.


%% (7) Property Specification

production(propertySpec).

propertySpec -->  ['TPROPERTIES'], ['='], propertySpecTail.

fragment(propertySpecTail).

propertySpecTail -->  determinedProperties.
propertySpecTail -->  determinedProperties, ['|'], propertiesToDetermine.

%% (7.1) Determined Properties

alias(determinedProperties).

determinedProperties -->  identList.

%% (7.2) Properties To Determine

fragment(propertiesToDetermine).

propertiesToDetermine -->  ident, ['?'].
propertiesToDetermine -->  ident, ['?'], [','], propertiesToDetermine.


%% (8) Literal Specification

production(literalSpec).

literalSpec -->  ['TLITERAL'], ['='], protoLiteralList.

fragment(protoLiteralList).

protoLiteralList -->  protoLiteral.
protoLiteralList -->  protoLiteral, ['|'], protoLiteralList.

%% (8.1) Proto Literal

fragment(protoLiteral).

protoLiteral -->  simpleProtoLiteral.
protoLiteral -->  structuredProtoLiteral.

%% (8.2) Simple Proto Literal

fragment(simpleProtoLiteral).

simpleProtoLiteral -->  literalOrReferential.

%% (8.3) Literal Identifier Or Referential Identifier

alias(literalOrReferential).

literalOrReferential -->  ident.


%% (9) Structured Proto Literal

production(structuredProtoLiteral).

structuredProtoLiteral -->  ['{'], splArglistHead, splArglistTail, ['}'].
structuredProtoLiteral -->  ['{'], simpleProtoLiteralList, ['}'].

fragment(splArglistHead).

splArglistHead -->  ['ARGLIST'], ['OF'].
splArglistHead -->  ['ARGLIST'], reqValueCount, ['OF'].

splArglistTail -->  ['{'], simpleProtoLiteralList, ['}'].
splArglistTail -->  simpleProtoLiteral.

%% (9.1) Required Value Count

fragment(reqValueCount).

reqValueCount -->  wholeNumber.
reqValueCount -->  greaterThan, wholeNumber.

%% (9.2) Greater Than

alias(greaterThan).

greaterThan -->  ['>'].

%% (9.3) Whole Number

alias(wholeNumber).

wholeNumber -->  numberLiteral.


%% (10) Requirement

production(requirement).

requirement -->  conditionalRequirement.
requirement -->  unconditionalRequirement.

fragment(conditionalRequirement).

conditionalRequirement -->  condition, ['->'], unconditionalRequirement.

fragment(unconditionalRequirement).

unconditionalRequirement -->  constRequirement.
unconditionalRequirement -->  procedureRequirement.
unconditionalRequirement -->  ['TYPE'], ['='], procedureType.

%% (10.1) Condition

fragment(condition).

condition -->  boolConstIdent.
condition -->  ['NOT'], boolConstIdent.

%% (10.1) Condition

alias(boolConstIdent).

boolConstIdent -->  ident.


%% (11) Constant Requirement

production(constRequirement).

constRequirement --> ['CONST'], constRequirementTail.

fragment(constRequirementTail).

constRequirementTail -->  constBindingHead, simpleConstRequirement.
constRequirementTail -->  constBindingHead, ['='], ['NONE'].
constRequirementTail -->  constBindingHead.

constRequirementTail -->  simpleConstRequirement.
constRequirementTail -->  constAttribute, simpleConstRequirement.

fragment(constBindingHead).

constBindingHead -->  ['['], propertyToBindTo, [']'].

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

alias --> ['-'].

%% (11.6) Restricted Export

alias(restrictedExport).

restrictedExport --> ['*'].


%% (12) Simple Constant Requirement

production(simpleConstRequirement).

simpleConstRequirement -->  ident, '=', constExpression.
simpleConstRequirement -->  ident, ':', predefOrRefTypeIdent.

%% (12.1) Constant Expression

alias(constExpression).

constExpression -->  expression.

%% (12.2) Predefined Or Referential Type Identifier

alias(predefOrRefTypeIdent).

predefOrRefTypeIdent -->  ident.


%% (13) Procedure Requirement

production(procedureRequirement).

procedureRequirement --> ['PROCEDURE'], procedureRequirementTail.

fragment(procedureRequirementTail).

procedureRequirementTail -->  procBindingHead, procedureSignature.
procedureRequirementTail -->  procBindingHead, ['='], ['NONE'].
procedureRequirementTail -->  procBindingHead.

fragment(procBindingHead).

procBindingHead -->  ['['], entityToBindTo, [']'].

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

bindableOperator --> ['+'].
bindableOperator --> ['-'].
bindableOperator --> ['*'].
bindableOperator --> ['/'].
bindableOperator --> backslash.
bindableOperator --> ['='].
bindableOperator --> ['<'].
bindableOperator --> ['>'].
bindableOperator --> ['*.'].
bindableOperator --> ['::'].
bindableOperator --> ['IN'].
bindableOperator --> ['DIV'].
bindableOperator --> ['MOD'].
bindableOperator --> unaryMinus.

%% (13.4) Unary Minus

fragment(unaryMinus).

unaryMinus --> ['+'], ['/'], ['-'].

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

multiBindableMacro1 --> multiBindableMacro1Head.
multiBindableMacro1 --> multiBindableMacro1Head, bindingDifferentiator1.

fragment(multiBindableMacro1Head).

multiBindableMacro1Head --> ['COUNT'].
multiBindableMacro1Head --> ['RETRIEVE'].

%% (13.7) Binding Differentiatior 1

fragment(bindingDifferentiator1).

bindingDifferentiator1 --> ['|'], ['#'].

%% (13.8) Multi-Bindable Macro 1

fragment(multiBindableMacro2).

multiBindableMacro2 --> multiBindableMacro2Head.
multiBindableMacro2 --> multiBindableMacro2Head, bindingDifferentiator2.

fragment(multiBindableMacro2Head).

multiBindableMacro2Head --> ['STORE'].
multiBindableMacro2Head --> ['INSERT'].
multiBindableMacro2Head --> ['REMOVE'].

%% (13.9) Binding Differentiatior 2

fragment(bindingDifferentiator2).

bindingDifferentiator2 --> ['|'], [','].
bindingDifferentiator2 --> ['|'], ['#'].
bindingDifferentiator2 --> ['|'], ['*'].


%% (14) Library Generation Directive

production(libGenDirective).

libGenDirective --> 
  ['GENLIB'], libIdent, ['FROM'], template, ['FOR'], templateParams, ['END'].

fragment(templateParams).

templateParams --> placeholder, ['='], replacement.
templateParams --> placeholder, ['='], replacement, [';'], templateParams.

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

importDirective --> ['IMPORT'], modulesToImport.
importDirective --> unqualifiedImportHead, identifiersToImport.
importDirective --> unqualifiedImportHead, importAll.

fragment(unqualifiedImportHead).

unqualifiedImportHead --> ['FROM'], modulesOrEnumIdent, ['IMPORT'].

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
identifiersToImport --> ident, reExport, [','], identifiersToImport.

%% (15.4) Re-Export

alias(reExport).

reExport --> ['+'].

%% (15.5) Re-Export

alias(importAll).

importAll --> ['*'].


%% (16) Block

production(block).

block --> declarationList, blockTail.
block --> blockTail.

fragment(blockTail).

blockTail --> ['BEGIN'], statementSequence, ['END'].
blockTail --> ['END'].

fragment(declarationList).

declarationList --> declaration.
declarationList --> declaration, declarationList.


%% (17) Statement Sequence

production(statementSequence).

statementSequence --> statement.
statementSequence --> statement, [';'], statementSequence.


%% (18) Definition

production(definition).

definition --> ['CONST'], constDefinitionList.
definition --> ['TYPE'], typeDefinitionList.
definition --> ['VAR'], varDeclarationList.
definition --> procedureHeader, [';'].

fragment(constDefinitionList).

constDefinitionList --> constDefinition, [';'].
constDefinitionList --> constDefinition, [';'], constDefinitionList.

fragment(typeDefinitionList).

typeDefinitionList --> typeDefinition, [';'].
typeDefinitionList --> typeDefinition, [';'], typeDefinitionList.

fragment(varDeclarationList).

varDeclarationList --> variableDeclaration, [';'].
varDeclarationList --> variableDeclaration, [';'], varDeclarationList.


%% (19) Constant Definition

production(constDefinition).

constDefinition --> constDefinitionHead, constDeclaration.
constDefinition --> constDeclaration.

fragment(constDefinitionHead).

constDefinitionHead --> constBindingHead.
constDefinitionHead --> restrictedExport.

fragment(constDeclaration).

constDeclaration --> ident, ['='], constExpression.


%% (20) Variable Declaration

production(variableDeclaration).

variableDeclaration --> identList, [':'], variableDeclarationTail.

fragment(variableDeclarationTail).

variableDeclarationTail --> range, ['OF'], typeIdent.
variableDeclarationTail --> typeIdent.


%% (21) Declaration

production(declaration).

declaration --> ['CONST'], constDeclarationList.
declaration --> ['TYPE'], typeDeclarationList.
declaration --> ['VAR'], varDeclarationList.
declaration --> procedureHeader, [';'], block, ident, [';'].

fragment(constDeclarationList).

constDeclarationList --> constDeclaration, [';'].
constDeclarationList --> constDeclaration, [';'], constDeclarationList.

fragment(typeDeclarationList).

typeDeclarationList --> typeDeclaration, [';'].
typeDeclarationList --> typeDeclaration, [';'], typeDeclarationList.

fragment(typeDeclaration).

typeDeclaration --> ident, ['='], type, [';'].


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

range --> rangeHead, ['..'], rangeTail.

fragment(rangeHead).

rangeHead --> ['['], greaterThan, constExpression.
rangeHead --> ['['], constExpression.

fragment(rangeTail).

rangeTail --> lessThan, constExpression, ['['].
rangeTail --> constExpression, ['['].

%% (23.1) Greater Than

%% see alias #9.2

%% (23.2) Less Than

alias(lessThan).

lessThan --> ['<'].


%% (24) Enumeration Type

production(enumType).

enumType --> ['('], identList, [')'].
enumType --> ['('], ['+'], enumTypeToExtend, identList, [')'].

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

arrayType --> ['ARRAY'], ['OF'], typeIdent.
arrayType --> ['ARRAY'], componentCountList, ['OF'], typeIdent.

fragment(componentCountList).

componentCountList --> componentCount.
componentCountList --> componentCount, [','], componentCountList.

%% (26.1) Component Count

alias(componentCount).

componentCount --> constExpression.


%% (27) Record Type

production(recordType).

recordType --> ['RECORD'], recordTypeBody, ['END'].

fragment(recordTypeBody).

recordTypeBody --> fieldListSequence.
recordTypeBody --> fieldListSequence, indeterminateField.
recordTypeBody --> ['('], recTypeToExtend, [')'], fieldListSequence.

fragment(fieldListSequence).

fieldListSequence --> fieldList.
fieldListSequence --> fieldList, [';'], fieldListSequence.

%% (27.1) Record Type To Extend

alias(recTypeToExtend).

recTypeToExtend --> typeIdent.

%% (27.2) Field List

fragment(fieldList).

fieldList --> restrictedExport, variableDeclaration.
fieldList --> variableDeclaration.


%% (28) Indeterminate Field

production(indeterminateField).

indeterminateField --> 
  ['INDETERMINATE'], ident, [':'],
  ['ARRAY'], discriminantFieldIdent, ['OF'], typeIdent.

%% (28.1) Discriminant Field Identifier

alias(discriminantFieldIdent).

discriminantFieldIdent --> ident.


%% (29) Pointer Type

production(pointerType).

pointerType --> ['POINTER'], ['TO'], pointerTypeTail.

fragment(pointerTypeTail).

pointerTypeTail --> typeIdent.
pointerTypeTail --> ['CONST'], typeIdent.


%% (30) Procedure Type

production(procedureType).

procedureType --> procedureTypeHead, procedureTypeMiddle, procedureTypeTail.
procedureType --> procedureTypeHead, procedureTypeMiddle.
procedureType --> procedureTypeHead, procedureTypeTail.
procedureType --> procedureTypeHead.

alias(procedureTypeHead).

procedureTypeHead --> ['PROCEDURE'].

fragment(procedureTypeMiddle).

procedureTypeMiddle --> ['('], formalTypeList, [')'].

fragment(formalTypeList).

formalTypeList --> formalType.
formalTypeList --> formalType, [','], formalTypeList.

fragment(procedureTypeTail).

procedureTypeTail --> [':'], returnedType.

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

simpleFormalType --> typeIdent.
simpleFormalType --> ['ARRAY'], ['OF'], typeIdent.
simpleFormalType --> castingFormalType.

%% (31.1) Casting Formal Type

fragment(castingFormalType).

castingFormalType --> ['CAST'], ['ARRAY'], ['OF'], ['OCTET'].
castingFormalType --> ['CAST'], addressTypeIdent.

%% (31.2) Address Type Identifier

fragment(addressTypeIdent).

addressTypeIdent --> ['ADDRESS'].
addressTypeIdent --> ['UNSAFE'], ['.'], ['ADDRESS'].


%% (32) Attributed Formal Type

production(attributedFormalType).

attributedFormalType --> attributedFormalTypeHead, simpleFormalType.
attributedFormalType --> attributedFormalTypeHead, simpleVariadicFormalType.

fragment(attributedFormalTypeHead).

attributedFormalTypeHead --> ['CONST'].
attributedFormalTypeHead --> ['VAR'].
attributedFormalTypeHead --> ['NEW'].


%% (33) Simple Variadic Formal Type

production(simpleVariadicFormalType).

simpleVariadicFormalType --> 
  simpleVariadicFormalTypeHead, simpleVariadicFormalTypeTail.

fragment(simpleVariadicFormalTypeHead).

simpleVariadicFormalTypeHead --> ['ARGLIST'], ['OF'].
simpleVariadicFormalTypeHead --> ['ARGLIST'], numOfArgsToPass, ['OF'].

fragment(simpleVariadicFormalTypeTail).

simpleVariadicFormalTypeTail --> simpleFormalType.
simpleVariadicFormalTypeTail --> simpleFormalType, ['|'], arglistTerminator.

%% (33.1) Number Of Arguments To Pass

fragment(numOfArgsToPass).

numOfArgsToPass --> constExpression.
numOfArgsToPass --> greaterThan, constExpression.

%% (33.2) Variadic Argument List Terminator

fragment(arglistTerminator).

arglistTerminator --> ['NIL'].
arglistTerminator --> minusOne.
arglistTerminator --> wholeNumber.
arglistTerminator --> constQualident.

%% (33.3) Minus One (Negative One)

fragment(minusOne).

minusOne --> ['-'], ['1'].

%% (33.4) Constant Qualified Identifier

alias(constQualident).

constQualident --> qualident.


%% (34) Variadic Formal Type

production(variadicFormalType).

variadicFormalType --> 
  variadicFormalTypeHead, variadicFormalTypeMiddle, variadicFormalTypeTail.
variadicFormalType --> 
  variadicFormalTypeHead, variadicFormalTypeMiddle.

fragment(variadicFormalTypeHead).

variadicFormalTypeHead --> ['ARGLIST'], ['OF'].
variadicFormalTypeHead --> ['ARGLIST'], numOfArgsToPass, ['OF'].

fragment(variadicFormalTypeMiddle).

variadicFormalTypeMiddle --> simpleFormalType.
variadicFormalTypeMiddle --> ['{'], nonVariadicFormalTypeList, ['}'].

fragment(variadicFormalTypeTail).

variadicFormalTypeTail --> ['|'], arglistTerminator.

fragment(nonVariadicFormalTypeList).

nonVariadicFormalTypeList --> nonVariadicFormalType.
nonVariadicFormalTypeList --> 
  nonVariadicFormalType, [';'], nonVariadicFormalTypeList.


%% (35) Non-Variadic Formal Type

production(nonVariadicFormalType).

nonVariadicFormalType --> simpleFormalType.
nonVariadicFormalType --> nonVariadicFormalTypeHead, simpleFormalType.

fragment(nonVariadicFormalTypeHead).

nonVariadicFormalTypeHead --> ['CONST'].
nonVariadicFormalTypeHead --> ['VAR'].
nonVariadicFormalTypeHead --> ['NEW'].


%% (36) Procedure Header

production(procedureHeader).

procedureHeader --> ['PROCEDURE'], procedureSignature.
procedureHeader --> ['PROCEDURE'], procedureHeaderMiddle, procedureSignature.

fragment(procedureHeaderMiddle).

procedureHeaderMiddle --> ['['], entityToBindTo, [']'].
procedureHeaderMiddle --> restrictedExport.


%% (37) Procedure Signature

production(procedureSignature).

procedureSignature --> ident.
procedureSignature --> ident, procedureSignatureMiddle.
procedureSignature --> 
  ident, procedureSignatureMiddle, procedureSignatureTail.

fragment(procedureSignatureMiddle).

procedureSignatureMiddle --> ['('], formalParamsList, [')'].

fragment(procedureSignatureTail).

procedureSignatureTail --> [':'], returnedType.

fragment(formalParamsList).

formalParamsList --> formalParams.
formalParamsList --> formalParams, [';'], formalParamsList.


%% (38) Formal Parameters

production(formalParams).

formalParams --> formalParamsIdentList, simpleFormalType.
formalParams --> formalParamsIdentList, variadicFormalParams.
formalParams --> attributedFormalParams.

fragment(formalParamsIdentList).

formalParamsIdentList --> identList, [':'].


%% (39) Attributed Formal Parameters

production(attributedFormalParams).

attributedFormalParams --> 
  attributedFormalParamsHead, formalParamsIdentList, simpleFormalType.
attributedFormalParams --> 
  attributedFormalParamsHead, formalParamsIdentList, simpleVariadicFormalType.

fragment(attributedFormalParamsHead).

attributedFormalParamsHead --> ['CONST'].
attributedFormalParamsHead --> ['VAR'].
attributedFormalParamsHead --> ['NEW'].


%% (40) Variadic Formal Parameters

production(variadicFormalParams).

variadicFormalParams --> 
  variadicFormalParamsHead, variadicFormalParamsMiddle, variadicFormalParamsTail.
variadicFormalParams --> 
  variadicFormalParamsHead, variadicFormalParamsMiddle.

fragment(variadicFormalParamsHead).

variadicFormalParamsHead --> ['ARGLIST'], ['OF'].
variadicFormalParamsHead --> ['ARGLIST'], numOfArgsToPass, ['OF'].

fragment(variadicFormalParamsMiddle).

variadicFormalParamsMiddle --> simpleFormalType.
variadicFormalParamsMiddle --> ['{'], nonVariadicFormalParamsList, ['}'].

fragment(variadicFormalParamsTail).

variadicFormalParamsTail --> ['|'], arglistTerminator.

fragment(nonVariadicFormalParamsList).

nonVariadicFormalParamsList --> nonVariadicFormalParams.
nonVariadicFormalParamsList --> 
  nonVariadicFormalParams, [';'], nonVariadicFormalParamsList.


%% (41) Non-Variadic Formal Parameters

production(nonVariadicFormalParams).

nonVariadicFormalParams --> 
  nonVariadicFormalParamsHead, formalParamsIdentList, simpleFormalType.
nonVariadicFormalParams --> 
  formalParamsIdentList, simpleFormalType.

fragment(nonVariadicFormalParamsHead).

nonVariadicFormalParamsHead --> ['CONST'].
nonVariadicFormalParamsHead --> ['VAR'].
nonVariadicFormalParamsHead --> ['NEW'].


%% (42) Qualified Identifier

production(qualident).

qualident --> ident.
qualident --> ident, ['.'], qualident.


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
statement --> ['RETURN'].
statement --> ['RETURN'], expression.
statement --> ['EXIT'].


%% (44) Memory Management Operation

production(memMgtOperation).

memMgtOperation --> newStatementHead.
memMgtOperation --> newStatementHead, ['OF'], initSize.
memMgtOperation --> newStatementHead, [':='], initValue.

memMgtOperation --> ['RETAIN'], designator.
memMgtOperation --> ['RELEASE'], designator.

fragment(newStatementHead).

newStatementHead --> ['NEW'], designator.

%% (44.1) Initial Size

alias(initSize).

initSize --> expression.

%% (44.2) Initial Value

alias(initValue).

initValue --> expression.


%% (45) Update Or Procedure Call

production(updateOrProcCall).

updateOrProcCall --> designator, [':='], expression.
updateOrProcCall --> designator, incOrDecSuffix.
updateOrProcCall --> designator, actualParameters.
updateOrProcCall --> designator.
updateOrProcCall --> ['COPY'], designator, [':='], expression.

%% (45.1) Increment Or Decrement Suffix

fragment(incOrDecSuffix).

incOrDecSuffix --> ['++'].
incOrDecSuffix --> ['--'].


%% (46) IF statement

production(ifStatement).

ifStatement --> ifBranch, ['END'].
ifStatement --> ifBranch, elseBranch, ['END'].
ifStatement --> ifBranch, elsifBranchList, ['END'].
ifStatement --> ifBranch, elsifBranchList, elseBranch, ['END'].

fragment(ifBranch).

ifBranch --> ['IF'], boolExpression, ['THEN'], statementSequence.

fragment(elsifBranch).

elsifBranch --> ['ELSIF'], boolExpression, ['THEN'], statementSequence.

fragment(elsifBranchList).

elsifBranchList --> elsifBranch.
elsifBranchList --> elsifBranch, elsifBranchList.

fragment(elseBranch).

elseBranch --> ['ELSE'], statementSequence.

%% (46.1) Boolean Expression

alias(boolExpression).

boolExpression --> expression.


%% (47) CASE statement

production(caseStatement).

caseStatement --> caseStatementHead, caseList, ['END'].
caseStatement --> caseStatementHead, caseList, elseBranch, ['END'].

fragment(caseStatementHead).

caseStatementHead --> ['CASE'], expression, ['OF'].

fragment(caseList).

caseList --> ['|'], case.
caseList --> ['|'], case, caseList.


%% (48) Case

production(case).

case --> caseLabelList, [':'], statementSequence.

fragment(caseLabelList).

caseLabelList --> caseLabels.
caseLabelList --> caseLabels, [','], caseLabelList.


%% (49) Case Labels

production(caseLabels).

caseLabels --> constExpression.
caseLabels --> constExpression, ['..'], constExpression.


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

forStatement --> forStatementHeader, ['DO'], statementSequence, ['END'].

fragment(forStatementHeader).

forStatementHeader --> forStatementHeaderHead, forStatementHeaderTail.

fragment(forStatementHeaderHead).

forStatementHeaderHead --> ['FOR'], controlVariable.
forStatementHeaderHead --> ['FOR'], controlVariable, ascendOrDescend.

fragment(forStatementHeaderTail).

forStatementHeaderTail --> ['IN'], designator.
forStatementHeaderTail --> ['IN'], range, ['OF'], ordinalType.

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

designator --> qualident.
designator --> qualident, designatorTail.


%% (55) Designator Tail

production(designatorTail).

designatorTail --> designatorTailBody.
designatorTail --> designatorTailBody, designatorTail.

fragment(designatorTailBody).

designatorTailBody --> designatorTailBodyHead.
designatorTailBody --> designatorTailBodyHead, designatorTailBodyTail.

fragment(designatorTailBodyHead).

designatorTailBodyHead --> ['['], exprListOrSlice, [']'].
designatorTailBodyHead --> ['^'].

fragment(designatorTailBodyTail).

designatorTailBodyTail --> ['.'], ident.
designatorTailBodyTail --> ['.'], ident, designatorTailBodyTail.


%% (56) Expression List Or Slice

production(exprListOrSlice).

exprListOrSlice --> expression.
exprListOrSlice --> expression, expressionListTail.
exprListOrSlice --> expression, ['..'].
exprListOrSlice --> expression, ['..'], expression.

fragment(expressionListTail).

expressionListTail --> [','], expression.
expressionListTail --> [','], expression, expressionListTail.


%% (57) Expression

production(expression).

expression --> simpleExpression.
expression --> simpleExpression, operL1, simpleExpression.

%% (57.1) Level-1 Operator

fragment(operL1).

operL1 --> ['='].
operL1 --> ['#'].
operL1 --> ['<'].
operL1 --> ['<='].
operL1 --> ['>'].
operL1 --> ['>='].
operL1 --> ['IN'].
operL1 --> identityOp.
operL1 --> arrayConcatOp.

%% (57.2) Identity Operator

alias(identityOp).

identityOp --> ['=='].

%% (57.4) Array Concatenation Operator

alias(arrayConcatOp).

arrayConcatOp --> ['+>'].


%% (58) Simple Expression

production(simpleExpression).

simpleExpression --> termList.
simpleExpression --> sign, termList.

fragment(termList).

termList --> term.
termList --> term, operL2, termList.

fragment(sign).

sign --> ['+'].
sign --> ['-'].

%% (58.1) Level-2 Operator

fragment(operL2).

operL2 --> ['+'].
operL2 --> ['-'].
operL2 --> ['OR'].


%% (59) Term

production(term).

term --> factorOrNegation.
term --> factorOrNegation, operL3, term.

%% (59.1) Level-3 Operator

fragment(operL3).

operL3 --> ['*'].
operL3 --> ['/'].
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

dotProductOp --> ['*.'].


%% (60) Factor Or Negation

production(factorOrNegation).

factorOrNegation --> factorOrTypeConv.
factorOrNegation --> ['NOT'], factorOrTypeConv.


%% (61) Factor Or Type Conversion

production(factorOrTypeConv).

factorOrTypeConv --> factor.
factorOrTypeConv --> factor, ['::'], typeIdent.


%% (62) Factor

production(factor).

factor --> numberLiteral.
factor --> stringLiteral.
factor --> structuredValue.
factor --> ['('], expression, [')'].
factor --> designator.
factor --> designator, actualParameters.


%% (63) Actual Parameters

production(actualParameters).

actualParameters --> ['('], [')'].
actualParameters --> ['('], expressionList, [')'].


%% (64) Expression List

production(expressionList).

expressionList --> expression.
expressionList --> expression, [','], expressionList.


%% (65) Structured Value

production(structuredValue).

structuredValue --> ['{'], valueComponentList, ['}'].

fragment(valueComponentList).

valueComponentList --> valueComponent.
valueComponentList --> valueComponent, [','], valueComponentList.


%% (66) Value Component

production(valueComponent).

valueComponent --> constExpression, ['BY'], constExpression.
valueComponent --> constExpression, ['..'], constExpression.
valueComponent --> runtimeExpression.

%% (66.1) Runtime Expression

alias(runtimeExpression).

runtimeExpression --> expression.


%% T E R M I N A L S

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

fragment(digitOneToNine).

digitOneToNine --> ['1'].
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

realNumberTail --> ['.'], digitSeq.
realNumberTail --> ['.'], digitSeq, exponent.

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
base2Digit --> ['1'].


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

fragment(backslash).

backslash --> ['\\'].

fragment(singleQuote).

singleQuote --> ['\''].

fragment(doubleQuote).

doubleQuote --> ['"'].


%% end of grammar