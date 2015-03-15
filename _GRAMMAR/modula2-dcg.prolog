%% (c) 2015 by B.Kowarsch & R.Sutcliffe. All Rights Reserved.

%% DCG Grammar for Modula-2 R10

%% N O N - T E R M I N A L S

%% (1) Compilation Unit

compilationUnit --> [['IMPLEMENTATION'], programModule].
compilationUnit --> [programModule].
compilationUnit --> [definitionModule].
compilationUnit --> [blueprint].

%% (2) Program Module

programModule --> [['MODULE'], moduleIdent, [';'], block, moduleIdent, ['.']].
programModule --> [
  ['MODULE'], moduleIdent, [';'], importList, [';'], block, moduleIdent, ['.']
].

%% (2.1) Module Identifier

moduleIdent --> [ident].

%% (2.2) Import List

importList --> [libGenDirective].
importList --> [libGenDirective, [';'], importList].
importList --> [importDirective].
importList --> [importDirective, [';'], importList].


%% (3) Definition Module

definitionModule --> [
  definitionModuleHeader, ['END'], moduleIdent, ['.']
].

definitionModule --> [
  definitionModuleHeader,
  importList, ['END'], moduleIdent, ['.']
].

definitionModule --> [
  definitionModuleHeader,
  definition, ['END'], moduleIdent, ['.']
].

definitionModule --> [
  definitionModuleHeader,
  importList, definition, ['END'], moduleIdent, ['.']
].

definitionModuleHeader --> [['DEFINITION'], ['MODULE'], moduleIdent].

definitionModuleHeader --> [
  ['DEFINITION'], ['MODULE'], moduleIdent,
  ['['], blueprintToObey, [']']
].

definitionModuleHeader --> [
  ['DEFINITION'], ['MODULE'], moduleIdent,
  ['FOR'], typeToExtend, [';']
].

definitionModuleHeader --> [
  ['DEFINITION'], ['MODULE'], moduleIdent,
  ['['], blueprintToObey, [']'], ['FOR'], typeToExtend, [';']
].


%% (3.1) Blueprint To Obey

blueprintToObey --> [blueprintIdent].

%% (3.2) Blueprint Identifier

blueprintIdent --> [ident].

%% (3.3) Type To Extend

typeToExtend --> [ident].


%% (4) Blueprint

blueprint --> [
  blueprintHeader,
  ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  ['END'], ['.']
].

blueprint --> [
  blueprintHeader,
  ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  requirementList, ['END'], ['.']
].

blueprint --> [
  blueprintHeader,
  referentials, ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  ['END'], ['.']
].

blueprint --> [
  blueprintHeader,
  referentials, ['MODULE'], ['TYPE'], ['='], moduleTypeSpecOrNone, [';'],
  requirementList, ['END'], ['.']
].

moduleTypeSpecOrNone --> [moduleTypeSpec].
moduleTypeSpecOrNone --> ['NONE'].

%% Blueprint Header 

blueprintHeader --> [
  ['BLUEPRINT'], blueprintIdent, [';']
].

blueprintHeader --> [
  ['BLUEPRINT'], blueprintIdent,
  ['['], blueprintToRefine, [']'], [';']
].

blueprintHeader --> [
  ['BLUEPRINT'], blueprintIdent,
  ['['], blueprintToRefine, [']'], [';'],
  ['FOR'], blueprintForTypeToExtend [';'],
].

blueprintHeader --> [
  ['BLUEPRINT'], blueprintIdent,
  ['FOR'], blueprintForTypeToExtend [';'],
].

%% (4.1) Blueprint To Refine

blueprintToRefine --> [blueprintIdent].

%% (4.2) Blueprint For Type To Extend

blueprintForTypeToExtend --> [ident].


%% (5) Identifier List

identList --> [ident].
identList --> [ident, [','], identList].


%% (6) Module Type Specification

moduleTypeSpec --> [moduleTypeSpecHeader].
moduleTypeSpec --> [moduleTypeSpecHeader, moduleTypeSpecTail].

moduleTypeSpecHeader --> ['OPAQUE'].
moduleTypeSpecHeader --> ['RECORD'].

moduleTypeSpecTail --> [[';'], propertySpec].
moduleTypeSpecTail --> [[';'], literalSpec].
moduleTypeSpecTail --> [[';'], propertySpec, [';'], literalSpec].


%% (7) Property Specification

propertySpec --> [['TPROPERTIES'], ['='], propertySpecTail].

propertySpecTail --> [determinedProperties].
propertySpecTail --> [determinedProperties, ['|'], propertiesToDetermine].

%% (7.1) Determined Properties

determinedProperties --> [identList].

%% (7.2) Properties To Determine

propertiesToDetermine --> [ident, ['?']].
propertiesToDetermine --> [ident, ['?'] [','], propertiesToDetermine].


%% (8) Literal Specification

literalSpec --> [['TLITERAL'], ['='], protoLiteralList].

protoLiteralList --> [protoLiteral].
protoLiteralList --> [protoLiteral, ['|'], protoLiteralList].

%% (8.1) Proto Literal

protoLiteral --> [simpleProtoLiteral].
protoLiteral --> [structuredProtoLiteral].

%% (8.2) Simple Proto Literal

simpleProtoLiteral --> [literalOrReferential].

%% (8.3) Literal Identifier Or Referential Identifier

literalOrReferential --> [ident].


%% (9) Structured Proto Literal

structuredProtoLiteral --> [['{'], splArglistHead, splArglistTail, ['}']].
structuredProtoLiteral --> [['{'], simpleProtoLiteralList, ['}']].

splArglistHead --> [['ARGLIST'], ['OF']].
splArglistHead --> [['ARGLIST'], reqValueCount, ['OF']].

splArglistTail --> [['{'], simpleProtoLiteralList ['}']].
splArglistTail --> [simpleProtoLiteral].

%% (9.1) Required Value Count

reqValueCount --> [wholeNumber].
reqValueCount --> [greaterThan, wholeNumber].

%% (9.2) Greater Than

greaterThan --> ['>'].

%% (9.3) Whole Number

wholeNumber --> [numberLiteral].


%% (10) Requirement

requirement --> [conditionalRequirement].
requirement --> [unconditionalRequirement].

conditionalRequirement --> [condition, ['->'], unconditionalRequirement].
unconditionalRequirement --> [constRequirement].
unconditionalRequirement --> [procedureRequirement].
unconditionalRequirement --> [['TYPE'], ['='], procedureType].

%% (10.1) Condition

condition --> [boolConstIdent].
condition --> [['NOT'], boolConstIdent].

%% (10.1) Condition

boolConstIdent --> [ident].


%% (11) Constant Requirement

constRequirement --> ['CONST', constRequirementTail].

constRequirementTail --> [constBindingHead, simpleConstRequirement].
constRequirementTail --> [constBindingHead, ['='], ['NONE']].
constRequirementTail --> [constBindingHead].

constRequirementTail --> [simpleConstRequirement].
constRequirementTail --> [constAttribute, simpleConstRequirement].

constBindingHead --> [['['], propertyToBindTo, [']']].

%% (11.1) Property To Bind To

propertyToBindTo --> [memMgtProperty].
propertyToBindTo --> [scalarProperty].
propertyToBindTo --> [collectionProperty].

%% (11.2) Memory Management Property

memMgtProperty --> ['TDYN'].
memMgtProperty --> ['TREFC'].

%% (11.3) Scalar Property

scalarProperty --> ['TBASE'].
scalarProperty --> ['TPRECISION'].
scalarProperty --> ['TMINEXP'].
scalarProperty --> ['TMAXEXP'].

%% (11.4) Collection Property

collectionProperty --> ['TNIL'].
collectionProperty --> ['TLIMIT'].

%% (11.5) Constant Attribute

constAttribute --> [ancillaryConstant].
constAttribute --> [restrictedExport].

%% (11.5) Constant Attribute

ancillaryConstant --> ['-'].

%% (11.6) Restricted Export

restrictedExport --> ['*'].


%% (12) Simple Constant Requirement

simpleConstRequirement --> [ident, '=', constExpression].
simpleConstRequirement --> [ident, ':', predefOrRefTypeIdent].

%% (12.1) Constant Expression

constExpression --> [expression].

%% (12.2) Predefined Or Referential Type Identifier

predefOrRefTypeIdent --> [ident].


%% (13) Procedure Requirement

procedureRequirement --> ['PROCEDURE', procedureRequirementTail].

procedureRequirementTail --> [procBindingHead, procedureSignature].
procedureRequirementTail --> [procBindingHead, ['='], ['NONE']].
procedureRequirementTail --> [procBindingHead].

procBindingHead --> [['['], entityToBindTo, [']']].

%% (13.1) Entity To Bind To

entityToBindTo --> [bindableResWord].
entityToBindTo --> [bindableOperator].
entityToBindTo --> [bindableMacro].

%% (13.2) Bindable Reserved Word

bindableResWord --> ['NEW'].
bindableResWord --> ['RETAIN'].
bindableResWord --> ['RELEASE'].
bindableResWord --> ['COPY'].
bindableResWord --> ['FOR'].

%% (13.3) Bindable Operator

bindableOperator --> ['+'].
bindableOperator --> ['-'].
bindableOperator --> ['*'].
bindableOperator --> ['/'].
bindableOperator --> [backslash].
bindableOperator --> ['='].
bindableOperator --> ['<'].
bindableOperator --> ['>'].
bindableOperator --> ['*.'].
bindableOperator --> ['::'].
bindableOperator --> ['IN'].
bindableOperator --> ['DIV'].
bindableOperator --> ['MOD'].
bindableOperator --> [unaryMinus].

%% (13.4) Unary Minus

unaryMinus --> [['+'], ['/'], ['-']].

%% (13.5) Bindable Macro

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
bindableMacro --> [multiBindableMacro1].
bindableMacro --> [multiBindableMacro2].

%% (13.6) Multi-Bindable Macro 1

multiBindableMacro1 --> [multiBindableMacro1Head].
multiBindableMacro1 --> [multiBindableMacro1Head, bindingDifferentiator1].

multiBindableMacro1Head --> ['COUNT'].
multiBindableMacro1Head --> ['RETRIEVE'].

%% (13.7) Binding Differentiatior 1

bindingDifferentiator1 --> [['|'], ['#']].

%% (13.8) Multi-Bindable Macro 1

multiBindableMacro2 --> [multiBindableMacro2Head].
multiBindableMacro1 --> [multiBindableMacro2Head, bindingDifferentiator2].

multiBindableMacro2Head --> ['STORE'].
multiBindableMacro2Head --> ['INSERT'].
multiBindableMacro2Head --> ['REMOVE'].

%% (13.9) Binding Differentiatior 2

bindingDifferentiator2 --> [['|'], [',']].
bindingDifferentiator2 --> [['|'], ['#']].
bindingDifferentiator2 --> [['|'], ['*']].


%% (14) Library Generation Directive

libGenDirective --> [
  ['GENLIB'], libIdent, ['FROM'], template, ['FOR'], templateParams, ['END']
].

templateParams --> [placeholder, ['='], replacement].
templateParams --> [placeholder, ['='], replacement, [';'], templateParams].

%% (14.1) Library Identifier

libIdent --> [ident].

%% (14.2) Template

template --> [ident].

%% (14.3) Placeholder

placeholder --> [ident].

%% (14.4) Replacement

replacement --> [numberLiteral].
replacement --> [stringLiteral].
replacement --> [ChevronText].


%% (15) Import Directive

importDirective --> [['IMPORT'], modulesToImport].
importDirective --> [unqualifiedImportHead, identifiersToImport].
importDirective --> [unqualifiedImportHead, importAll].

unqualifiedImportHead --> [['FROM'], modulesOrEnumIdent, ['IMPORT']].

%% (15.1) Modules To Import

modulesToImport --> [identifiersToImport].

%% (15.2) Module Or Enumeration Identifier

modulesOrEnumIdent --> [ident].

%% (15.3) Identifiers To Import

identifiersToImport --> [ident].
identifiersToImport --> [ident, reExport].
identifiersToImport --> [ident, reExport, [','], identifiersToImport].

%% (15.4) Re-Export

reExport --> ['+'].

%% (15.5) Re-Export

importAll --> ['*'].


%% (16) Block

block --> [declarationList, blockTail].
block --> [blockTail].

blockTail --> [['BEGIN'], statementSequence, ['END']].
blockTail --> ['END'].

declarationList --> [declaration].
declarationList --> [declaration, declarationList].


%% (17) Statement Sequence

statementSequence --> [statement].
statementSequence --> [statement, [';'] statementSequence].


%% (18) Definition

definition --> [['CONST'], constDefinitionList].
definition --> [['TYPE'], typeDefinitionList].
definition --> [['VAR'], varDeclarationList].
definition --> [procedureHeader, [';']].

constDefinitionList --> [constDefinition, [';']].
constDefinitionList --> [constDefinition, [';'], constDefinitionList].

typeDefinitionList --> [typeDefinition, [';']].
typeDefinitionList --> [typeDefinition, [';'], typeDefinitionList].

varDeclarationList --> [variableDeclaration, [';']].
varDeclarationList --> [variableDeclaration, [';'], varDeclarationList].


%% (19) Constant Definition

constDefinition --> [constDefinitionHead, constDeclaration].
constDefinition --> [constDeclaration].

constDefinitionHead --> [constBindingHead].
constDefinitionHead --> [restrictedExport].

constDeclaration --> [ident, ['='], constExpression].


%% (20) Variable Declaration

variableDeclaration --> [identList, [':'], variableDeclarationTail].

variableDeclarationTail --> [range, ['OF'], typeIdent].
variableDeclarationTail --> [typeIdent].


%% (21) Declaration

declaration --> [['CONST'], constDeclarationList].
declaration --> [['TYPE'], typeDeclarationList].
declaration --> [['VAR'], varDeclarationList].
declaration --> [procedureHeader, [';'], block, ident, [';']].

constDeclarationList --> [constDeclaration, [';']].
constDeclarationList --> [constDeclaration, [';'], constDeclarationList].

typeDeclarationList --> [typeDeclaration, [';']].
typeDeclarationList --> [typeDeclaration, [';'], typeDeclarationList].

typeDeclaration --> [ident, ['='], type, [';']].


%% (22) Type

type --> [typeIdent].
type --> [derivedSubType].
type --> [enumType].
type --> [setType].
type --> [arrayType].
type --> [recordType].
type --> [pointerType].
type --> [procedureType].

%% (22.1) Type Identifier

typeIdent --> [qualident].

%% (22.2) Derived Sub-Type

derivedSubType --> [['ALIAS'], ['OF'], typeIdent].
derivedSubType --> [range, ['OF'], ordinalOrScalarType].
derivedSubType --> [['CONST'], dynamicTypeIdent].

%% (22.3) Ordinal Or Scalar Type

ordinalOrScalarType --> [typeIdent].

%% (22.4) Dynamic Type Identifier

dynamicTypeIdent --> [typeIdent].


%% (23) Range

range --> [rangeHead, ['..'], rangeTail].

rangeHead --> [['['], greaterThan, constExpression].
rangeHead --> [['['], constExpression].

rangeTail --> [lessThan, constExpression, ['[']].
rangeTail --> [constExpression, ['[']].

%% (23.1) Greater Than

%% see production #9.2

%% (23.2) Less Than

lessThan --> [['<']].


%% (24) Enumeration Type

enumType --> [['('], identList, [')']].
enumType --> [['('], ['+'], enumTypeToExtend, identList, [')']].

%% (24.1) Enumeration Type To Extend

enumTypeToExtend --> [enumTypeIdent].

%% (24.2) Enumeration Type Identifier

enumTypeIdent --> [typeIdent].


%% (25) Set Type

enumType --> [['SET'], ['OF'], enumTypeIdent].


%% (26) Array Type

arrayType --> [['ARRAY'], ['OF'], typeIdent].
arrayType --> [['ARRAY'], componentCountList, ['OF'], typeIdent].

componentCountList --> [componentCount].
componentCountList --> [componentCount, [','], componentCountList].

%% (26.1) Component Count

componentCount --> [constExpression].


%% (27) Record Type

recordType --> [['RECORD'], recordTypeBody, ['END']].

recordTypeBody --> [fieldListSequence].
recordTypeBody --> [fieldListSequence, indeterminateField].
recordTypeBody --> [['('], recTypeToExtend, [')'], fieldListSequence].

fieldListSequence --> [fieldList].
fieldListSequence --> [fieldList, [';'], fieldListSequence].

%% (27.1) Record Type To Extend

recTypeToExtend --> [typeIdent].

%% (27.2) Field List

fieldList --> [restrictedExport, variableDeclaration].
fieldList --> [variableDeclaration].


%% (28) Indeterminate Field

indeterminateField --> [
  ['INDETERMINATE'], ident, [':'],
  ['ARRAY'], discriminantFieldIdent, ['OF'], typeIdent
].

%% (28.1) Discriminant Field Identifier

discriminantFieldIdent --> [ident].


%% (29) Pointer Type

pointerType --> [['POINTER'], ['TO'], pointerTypeTail].

pointerTypeTail --> [typeIdent].
pointerTypeTail --> [['CONST'], typeIdent].


%% (30) Procedure Type

procedureType --> [procedureTypeHead, procedureTypeMiddle, procedureTypeTail].
procedureType --> [procedureTypeHead, procedureTypeMiddle].
procedureType --> [procedureTypeHead, procedureTypeTail].
procedureType --> [procedureTypeHead].

procedureTypeHead --> ['PROCEDURE'].

procedureTypeMiddle --> [['('], formalTypeList, [')']].

formalTypeList --> [formalType].
formalTypeList --> [formalType, [','], formalTypeList].

procedureTypeTail --> [[':'], returnedType].

%% (30.1) Formal Type

formalType --> [simpleFormalType].
formalType --> [attributedFormalType].
formalType --> [variadicFormalType].

%% (30.2) Returned Type

returnedType --> [typeIdent].


%% (31) Simple Formal Type

simpleFormalType --> [typeIdent].
simpleFormalType --> [['ARRAY'], ['OF'], typeIdent].
simpleFormalType --> [castingFormalType].

%% (31.1) Casting Formal Type

castingFormalType --> [['CAST'], ['ARRAY'], ['OF'], ['OCTET']].
castingFormalType --> [['CAST'], addressTypeIdent].

%% (31.2) Address Type Identifier

addressTypeIdent --> ['ADDRESS'].
addressTypeIdent --> [['UNSAFE'], ['.'], ['ADDRESS']].


%% (32) Attributed Formal Type

attributedFormalType --> [attributedFormalTypeHead, simpleFormalType].
attributedFormalType --> [attributedFormalTypeHead, simpleVariadicFormalType].

attributedFormalTypeHead --> ['CONST'].
attributedFormalTypeHead --> ['VAR'].
attributedFormalTypeHead --> ['NEW'].


%% (33) Simple Variadic Formal Type

simpleVariadicFormalType --> [
  simpleVariadicFormalTypeHead, simpleVariadicFormalTypeTail
].

simpleVariadicFormalTypeHead --> [['ARGLIST'], ['OF']].
simpleVariadicFormalTypeHead --> [['ARGLIST'], numOfArgsToPass, ['OF']].

simpleVariadicFormalTypeTail --> [simpleFormalType].
simpleVariadicFormalTypeTail --> [simpleFormalType, ['|'], arglistTerminator].

%% (33.1) Number Of Arguments To Pass

numOfArgsToPass --> [constExpression].
numOfArgsToPass --> [greaterThan, constExpression].

%% (33.2) Variadic Argument List Terminator

numOfArgsToPass --> ['NIL'].
numOfArgsToPass --> [minusOne].
numOfArgsToPass --> [wholeNumber].
numOfArgsToPass --> [constQualident].

%% (33.3) Minus One (Negative One)

minusOne --> [['-'], ['1']].

%% (33.4) Constant Qualified Identifier

constQualident --> [qualident].


%% (34) Variadic Formal Type

variadicFormalType --> [
  variadicFormalTypeHead, variadicFormalTypeMiddle, variadicFormalTypeTail
].
variadicFormalType --> [
  variadicFormalTypeHead, variadicFormalTypeMiddle
].

variadicFormalTypeHead --> [['ARGLIST'], ['OF']].
variadicFormalTypeHead --> [['ARGLIST'], numOfArgsToPass, ['OF']].

variadicFormalTypeMiddle --> [simpleFormalType].
variadicFormalTypeMiddle --> [['{'], nonVariadicFormalTypeList, ['}']].

variadicFormalTypeTail --> [['|'], arglistTerminator].

nonVariadicFormalTypeList --> [nonVariadicFormalType].
nonVariadicFormalTypeList --> [
  nonVariadicFormalType, [';'], nonVariadicFormalTypeList
].


%% (35) Non-Variadic Formal Type

nonVariadicFormalType --> [simpleFormalType].
nonVariadicFormalType --> [nonVariadicFormalTypeHead, simpleFormalType].

nonVariadicFormalTypeHead --> ['CONST'].
nonVariadicFormalTypeHead --> ['VAR'].
nonVariadicFormalTypeHead --> ['NEW'].


%% (36) Procedure Header

procedureHeader --> [['PROCEDURE'], procedureSignature].
procedureHeader --> [['PROCEDURE'], procedureHeaderMiddle, procedureSignature].

procedureHeaderMiddle --> [['['], entityToBindTo, [']']].
procedureHeaderMiddle --> [restrictedExport].


%% (37) Procedure Signature

procedureSignature --> [ident].
procedureSignature --> [ident, procedureSignatureMiddle].
procedureSignature --> [
  ident, procedureSignatureMiddle, procedureSignatureTail
].

procedureSignatureMiddle --> [['('], formalParamsList, [')']].

procedureSignatureTail --> [[':'], returnedType].

formalParamsList --> [formalParams].
formalParamsList --> [formalParams, [';'], formalParamsList].


%% (38) Formal Parameters

formalParams --> [formalParamsIdentList, simpleFormalType].
formalParams --> [formalParamsIdentList, variadicFormalParams].
formalParams --> [attributedFormalParams].

formalParamsIdentList --> [identList, [':']].


%% (39) Attributed Formal Parameters

attributedFormalParams --> [
  attributedFormalParamsHead, formalParamsIdentList, simpleFormalType
].
attributedFormalParams --> [
  attributedFormalParamsHead, formalParamsIdentList, simpleVariadicFormalType
].

attributedFormalParamsHead --> ['CONST'].
attributedFormalParamsHead --> ['VAR'].
attributedFormalParamsHead --> ['NEW'].


%% (40) Variadic Formal Parameters

variadicFormalParams --> [
  variadicFormalParamsHead, variadicFormalParamsMiddle, variadicFormalParamsTail
].
variadicFormalParams --> [
  variadicFormalParamsHead, variadicFormalParamsMiddle
].

variadicFormalParamsHead --> [['ARGLIST',] ['OF']].
variadicFormalParamsHead --> [['ARGLIST'], numOfArgsToPass, ['OF']].

variadicFormalParamsMiddle --> [simpleFormalType].
variadicFormalParamsMiddle --> [['{'], nonVariadicFormalParamsList, ['}']].

variadicFormalParamsTail --> [['|'], arglistTerminator].

nonVariadicFormalParamsList --> [nonVariadicFormalParams].
nonVariadicFormalParamsList --> [
  nonVariadicFormalParams, [';'], nonVariadicFormalParamsList
].


%% (41) Non-Variadic Formal Parameters

nonVariadicFormalParams --> [
  nonVariadicFormalParamsHead, formalParamsIdentList, simpleFormalType
].
nonVariadicFormalParams --> [
  formalParamsIdentList, simpleFormalType
].

nonVariadicFormalParamsHead --> ['CONST'].
nonVariadicFormalParamsHead --> ['VAR'].
nonVariadicFormalParamsHead --> ['NEW'].


%% (42) Qualified Identifier

qualident --> [ident].
qualident --> [ident, ['.'], qualident].


%% (43) Statement

statement --> [memMgtOperation].
statement --> [updateOrProcCall].
statement --> [ifStatement].
statement --> [caseStatement].
statement --> [loopStatement].
statement --> [whileStatement].
statement --> [repeatStatement].
statement --> [forStatement].
statement --> ['RETURN'].
statement --> ['RETURN', expression].
statement --> ['EXIT'].


%% (44) Statement

memMgtOperation --> [newStatementHead].
memMgtOperation --> [newStatementHead, ['OF'], initSize].
memMgtOperation --> [newStatementHead, [':='], initValue].

memMgtOperation --> [['RETAIN'], designator].
memMgtOperation --> [['RELEASE'], designator].

newStatementHead --> [['NEW'], designator].

%% (44.1) Initial Size

initSize --> [expression].

%% (44.2) Initial Value

initValue --> [expression].


%% (45) Update Or Procedure Call

updateOrProcCall --> [designator, [':='], expression].
updateOrProcCall --> [designator, incOrDecSuffix].
updateOrProcCall --> [designator, actualParameters].
updateOrProcCall --> [designator].
updateOrProcCall --> ['COPY', designator, [':='], expression].

%% (45.1) Increment Or Decrement Suffix

incOrDecSuffix --> ['++'].
incOrDecSuffix --> ['--'].


%% (46) IF statement

ifStatement --> [ifBranch, 'END'].
ifStatement --> [ifBranch, elseBranch, 'END'].
ifStatement --> [ifBranch, elsifBranchList, 'END'].
ifStatement --> [ifBranch, elsifBranchList, elseBranch, 'END'].

ifBranch --> [['IF'], boolExpression, ['THEN'], statementSequence].
elsifBranch --> [['ELSIF'], boolExpression, ['THEN'], statementSequence].
elsifBranchList --> [elsifBranch].
elsifBranchList --> [elsifBranch, elsifBranchList].
elseBranch --> [['ELSE'], statementSequence].

%% (46.1) Boolean Expression

boolExpression --> [expression].


%% (47) CASE statement

caseStatement --> [caseStatementHead, caseList, 'END'].
caseStatement --> [caseStatementHead, caseList, elseBranch, 'END'].

caseStatementHead --> [['CASE'], expression, 'OF'].

caseList --> [['|'], case].
caseList --> [['|'], case, caseList].


%% (48) Case

case --> [caseLabelList, [':'], statementSequence].

caseLabelList --> [caseLabels].
caseLabelList --> [caseLabels, [','], caseLabelList].


%% (49) Case Labels

caseLabels --> [constExpression].
caseLabels --> [constExpression, ['..'], constExpression].


%% (50) LOOP Statement

loopStatement --> [['LOOP'], statementSequence, ['END']].


%% (51) WHILE Statement

whileStatement --> [
  ['WHILE'], boolExpression, ['DO'], statementSequence, ['END']
].


%% (52) REPEAT Statement

repeatStatement --> [
  ['REPEAT'], statementSeqence, ['UNTIL'], boolExpression, ['END']
].


%% (53) FOR Statement

forStatement --> [forStatementHeader, ['DO'] statementSequence, ['END']].

forStatementHeader --> [forStatementHeaderHead, forStatementHeaderTail].

forStatementHeaderHead --> [['FOR'], controlVariable].
forStatementHeaderHead --> [['FOR'], controlVariable, ascendOrDescend].

forStatementHeaderTail --> [['IN'], designator].
forStatementHeaderTail --> [['IN'], range, ['OF'], ordinalType].

%% (53.1) Control Variable

controlVariable --> [ident].

%% (53.2) Ascend Or Descend

ascendOrDescend --> [incOrDecSuffix].

%% (53.3) Ordinal Type

ordinalType --> [typeIdent].


%% (54) Designator

designator --> [qualident].
designator --> [qualident, designatorTail].


%% (55) Designator Tail

designatorTail --> [designatorTailBody].
designatorTail --> [designatorTailBody, designatorTail].

designatorTailBody --> [designatorTailBodyHead].
designatorTailBody --> [designatorTailBodyHead, designatorTailBodyTail].

designatorTailBodyHead --> [['['], exprListOrSlice, [']']].
designatorTailBodyHead --> ['^'].

designatorTailBodyTail --> ['.', ident].
designatorTailBodyTail --> ['.', ident, designatorTailBodyTail].


%% (56) Expression List Or Slice

exprListOrSlice --> [expression].
exprListOrSlice --> [expression, expressionListTail].
exprListOrSlice --> [expression, ['..']].
exprListOrSlice --> [expression, ['..'], expression].

expressionListTail --> [[','], expression].
expressionListTail --> [[','], expression, expressionListTail].


%% (57) Expression

expression --> [simpleExpression].
expression --> [simpleExpression, operL1, simpleExpression].

%% (57.1) Level-1 Operator

operL1 --> ['='].
operL1 --> ['#'].
operL1 --> ['<'].
operL1 --> ['<='].
operL1 --> ['>'].
operL1 --> ['>='].
operL1 --> ['IN'].
operL1 --> [identityOp].
operL1 --> [arrayConcatOp].

%% (57.2) Identity Operator

identityOp --> ['=='].

%% (57.4) Array Concatenation Operator

arrayConcatOp --> ['+>'].


%% (58) Simple Expression

simpleExpression --> [termList].
simpleExpression --> [sign, termList].

termList --> [term].
termList --> [term, operL2, termList].

sign --> ['+'].
sign --> ['-'].

%% (58.1) Level-2 Operator

operL2 --> ['+'].
operL2 --> ['-'].
operL2 --> ['OR'].


%% (59) Term

term --> [factorOrNegation].
term --> [factorOrNegation, operL3, term].

%% (59.1) Level-3 Operator

operL2 --> ['*'].
operL2 --> ['/'].
operL2 --> ['DIV'].
operL2 --> ['MOD'].
operL2 --> ['AND'].
operL2 --> [setDiffOp].
operL2 --> [dotProductOp].

%% (59.2) Set Difference Operator

setDiffOp --> [backslash].

%% (59.3) Dot Product Operator

dotProductOp --> ['*.'].


%% (60) Factor Or Negation

factorOrNegation --> [factorOrTypeConv].
factorOrNegation --> [['NOT'], factorOrTypeConv].


%% (61) Factor Or Type Conversion

factorOrTypeConv --> [factor].
factorOrTypeConv --> [factor, ['::'], typeIdent].


%% (62) Factor

factor --> [numberLiteral].
factor --> [stringLiteral].
factor --> [structuredValue].
factor --> [['('], expression, [')']].
factor --> [designator].
factor --> [designator, actualParameters].


%% (63) Actual Parameters

actualParameters --> [['('], [')']].
actualParameters --> [['('], expressionList, [')']].


%% (64) Expression List

expressionList --> [expression].
expressionList --> [expression, [','], expressionList].


%% (65) Structured Value

structuredValue --> [['{'], valueComponentList, ['}']].

valueComponentList --> [valueComponent].
valueComponentList --> [valueComponent, [','], valueComponentList].


%% (66) Value Component

valueComponent --> [constExpression, ['BY'], constExpression].
valueComponent --> [constExpression, ['..'], constExpression].
valueComponent --> [runtimeExpression].

%% (66.1) Runtime Expression

runtimeExpression --> [expression].


%% T E R M I N A L S

%% Identifier

ident --> [identHead, identTail].

identHead --> [letter].
identHead --> foreignIdentCharSequence, letterOrDigitSequence].

foreignIdentCharSequence --> [foreignIdentChar].
foreignIdentCharSequence --> [foreignIdentChar, foreignIdentCharSequence].

letterOrDigitSequence --> [letterOrDigit].
letterOrDigitSequence --> [letterOrDigit, letterOrDigitSequence].

identTail --> [identTailChar].
identTail --> [identTailChar, identTail].

foreignIdentChar --> ['_'].
foreignIdentChar --> ['$'].

letterOrDigit --> [letter].
letterOrDigit --> [digit].


%% Number Literal

numberLiteral --> [digitZero]
numberLiteral --> [digitZero, RealNumberTail]
numberLiteral --> [digitZero, ['b'], base2DigitSeq]
numberLiteral --> [digitZero, ['x'], base16DigitSeq]
numberLiteral --> [digitZero, ['u'], base16DigitSeq]
numberLiteral --> [digitOneToNine]
numberLiteral --> [digitOneToNine, decimalNumberTail]

digitZero --> ['0'].
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

decimalNumberTail --> [digitSeq].
decimalNumberTail --> [digitSeq, realNumberTail].
decimalNumberTail --> [singleQuote, digitSeq].
decimalNumberTail --> [singleQuote, digitSeq, realNumberTail].
decimalNumberTail --> [realNumberTail].

%% Real Number Tail

realNumberTail --> [['.'], digitSeq].
realNumberTail --> [['.'], digitSeq, exponent].

exponent --> [['e'], digitSeq].
exponent --> [['e'], sign, digitSeq].

%% Digit Sequence

digitSeq --> [digitGroup].
digitSeq --> [digitGroup, singleQuote, digitSeq].

%% Digit Group

digitGroup --> [digit].
digitGroup --> [digit, digitGroup].

%% Digit

digit --> [digitZero].
digit --> [digitOneToNine].

%% Base-16 Digit Sequence

base16DigitSeq --> [base16DigitGroup].
base16DigitSeq --> [base16DigitGroup, singleQuote, base16DigitSeq].

%% Base-16 Digit Group

base16DigitGroup --> [base16Digit].
base16DigitGroup --> [base16Digit, base16DigitGroup].

%% Base-16 Digit

base16Digit --> [digit].
base16Digit --> ['A'].
base16Digit --> ['B'].
base16Digit --> ['C'].
base16Digit --> ['D'].
base16Digit --> ['E'].
base16Digit --> ['F'].

%% Base-2 Digit Sequence

base2DigitSeq --> [base2DigitGroup].
base2DigitSeq --> [base2DigitGroup, singleQuote, base2DigitSeq].

%% Base-2 Digit Group

base2DigitGroup --> [base2Digit].
base2DigitGroup --> [base2Digit, base2DigitGroup].

%% Base-2 Digit

base2Digit --> [digitZero].
base2Digit --> ['1'].


%% String Literal

stringLiteral --> [singleQuotedString].
stringLiteral --> [doubleQuotedString].

%% Single Quoted String

singleQuotedString --> [singleQuote, singleQuote].
singleQuotedString --> [singleQuote, singleQuotedCharSeq, singleQuote].

singleQuotedCharSeq --> [singleQuotedChar].
singleQuotedCharSeq --> [singleQuotedChar, singleQuotedCharSeq].

singleQuotedChar --> [doubleQuote].
singleQuotedChar --> [quotableCharacter].

%% Double Quoted String

doubleQuotedString --> [doubleQuote, doubleQuote].
doubleQuotedString --> [doubleQuote, doubleQuotedCharSeq, doubleQuote].

doubleQuotedCharSeq --> [doubleQuotedChar].
doubleQuotedCharSeq --> [doubleQuotedChar, doubleQuotedCharSeq].

doubleQuotedChar --> [singleQuote].
doubleQuotedChar --> [quotableCharacter].

%% Quotable Character

quotableCharacter --> [digit].
quotableCharacter --> [letter].
quotableCharacter --> [space].
quotableCharacter --> [nonAlphaNumQuotable].
quotableCharacter --> [escapedCharacter].

%% Letter

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

space --> [' '].

%% Non-Alphanumeric Quotable Character

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

escapedCharacter --> [backslash, ['n']].
escapedCharacter --> [backslash, ['t']].
escapedCharacter --> [backslash, backslash].

%% Chevron Delimited Text

chevronText --> ['<<', ['>>']].
chevronText --> ['<<', chevronQuotedCharSeq, ['>>']].

chevronQuotedCharSeq --> [chevronQuotedChar].
chevronQuotedCharSeq --> [chevronQuotedChar, chevronQuotedCharSeq].

chevronQuotedChar --> [singleQuote].
chevronQuotedChar --> [doubleQuote].
chevronQuotedChar --> [quotableCharacter].

% Synonyms For Escaped Characters

backslash --> ['\\'].
singleQuote --> ['\''].
doubleQuote --> ['\"'].


%% end of grammar