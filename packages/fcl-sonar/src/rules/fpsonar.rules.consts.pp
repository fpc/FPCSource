{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Resource strings: the message templates for the analysis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Rules.Consts;

{$mode objfpc}{$H+}

interface

resourcestring
  // FpSonar.Rules.Calls
  SFormatArgumentType = 'Format conversion "%s" does not accept an argument of type %s';
  SFormatArgumentCount = 'Format string needs %s argument(s) but %s supplied';
  SValidFormatString = 'Invalid format conversion "%s" in format string';
  SFreeAndNilArgument = 'FreeAndNil applied to a non-class value of type %s';
  SConstructorOnInstanceVariable =
    'Constructor "%s" called on an already-allocated instance';
  SStringListDuplicatesNeedsSorted =
    'Duplicates set to %s but the list is never sorted (setting is ignored)';
  SDestructorShouldOverrideDestroy =
    'Destructor "%s" will not participate in polymorphic destruction (must be named Destroy and marked override)';
  SOverrideOnlyCallsInherited =
    'Override method "%s" only forwards to the inherited method and can be removed';
  SIfThenNotShortCircuit =
    'IfThen evaluates both value arguments; "%s" is guarded by the condition and may be used unsafely';
  SAssertWithoutMessage =
    'Assert without a message; the raised EAssertionFailed carries no diagnostic text';
  SDefaultFormatSettingsInDateFormat =
    '"%s" uses the global DefaultFormatSettings; pass an explicit TFormatSettings';
  SExplicitDefaultArrayProperty =
    'Default array property "%s" accessed by name; the shorthand [] is equivalent';
  SStringFirstCharByIndex =
    'First character read by index; a clearer intent-revealing form is preferable';
  STListLastByIndex = 'Last element fetched by index; use the dedicated Last instead';
  SRedundantInherited =
    'Redundant inherited statement; it binds to no overridable parent method and can be removed';
  SImplicitTEncodingDefault =
    'Encoding-omitting overload implicitly uses the platform-dependent TEncoding.Default; specify an explicit TEncoding instead';
  SSingleOverloadOfMathFunction =
    'Math routine bound to its Single-precision overload; a higher-precision Double/Extended overload is available';

  // FpSonar.Rules.Casts
  SCharToCharPointerCast =
    'Casting a single character to %s yields a pointer to a temporary, not a string';
  SObjectCastNotInHierarchy =
    'Cast between unrelated class types %s and %s can never succeed';
  SRedundantCast = 'Redundant cast to %s; the operand already has that type';
  SObjectCastBeforeFree = 'Redundant cast to %s before Free; Free operates on TObject';
  SUnicodeToAnsiCast =
    'Cast to ANSI type %s from a Unicode/wide type silently loses characters';
  SPlatformDependentCast =
    'Cast between a pointer and fixed-width integer %s is not 64-bit safe; use PtrInt/PtrUInt';
  SPlatformDependentTruncation =
    'Narrowing a wide integer to fixed-width %s silently drops high bits on 64-bit';

  // FpSonar.Rules.Classes
  SVisibilityAscendingOrder = 'Visibility section %s is out of ascending order';
  SDeclarationsFollowVisibilityOrder =
    'Declaration %s does not follow the field, method, property order';
  SFieldsNotPublic =
    'Field %s should not be public — use a private field with a property';
  SFileNotTooManyClasses = 'Unit declares %s classes, more than the maximum of 5';
  SInterfaceNotEmpty = 'Interface %s declares no methods or properties';
  SInterfaceUniqueGuid = 'Interface %s should have a unique GUID';
  SConstructorInherited = 'Constructor %s does not call inherited';
  SDestructorInherited = 'Destructor %s does not call inherited';
  STopLevelClassInheritsTObject = 'Class %s should explicitly declare an ancestor';

  // FpSonar.Rules.Control
  SExhaustiveCaseStatement = 'case statement does not handle all enumerated values: %s';
  SExceptionRaised = 'Exception %s is constructed but never raised';
  SSingleIterationLoop = 'Loop body always exits on the first iteration; use if instead';
  SNoPascalStyleResultAssignment =
    'function %s returns by assigning to its own name; use Result instead';
  SRedundantAssignedCheckBeforeFree =
    'redundant Assigned/nil check before Free; Free is already nil-safe';
  SLoopBeyondCollectionEnd = 'loop indexes the collection past its last valid element';
  SRoutineResultAssigned =
    'function may return without assigning a result on some code path';
  SNoCatchRawException =
    'Catching the root Exception class masks unrelated failures; catch a specific subclass';
  SNoRaiseRawException =
    'Raising the root Exception class is too generic; raise a specific subclass';

  // FpSonar.Rules.Exceptions
  SNoEmptyFinally = 'finally block is empty';
  SExceptionsNotSwallowed = 'Exception is swallowed by an empty except handler';
  SNoExplicitReRaise = 'Use a bare raise instead of re-raising %s';

  // FpSonar.Rules.Forms
  SLfmFormFileExists = 'This form/frame/datamodule unit has no sibling .lfm file.';

  // FpSonar.Rules.Imports
  SFullyQualifiedImports =
    'Reference to "%s" is ambiguous across units; qualify it with its unit name';
  SMoveImportToImplementation =
    'Unit "%s" is used only in the implementation; move it to the implementation uses clause';

  // FpSonar.Rules.Layout
  SNoTrailingWhitespace = 'Trailing whitespace';
  SNoTabs = 'Tab character; use spaces';
  SLineTooLong = 'Line is %s characters long; the limit is %s';
  SLongNumericLiteralUnderscores =
    'Numeric literal with %s digits should use _ separators';
  SDigitGroupingStandard = 'Irregular digit grouping in numeric literal';

  // FpSonar.Rules.Naming
  SClassNaming = 'Class name "%s" does not match the required pattern "%s"';
  SRecordNaming = 'Record name "%s" does not match the required pattern "%s"';
  SInterfaceNaming = 'Interface name "%s" does not match the required pattern "%s"';
  SEnumNaming = 'Enumeration name "%s" does not match the required pattern "%s"';
  SHelperNaming = 'Helper name "%s" does not match the required pattern "%s"';
  SPointerNaming = 'Pointer type name "%s" does not match the required pattern "%s"';
  SAttributeNaming = 'Attribute name "%s" does not match the required pattern "%s"';
  SConstantNaming = 'Constant name "%s" does not match the required pattern "%s"';
  SFieldNaming = 'Field name "%s" does not match the required pattern "%s"';
  SVariableNaming = 'Variable name "%s" does not match the required pattern "%s"';
  SRoutineNaming = 'Routine name "%s" does not match the required pattern "%s"';
  SConstructorNaming = 'Constructor name "%s" does not match the required pattern "%s"';
  SUnitNaming = 'Unit name "%s" does not match the required pattern "%s"';
  SIdentifierTooShort = 'Identifier "%s" is shorter than the minimum length of %s';

  // FpSonar.Rules.Parens
  SRemoveRedundantParentheses = 'Redundant parentheses can be removed.';
  SParenthesizeAmbiguousNot =
    'Ambiguous "not": parenthesize the negated operand, e.g. (not a) and b.';

  // FpSonar.Rules.Refs
  SNoObjectAsInterface =
    'Assigning an object to a COM interface implicitly acquires reference counting on a manually managed instance';
  SNoNestedRoutineAsProcValue =
    'The address of a nested routine is stored where it can outlive the enclosing routine''s stack frame';
  SNoInlineVarCapturedByAnonMethod =
    'An anonymous method captures a variable whose lifetime is narrower than the closure (a block-scoped inline var or a per-iteration for-var loop variable)';

  // FpSonar.Rules.SemNaming
  SConsistentNameCasing =
    'Identifier "%s" is spelled with different letter casing than its declaration "%s"';
  SDescendantNamingConvention =
    'Type "%s" descends a base whose convention requires the name to match "%s"';

  // FpSonar.Rules.Structure
  SCyclomaticComplexity = 'Cyclomatic complexity is %s; the maximum allowed is %s';
  SCognitiveComplexity = 'Cognitive complexity is %s; the maximum allowed is %s';
  SRoutineTooLarge = 'Routine has %s statements; the maximum allowed is %s';
  SRoutineTooDeeplyNested = 'Routine nesting depth is %s; the maximum allowed is %s';
  STooManyNestedRoutines = 'Routine has %s nested routines; the maximum allowed is %s';
  STooManyParameters = 'Routine has %s parameters; the maximum allowed is %s';
  STooManyVariables = 'Routine has %s local variables; the maximum allowed is %s';
  STooManyDefaultParameters =
    'Routine has %s default parameters; the maximum allowed is %s';
  SBeginEndRequired = 'The %s body must be a begin..end block';
  SNoGoto = 'Avoid goto statements';
  SNoWith = 'Avoid with statements';
  SNoSelfAssignment = 'Remove this self-assignment; it has no effect';
  SNoInlineAssembly = 'Avoid inline assembly (asm..end) blocks';
  SCaseAtLeastTwoItems =
    'A case statement has %s case branch(es); a case should have at least two';
  SNoEmptyBlock = 'Remove this empty begin..end block';
  SRoutineNotEmpty = 'Routine %s has an empty body';
  SUnitNotEmpty = 'Unit %s has no declarations or statements';
  SRedundantJump = 'Remove this redundant %s';
  SFunctionReturnTypeRequired = 'Function %s must declare an explicit result type';
  SRedundantBooleanLiteral = 'Remove the redundant boolean literal from this comparison';
  SNilCheckViaAssigned = 'Use Assigned() instead of comparing to nil';
  SNoObjectTypes = 'Type %s uses the legacy object type; use class or record';
  SNoLegacyInitializationSection =
    'Replace the legacy begin..end unit body with an explicit initialization section';
  SInlineConstNoTypeInference = 'Declare an explicit type for this inline constant';
  SInlineLoopVarNoTypeInference =
    'Declare an explicit type for this inline loop variable';
  SInlineVarNoTypeInference = 'Declare an explicit type for inline variable %s';
  SProjectFileNoRoutines = 'Move routine %s out of the project file into a unit';
  SProjectFileNoVariables =
    'Move global variable %s out of the project file into a unit';

  // FpSonar.Rules.Tokens
  SLowercaseKeywords = 'Keyword should be lowercase: write %s';
  SCombineConstSections = 'Combine this const section with the previous one';
  SCombineTypeSections = 'Combine this type section with the previous one';
  SCombineVarSections = 'Combine this var section with the previous one';
  SDeclareFieldsIndividually = 'Declare each field in its own declaration';
  SDeclareVariablesIndividually = 'Declare each variable in its own declaration';
  SDeclareParametersIndividually = 'Declare each parameter in its own group';
  SNoEmptyParenthesesOnRoutines = 'Remove the empty parentheses';
  SNoStraySemicolons = 'Remove this stray semicolon';
  SNoOmittedSemicolons = 'Add the missing semicolon before this block terminator';
  SNoExtraneousCommas = 'Remove this extraneous comma';
  SNoDisabledCompilerHints = 'Do not disable compiler hints';
  SNoDisabledCompilerWarnings = 'Do not disable compiler warnings';
  SNoIndentUnitLevelKeywords = 'Unit-level keyword should start at column 1';
  SIndentVisibilitySpecifiers =
    'Indent this visibility specifier to the type declaration';
  SNoCommentedOutCode = 'Remove this commented-out code';
  STrackNoSonar = 'NOSONAR suppression used here';
  STrackComments = 'Tracked comment marker: %s';
  STrackStringLiterals = 'Tracked string literal pattern: %s';
  SCombineVisibilitySections = 'Merge this %s section with the preceding one';
  SRemoveEmptyVisibilitySection = 'Remove this empty %s visibility section';
  SRemoveEmptyFieldSection = 'Remove this empty field section';

  // FpSonar.Rules.Trackers
  SDisallowedImportByPath = 'Import of unit "%s" is disallowed. %s';
  SDisallowedConstant = 'Use of disallowed constant "%s". %s';
  SDisallowedEnumValue = 'Use of disallowed enum value "%s". %s';
  SDisallowedField = 'Use of disallowed field "%s". %s';
  SDisallowedIdentifier = 'Use of disallowed identifier "%s". %s';
  SDisallowedProperty = 'Use of disallowed property "%s". %s';
  SDisallowedRoutine = 'Call to disallowed routine "%s". %s';
  SDisallowedType = 'Use of disallowed type "%s". %s';
  STrackTypeAliases = 'Tracked type alias "%s". %s';

  // FpSonar.Rules.Unused
  SRemoveUnusedLocalVariable = 'Local variable "%s" is declared but never used';
  SRemoveUnusedField = 'Private field "%s" is never used';
  SRemoveUnusedProperty = 'Private property "%s" is never used';
  SRemoveUnusedConstant = 'Constant "%s" is never used';
  SRemoveUnusedRoutine = 'Private method "%s" is never used';
  SRemoveUnusedRoutinePublic =
    'Public routine "%s" is never used anywhere in the project';
  SRemoveUnusedType = 'Private type "%s" is never used';
  SRemoveUnusedTypePublic = 'Public type "%s" is never used anywhere in the project';
  SRemoveUnusedImports = 'Unit "%s" is in the uses clause but never used';
  SRemoveUnusedGlobalVariable = 'Global variable "%s" is never used';


implementation

end.
