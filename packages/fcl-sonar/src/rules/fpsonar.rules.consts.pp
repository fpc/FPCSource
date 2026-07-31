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
  SMethodHidesVirtualWithoutOverride =
    'Method %s hides a virtual ancestor method and is not declared override';
  SOverrideChangesDefaultParameterValue =
    'Override %s changes an inherited default parameter value';
  SAbstractMethodCalledDirectly = 'Abstract method %s is called directly';
  SInstantiatesClassWithAbstractMethods =
    'Class %s is instantiated but has an unimplemented abstract method';
  SInterfaceWithoutGuidUsedDynamically =
    'Interface %s is queried dynamically but declares no GUID';
  SSupportsResultIgnored = 'Result of %s is discarded';
  SClassHelperHidesAncestorMethod =
    'Class helper method %s hides a method of the extended type';
  SAssignedOnNonReference = 'Argument %s of Assigned is not a reference type';
  SPublicFieldAndPropertyForSameStorage =
    'Field %s is public and also exposed by a property';
  SPropertyAccessorVisibilityWiderThanProperty =
    'Property %s has an accessor with wider visibility';
  SPropertyGetterWithSideEffect = 'Getter of property %s writes to a field';
  SConstructorNotVirtualInPolymorphicHierarchy =
    'Constructor %s is not virtual and a descendant declares its own';
  SInheritedCreateNotFirstStatement =
    'Constructor %s does not call inherited first';
  SInheritedDestroyNotLastStatement =
    'Destructor %s does not call inherited last';
  SComparingClassReferencesWithEquals =
    'Class %s is compared by reference identity';

  // FpSonar.Rules.Concurrency
  SGlobalWrittenFromThreadRoutine =
    'Global %s is written in a thread routine with no critical section held';
  SSynchronizeWithLockHeld = '%s is called while critical section %s is held';
  SCriticalSectionNotInitialized =
    'Critical section %s is used without InitCriticalSection';
  SThreadvarInitialization =
    'Threadvar %s is never assigned outside unit initialization';
  SVclAccessOffMainThread = 'UI member %s is accessed in thread routine %s';

  // FpSonar.Rules.CondComp
  SEmptyConditionalBranch = 'Conditional branch on %s is empty';
  SNegatedConditionalWithEmptyElse =
    'Negated conditional on %s has an empty else branch';
  SHardcodedPathSeparator =
    'Path separator %s is hardcoded in a concatenation';
  SHardcodedLineEnding = 'Line ending %s is hardcoded';
  SPackedRecordFieldAlignmentAssumption =
    'Size of non-packed record %s is used as an I/O byte count';
  SAbsoluteVariableOverlay =
    'Variable %s overlays %s of a different declared size';
  SPointerSizedDatumTruncatedByByteCount =
    'Size of fixed-width type %s is used as the byte count of a pointer-sized '
    + 'datum';
  SUnknownConditionalSymbol =
    'Conditional symbol %s is neither defined for this analysis nor a known '
    + 'FPC or target symbol';
  SConditionalBranchNeverCompiled =
    'Branch guarded by %s is never compiled under the configured defines';

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
  SIdenticalBranches = 'Then and else branches are identical for condition %s';
  SDuplicateConditionInChain = 'Condition %s is repeated in the same if/else if chain';
  SDuplicateCaseLabel = 'Case label %s is used more than once';
  SSelfComparison = 'Operand %s is compared with itself using %s';
  SEmptyThenWithFollowingStatement =
    'Empty then branch on condition %s leaves the next statement unconditional';
  SMixedBooleanAndRelational = 'Boolean operator %s and comparison %s are mixed without parentheses';
  SBitwiseOnBooleanOperands = 'Operator %s mixes boolean and integer operands, or bit-tests an integer as a condition';
  SAssignmentInsteadOfComparison = 'Named argument %s assigns with := inside a call argument list';
  SConditionWithSideEffect = 'Call to %s modifies an argument in a short-circuited operand';
  SRedundantElseAfterExit = 'Else branch is redundant because the then branch ends with %s';
  SCollapsibleNestedIf = 'Nested if on condition %s can be merged with the enclosing condition';
  SNegatedConditionWithElse = 'Condition %s is negated while the if has an else branch';
  SSwitchOnBooleanExpression = 'Case selector %s is a Boolean expression';
  SLoopConditionNeverChanges =
    'No variable of loop condition %s is written in the loop body';
  SUnreachableCode = 'Statement cannot be reached';

  // FpSonar.Rules.DataFlow
  SUninitializedVariable = 'Variable %s is read before it is assigned a value';
  SDeadStore = 'Value assigned to %s is overwritten before it is read';
  SUninitializedVariableStrict =
    'Variable %s is not assigned on every path reaching this read';
  SSelfAssignedNeverUsed =
    'Variable %s is assigned a value derived from itself that is never read';
  SResultOverwrittenBeforeExit =
    'Value assigned to %s is overwritten before the function returns';

  // FpSonar.Rules.Eval
  SDivisionByZeroConstant = 'Divisor of the %s operation is a constant zero';
  SConstantConditionAlwaysTrueOrFalse =
    'Condition of the %s statement is always %s';
  SComparisonAlwaysTrueForType = 'Comparison of %s against %s is always %s';
  SConstantOutOfRangeForTarget = 'Constant %s is outside the range of %s';
  SConstantOverflowInExpression =
    'Result of the constant %s operation overflows the integer range';
  SShiftCountExceedsWidth = 'Shift count %s exceeds the width of %s';
  SSetElementOutOfRange =
    'Set constructor holds an element outside the range of %s';
  SEnumOrdinalOutOfRange =
    'Ordinal %s is outside the range of enumeration %s';
  SArrayIndexConstantOutOfBounds =
    'Array index %s is outside the bounds of %s';
  SSizeOfOnReferenceType =
    'SizeOf of reference type %s yields the pointer size';
  SMoveFillCharSizeMismatch =
    'Byte count passed to %s uses %s, which is not the size of the data in bytes';
  SFloatEqualityComparison =
    'Exact equality comparison on floating-point type %s';
  SIntegerDivisionAssignedToFloat =
    'Integer division assigned to floating-point type %s discards the remainder';
  SMixedSignedUnsignedComparison =
    'Comparison mixes signed %s with unsigned %s of the same width';

  // FpSonar.Rules.Exceptions
  SNoEmptyFinally = 'finally block is empty';
  SExceptionsNotSwallowed = 'Exception is swallowed by an empty except handler';
  SNoExplicitReRaise = 'Use a bare raise instead of re-raising %s';
  SExitInsideFinally = 'exit in a finally block discards the in-flight exception';
  SRaiseInsideFinally = 'raise in a finally block replaces the in-flight exception';
  SHandlerOrderShadowsDerived = 'Handler for %s is unreachable because an earlier handler catches %s';
  STryFinallyAcquireOutsideTry = 'Resource %s is acquired inside the try block that releases it';
  SExceptionClassNotDerivedFromException = 'Raised class %s does not descend from Exception';
  SEmptyTryBody = 'try block is empty, so its handler guards nothing';
  SRaiseInDestructor = 'Unguarded raise in destructor %s can abort destruction';
  SAssertUsedForControlFlow = 'Assert argument calls %s, which is removed under {$C-}';

  // FpSonar.Rules.Forms
  SLfmFormFileExists = 'This form/frame/datamodule unit has no sibling .lfm file.';

  // FpSonar.Rules.FpcStyle
  SDottedUnitsBranchesInconsistent =
    'Unit %s is listed in only one FPC_DOTTEDUNITS branch';
  SMissingDottedUnitsGuard = 'Uses clause has no FPC_DOTTEDUNITS guard';
  SDottedUnitAliasMismatch =
    'Unit %s is not aliased as %s in the FPC_DOTTEDUNITS branch';
  SUnitFileNameCaseMismatch =
    'File name %s is not the lowercase form of unit name %s';
  SMissingModeDirective = 'Module %s has no {$mode} directive';
  SMissingCopyrightHeader =
    'Module %s has no COPYING.FPC reference in its leading comment';
  SDeprecatedSymbolUsed = 'Symbol %s is deprecated';
  SPlatformSymbolUsedInPortableUnit =
    'Symbol %s is platform-specific and this unit is not marked platform';
  SExperimentalSymbolUsed = 'Symbol %s is experimental';
  SPublicMethodUndocumented =
    'Method %s has no preceding documentation comment';
  SPublicPropertyUndocumented =
    'Property %s has no preceding documentation comment';
  SInterfaceUsesTooBroad =
    'Interface uses clause names %s, which no interface declaration references';
  SIOResultNotChecked =
    'Call to %s under {$I-} is not followed by an IOResult check';

  // FpSonar.Rules.Generics
  SGenericConstraintUnused =
    'Generic parameter %s declares a constraint the generic never relies on';
  SSpecializationOfUnconstrainedGeneric =
    'Specialization of %s constrains no type parameter';
  SNestedGenericSpecializationDepth =
    'Specialization of %s nests %s levels; the maximum allowed is %s';
  SAnonymousMethodCapturesLoopVariable =
    'Anonymous method captures loop variable %s, which the loop reuses on '
    + 'every iteration';
  SAnonymousMethodCapturesSelf =
    'Anonymous method in %s captures Self, so it is only valid while the '
    + 'instance lives';
  SAttributeOnNonRttiMember =
    'Attribute on %s, which no RTTI reaches because the member is not '
    + 'published';

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

  // FpSonar.Rules.Lifetime
  SFreeOnInterfaceReference = 'Free applied to a reference of interface type %s';
  SSelfDestroyedInMethod = 'Method %s frees Self outside a destructor';
  SNewDisposeMismatch = 'Pointer %s is allocated with %s and released with %s';
  SOwnedFieldNotFreedInDestructor = 'Field %s is created in a constructor but not released in %s';
  SCreateWithoutTryFinally = 'Instance %s is created and released without a protecting try..finally';
  SExceptionObjectFreedInHandler = 'Exception object %s is freed inside the handler that caught it';
  SRaisedExceptionInstanceReused = 'Exception instance %s is referenced after it is raised';
  SLoopVariableUsedAfterLoop = 'Loop variable %s is read after its loop has ended';
  SLoopVariableModifiedInBody = 'Loop variable %s is assigned inside its own loop body';
  SLeakOnEarlyExit = 'Instance %s is not released on this exit path';
  SStreamNotProtected = 'Stream %s is created without a protecting try..finally';
  SUseAfterFree = 'Reference %s is read after it is released on this path';
  SDoubleFree = 'Reference %s is released again with no intervening assignment';
  SFreeNotFreeAndNilOnField = 'Field %s is freed without being nilled and is read afterwards';
  SGetMemWithoutFreeMem = 'Pointer %s is allocated and never released in this routine';
  SObjectCreatedInLoopNotFreed = 'Instance %s is created in a loop that does not release it';
  SUnbalancedPair = '%s is acquired without a matching %s in a finally';

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

  // FpSonar.Rules.Strings
  SPCharOfTemporaryString =
    'Cast to %s points into a temporary string freed at the end of the statement';
  SImplicitStringConversionWithDataLoss =
    'Implicit conversion from %s to %s loses characters';
  SLengthUsedAsByteCount = 'Length of %s counts characters, not the bytes %s expects';
  SCopyWithZeroIndex =
    'Copy of a %s starts at index 0, but string indices start at 1';
  SPosResultComparedToZeroBased =
    'Result of %s is compared to %s, but it returns 0 when the substring is absent';
  SShortStringTruncation =
    'Constant assigned to %s is longer than the %s characters it holds';
  SCharComparedToString =
    'A %s is compared to a string constant of %s characters, which is never equal';
  SRawByteStringCodePageMix =
    'RawByteString %s is mixed with a code page %s string and no conversion is written';
  SStringConcatInLoop =
    'String %s is rebuilt by concatenation on every iteration of the enclosing loop';
  SStrToIntWithoutGuard =
    'StrToInt on %s raises when the text is not an integer, and no handler guards the call';
  SWideStringOnNonWindows =
    'Declaration %s is a WideString, which is a COM BSTR only on Windows';
  SSetLengthWithoutFill =
    '%s is indexed after SetLength with no intervening write';

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
  SRemoveUnusedParameter = 'Parameter "%s" is never used in the routine body';
  SParameterAssignedButNeverUsed =
    'Value parameter "%s" is assigned but its value is never used';
  SUnusedExceptionVariable = 'Exception variable "%s" is never used in the handler';
  SUnusedLabel = 'Label "%s" is declared but no goto targets it';
  SUnusedGenericParameter =
    'Generic type parameter "%s" is never used in the generic body';
  SUnusedUnitInInterface = 'Unit "%s" is used only in the implementation '
    + 'section but imported in the interface';
  SPrivateMemberOnlyUsedByOneMethod =
    'Private member "%s" is referenced by only one method';
  SWriteOnlyVariable = 'Variable "%s" is written but never read';


implementation

end.
