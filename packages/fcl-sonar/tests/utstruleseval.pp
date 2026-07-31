{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the constant-evaluation (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesEval;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Eval, UtstFixtures;

type
  { SEM-tier constant-evaluation rule position, degradation and registration
    tests. }
  TRulesEvalTest = class(TTestCase)
  private
    { Runs aRule (taken into a fresh local registry, freed here) over aFixture
      with resolution intact, collecting into aCollector (caller-owned).
      A non-zero aPointerSize selects the target width of a resolving run. }
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      aWithhold: boolean; const aCollector: TFpSonarIssueCollector;
      aPointerSize: integer = 0);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // How often aRule fires on aSource, staged as its own fixture. aWithhold
    // withholds resolution the way the silence sweep's degraded pass does.
    function RuleCount(aRule: TRuleBase; aWithhold: boolean;
      const aSource: array of string): Integer;
    // Asserts aRule fires once at aLine, column 1, with key rule.<aId>.message
    // and message args = aArgs, and zero on aCompliant.
    procedure CheckEvalRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewDivisionByZeroConstant: TRuleBase;
    function NewConstantConditionAlwaysTrueOrFalse: TRuleBase;
    function NewComparisonAlwaysTrueForType: TRuleBase;
    function NewConstantOutOfRangeForTarget: TRuleBase;
    function NewConstantOverflowInExpression: TRuleBase;
    function NewShiftCountExceedsWidth: TRuleBase;
    function NewSetElementOutOfRange: TRuleBase;
    function NewEnumOrdinalOutOfRange: TRuleBase;
    function NewArrayIndexConstantOutOfBounds: TRuleBase;
    function NewSizeOfOnReferenceType: TRuleBase;
    function NewMoveFillCharSizeMismatch: TRuleBase;
    function NewFloatEqualityComparison: TRuleBase;
    function NewIntegerDivisionAssignedToFloat: TRuleBase;
    function NewMixedSignedUnsignedComparison: TRuleBase;
    { Asserts aRule is silent on aSource while the division-by-zero sibling
      fires once on that same file. }
    procedure CheckSilentWithLiveSibling(aRule: TRuleBase; const aId: string;
      const aSource: array of string);
  published
    procedure DivisionByZeroConstantPositions;
    procedure DivisionByZeroConstantFloatDivisor;
    procedure DivisionByZeroConstantModuloDivisor;
    procedure DivisionByZeroConstantDegradesWithoutResolver;
    procedure DivisionByZeroConstantSilentOnUnresolvedOperand;
    procedure ConstantConditionAlwaysTrueOrFalsePositions;
    procedure ConstantConditionAlwaysTrueOrFalseReportsLoopKeywords;
    procedure ConstantConditionAlwaysTrueOrFalseInfiniteLoopsAreSilent;
    procedure ConstantConditionAlwaysTrueOrFalseDegradesWithoutResolver;
    procedure ConstantConditionAlwaysTrueOrFalseSilentOnUnresolvedOperand;
    procedure ConstantConditionAlwaysTrueOrFalseMeasuresIntrinsicFold;
    procedure ComparisonAlwaysTrueForTypePositions;
    procedure ComparisonAlwaysTrueForTypeUnsignedLowerBound;
    procedure ComparisonAlwaysTrueForTypeDegradesWithoutResolver;
    procedure ComparisonAlwaysTrueForTypeSilentOnUnresolvedOperand;
    procedure ConstantOutOfRangeForTargetPositions;
    procedure ConstantOutOfRangeForTargetIsTargetDependent;
    procedure ConstantOutOfRangeForTargetDegradesWithoutResolver;
    procedure ConstantOutOfRangeForTargetSilentOnUnresolvedOperand;
    procedure ConstantOverflowInExpressionPositions;
    procedure ConstantOverflowInExpressionDegradesWithoutResolver;
    procedure ConstantOverflowInExpressionSilentOnUnresolvedOperand;
    procedure ShiftCountExceedsWidthPositions;
    procedure ShiftCountExceedsWidthDegradesWithoutResolver;
    procedure ShiftCountExceedsWidthSilentOnUnresolvedOperand;
    procedure SetElementOutOfRangePositions;
    procedure SetElementOutOfRangeDegradesWithoutResolver;
    procedure SetElementOutOfRangeSilentOnUnresolvedOperand;
    procedure EnumOrdinalOutOfRangePositions;
    procedure EnumOrdinalOutOfRangeDegradesWithoutResolver;
    procedure EnumOrdinalOutOfRangeSilentOnUnresolvedOperand;
    procedure ArrayIndexConstantOutOfBoundsPositions;
    procedure ArrayIndexConstantOutOfBoundsDegradesWithoutResolver;
    procedure ArrayIndexConstantOutOfBoundsSilentOnUnresolvedOperand;
    procedure SizeOfOnReferenceTypePositions;
    procedure SizeOfOnReferenceTypeReportsStringAndDynamicArray;
    procedure SizeOfOnReferenceTypeIgnoresUserRoutine;
    procedure SizeOfOnReferenceTypeDegradesWithoutResolver;
    procedure SizeOfOnReferenceTypeSilentOnUnresolvedOperand;
    procedure MoveFillCharSizeMismatchPositions;
    procedure MoveFillCharSizeMismatchReportsPointerCount;
    procedure MoveFillCharSizeMismatchReadsFillCharSecondArgument;
    procedure MoveFillCharSizeMismatchIgnoresTypedRoutine;
    procedure MoveFillCharSizeMismatchDegradesWithoutResolver;
    procedure MoveFillCharSizeMismatchSilentOnUnresolvedOperand;
    procedure FloatEqualityComparisonPositions;
    procedure FloatEqualityComparisonReportsInequality;
    procedure FloatEqualityComparisonDegradesWithoutResolver;
    procedure FloatEqualityComparisonSilentOnUnresolvedOperand;
    procedure IntegerDivisionAssignedToFloatPositions;
    procedure IntegerDivisionAssignedToFloatDegradesWithoutResolver;
    procedure IntegerDivisionAssignedToFloatSilentOnUnresolvedOperand;
    procedure MixedSignedUnsignedComparisonPositions;
    procedure MixedSignedUnsignedComparisonNamesSignedOperandFirst;
    procedure MixedSignedUnsignedComparisonDegradesWithoutResolver;
    procedure MixedSignedUnsignedComparisonSilentOnUnresolvedOperand;
    procedure EvalHarnessSurvivesRangeErrors;
    procedure EvalRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cDivisionByZeroConstantId = 'DivisionByZeroConstant';
  cConstantConditionAlwaysTrueOrFalseId = 'ConstantConditionAlwaysTrueOrFalse';
  cComparisonAlwaysTrueForTypeId = 'ComparisonAlwaysTrueForType';
  cConstantOutOfRangeForTargetId = 'ConstantOutOfRangeForTarget';
  cConstantOverflowInExpressionId = 'ConstantOverflowInExpression';
  cShiftCountExceedsWidthId = 'ShiftCountExceedsWidth';
  cSetElementOutOfRangeId = 'SetElementOutOfRange';
  cEnumOrdinalOutOfRangeId = 'EnumOrdinalOutOfRange';
  cArrayIndexConstantOutOfBoundsId = 'ArrayIndexConstantOutOfBounds';
  cSizeOfOnReferenceTypeId = 'SizeOfOnReferenceType';
  cMoveFillCharSizeMismatchId = 'MoveFillCharSizeMismatch';
  cFloatEqualityComparisonId = 'FloatEqualityComparison';
  cIntegerDivisionAssignedToFloatId = 'IntegerDivisionAssignedToFloat';
  cMixedSignedUnsignedComparisonId = 'MixedSignedUnsignedComparison';

  // Embedded fixtures: line i+1 == [i].

  cDivByZeroNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  x, y: Integer;',
    'begin',
    '  x := 10;',
    '  y := x div 0;',
    'end;',
    'end.');

  cDivByZeroCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  x, y, n: Integer;',
    'begin',
    '  x := 10;',
    '  n := 2;',
    '  y := x div n;',
    'end;',
    'end.');

  cDivByZeroFloat: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f, g: Double;',
    'begin',
    '  g := 1.0;',
    '  f := g / 0.0;',
    'end;',
    'end.');

  // A non-zero float divisor: the negative control for the revkFloat test.
  cDivByZeroFloatCompliant: array[0..15] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f, g: Double;',
    'begin',
    '  g := 1.0;',
    '  f := g / 2.0;',
    'end;',
    'end.');

  cDivByZeroModulo: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  x, y: Integer;',
    'begin',
    '  x := 10;',
    '  y := x mod 0;',
    'end;',
    'end.');

  // The divisor is a function result; the constant condition is the control.
  cDivByZeroUnresolvedOperand: array[0..24] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'const',
    '  cAlways = True;',
    '',
    'function Divisor: Integer;',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 10 div Divisor;',
    '  if cAlways then',
    '    y := 1;',
    'end;',
    'end.');

  cConstantConditionNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'const',
    '  cDebug = False;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  if cDebug then',
    '    y := 1;',
    'end;',
    'end.');

  cConstantConditionCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  i, y: Integer;',
    'begin',
    '  i := 1;',
    '  if i > 0 then',
    '    y := 1;',
    'end;',
    'end.');

  cConstantConditionInfiniteLoops: array[0..19] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 0;',
    '  while True do',
    '    Break;',
    '  repeat',
    '    Inc(y);',
    '  until False;',
    'end;',
    'end.');

  cConstantConditionFalseWhile: array[0..19] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'const',
    '  cNever = False;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 0;',
    '  while cNever do',
    '    Inc(y);',
    'end;',
    'end.');

  cConstantConditionTrueRepeat: array[0..20] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'const',
    '  cAlways = True;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 0;',
    '  repeat',
    '    Inc(y);',
    '  until cAlways;',
    'end;',
    'end.');

  // The condition is a function result; the zero divisor is the control.
  cConstantConditionUnresolvedOperand: array[0..21] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Flag: Boolean;',
    'begin',
    '  Result := True;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  x, y: Integer;',
    'begin',
    '  x := 3;',
    '  if Flag then',
    '    y := x div 0;',
    'end;',
    'end.');

  { A range error in an excluded branch and one in a compiled unused constant
    under {$R+}, each paired with a constant condition, plus the division. }
  cRangeErrorSurvival: array[0..34] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    '{$IFDEF NEVERDEFINED}',
    'const',
    '  cExcluded: byte = 300;',
    '  cAlwaysTrue = True;',
    '',
    'procedure Dead;',
    'begin',
    '  if cAlwaysTrue then',
    '    Halt;',
    'end;',
    '{$ENDIF}',
    '',
    '{$R+}',
    'const',
    '  cUnused: byte = 300;',
    '  cCompiled = False;',
    '',
    'procedure Run;',
    'var',
    '  x, y: Integer;',
    'begin',
    '  x := 10;',
    '  y := x div 0;',
    '  if cCompiled then',
    '    y := 0;',
    'end;',
    'end.');

  // `if SizeOf(Pointer) = 8 then`: the target-dependent intrinsic fold.
  cIntrinsicFold: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 0;',
    '  if SizeOf(Pointer) = 8 then',
    '    y := 1;',
    'end;',
    'end.');


  cCompareNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  if b > 255 then',
    '    y := 1;',
    'end;',
    'end.');

  // The last two conditions are computed at 32 bits, so neither is type-fixed.
  cCompareCompliant: array[0..22] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b, c: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  c := 2;',
    '  if b > 100 then',
    '    y := 1;',
    '  if b + c > 255 then',
    '    y := 2;',
    '  if -b < 0 then',
    '    y := 3;',
    'end;',
    'end.');

  // The unsigned lower bound: the constant is in range and the verdict is not.
  cCompareLowerBound: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  if b < 0 then',
    '    y := 1;',
    'end;',
    'end.');

  cCompareUnresolvedOperand: array[0..23] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Limit: Byte;',
    'begin',
    '  Result := 255;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  if b > Limit then',
    '    y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  cOutOfRangeNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    'begin',
    '  b := 300;',
    'end;',
    'end.');

  cOutOfRangeCompliant: array[0..14] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    'begin',
    '  b := 200;',
    'end;',
    'end.');

  // 1024 div SizeOf(Pointer) is 128 on a 64-bit target and 256 on a 32-bit one.
  cOutOfRangeTargetDependent: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'const',
    '  cWords = 1024 div SizeOf(Pointer);',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    'begin',
    '  b := cWords;',
    'end;',
    'end.');

  cOutOfRangeUnresolvedOperand: array[0..21] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Wide: Byte;',
    'begin',
    '  Result := 200;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := Wide;',
    '  y := y div 0;',
    'end;',
    'end.');

  { The overflow sits in a condition, not on the right of an assignment: the
    resolver folds an assigned constant eagerly and the raise fails the unit. }
  cOverflowNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  y: Int64;',
    'begin',
    '  y := 0;',
    '  if High(Int64) * High(Int64) > 0 then',
    '    y := 1;',
    'end;',
    'end.');

  // The evaluator retries the add in TMaxPrecUInt and succeeds.
  cOverflowCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  y: Int64;',
    'begin',
    '  y := 0;',
    '  if High(Int64) + 1 > 0 then',
    '    y := 1;',
    'end;',
    'end.');

  cOverflowUnresolvedOperand: array[0..20] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Wide: Int64;',
    'begin',
    '  Result := 2;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  y: Int64;',
    'begin',
    '  y := Wide * High(Int64);',
    '  y := y div 0;',
    'end;',
    'end.');

  cShiftNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  l, y: Longint;',
    'begin',
    '  l := 1;',
    '  y := l shl 40;',
    'end;',
    'end.');

  cShiftCompliant: array[0..18] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  l, y: Longint;',
    'begin',
    '  b := 1;',
    '  l := 1;',
    '  y := l shl 8;',
    '  y := b shl 16;',
    'end;',
    'end.');

  cShiftUnresolvedOperand: array[0..21] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Count: Integer;',
    'begin',
    '  Result := 40;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  l, y: Longint;',
    'begin',
    '  l := 1;',
    '  y := l shl Count;',
    '  y := y div 0;',
    'end;',
    'end.');

  cSetNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  if b in [1, 300] then',
    '    y := 1;',
    'end;',
    'end.');

  cSetCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  if b in [1, 200] then',
    '    y := 1;',
    'end;',
    'end.');

  cSetUnresolvedOperand: array[0..23] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Mask: Byte;',
    'begin',
    '  Result := 3;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  y: Integer;',
    'begin',
    '  b := 1;',
    '  if b in [1, Mask] then',
    '    y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  { The cast sits in a condition for the same reason as the overflow fixture:
    an assigned out-of-range ordinal fails the unit inside the resolver. }
  cEnumNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TThree = (tA, tB, tC);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 0;',
    '  if TThree(7) = tA then',
    '    y := 1;',
    'end;',
    'end.');

  cEnumCompliant: array[0..19] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TThree = (tA, tB, tC);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 0;',
    '  if TThree(2) = tA then',
    '    y := 1;',
    'end;',
    'end.');

  cEnumUnresolvedOperand: array[0..24] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TThree = (tA, tB, tC);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Ordinal: Integer;',
    'begin',
    '  Result := 7;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  e: TThree;',
    '  y: Integer;',
    'begin',
    '  e := TThree(Ordinal);',
    '  y := y div 0;',
    'end;',
    'end.');

  cArrayNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  a: array[0..3] of Integer;',
    'begin',
    '  a[10] := 1;',
    'end;',
    'end.');

  // The dynamic array carries the no-static-range row of the matrix.
  cArrayCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  a: array[0..3] of Integer;',
    '  d: array of Integer;',
    'begin',
    '  a[2] := 1;',
    '  d[10] := 1;',
    'end;',
    'end.');

  cArrayUnresolvedOperand: array[0..21] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function Index: Integer;',
    'begin',
    '  Result := 10;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  a: array[0..3] of Integer;',
    '  y: Integer;',
    'begin',
    '  a[Index] := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  cSizeOfNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  o: TObject;',
    '  n: Integer;',
    'begin',
    '  n := SizeOf(o);',
    'end;',
    'end.');

  // ShortString, a record and an ordinal type are all value types, and SizeOf
  // over a type name is not over a variable.
  cSizeOfCompliant: array[0..24] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'type',
    '  TRec = record',
    '    Value: Integer;',
    '  end;',
    '',
    'procedure Run;',
    'var',
    '  s: ShortString;',
    '  r: TRec;',
    '  n: Integer;',
    'begin',
    '  n := SizeOf(s);',
    '  n := SizeOf(r);',
    '  n := SizeOf(Integer);',
    '  n := SizeOf(TObject);',
    'end;',
    'end.');

  cSizeOfStringAndDynArray: array[0..25] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}{$interfaces corba}',
    'interface',
    '',
    'type',
    '  TIntArray = array of Integer;',
    '  IThing = interface',
    '    procedure Go;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  d: TIntArray;',
    '  s: AnsiString;',
    '  t: IThing;',
    '  n: Integer;',
    'begin',
    '  n := SizeOf(d);',
    '  n := SizeOf(s);',
    '  n := SizeOf(t);',
    'end;',
    'end.');

  // The unit's own SizeOf shadows the built-in one for the whole unit.
  cSizeOfUserRoutine: array[0..21] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'function SizeOf(aItem: TObject): Integer;',
    'begin',
    '  Result := 4;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  o: TObject;',
    '  n: Integer;',
    'begin',
    '  n := SizeOf(o);',
    '  n := n div 0;',
    'end;',
    'end.');

  // An inline dynamic array type has no name to report.
  cSizeOfUnresolvedOperand: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  d: array of Integer;',
    '  n, y: Integer;',
    'begin',
    '  n := SizeOf(d);',
    '  y := n div 0;',
    'end;',
    'end.');

  { Move and FillChar are ordinary system.pp declarations, not resolver
    built-ins, and the synthetic System unit declares neither. }
  cMoveNoncompliant: array[0..22] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TIntArray = array of Integer;',
    '',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  a, b: TIntArray;',
    'begin',
    '  Move(a[0], b[0], Length(a));',
    'end;',
    'end.');

  cMoveCompliant: array[0..48] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TIntArray = array of Integer;',
    '  TByteArray = array of Byte;',
    '  TRec = record',
    '    Value: Integer;',
    '  end;',
    '  PRec = ^TRec;',
    '  TGrid = class',
    '    procedure Move(aFrom, aTo, aCount: Integer);',
    '  end;',
    '',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    '',
    'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
    'begin',
    'end;',
    '',
    'procedure TGrid.Move(aFrom, aTo, aCount: Integer);',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  a, b: TIntArray;',
    '  u, v: TByteArray;',
    '  p, q: PRec;',
    '  g: TGrid;',
    'begin',
    '  Move(a[0], b[0], Length(a) * SizeOf(Integer));',
    '  Move(u[0], v[0], Length(u));',
    '  Move(p^, q^, SizeOf(PRec));',
    '  Move(p, q, SizeOf(p));',
    '  g.Move(0, 1, Length(a));',
    '  FillChar(a[0], Length(a) * SizeOf(Integer), 0);',
    '  FillChar(p, SizeOf(p), 0);',
    'end;',
    'end.');

  // A unit-level routine named Move that is not the RTL one: its first
  // argument is typed.
  cMoveTypedRoutine: array[0..25] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TIntArray = array of Integer;',
    '',
    'procedure Move(aFrom, aTo, aCount: Integer);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Move(aFrom, aTo, aCount: Integer);',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  a: TIntArray;',
    '  y: Integer;',
    'begin',
    '  Move(0, 1, Length(a));',
    '  y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  cMovePointerCount: array[0..25] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TRec = record',
    '    Value: Integer;',
    '  end;',
    '  PRec = ^TRec;',
    '',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  p, q: PRec;',
    'begin',
    '  Move(p^, q^, SizeOf(p));',
    'end;',
    'end.');

  cFillCharCount: array[0..22] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TIntArray = array of Integer;',
    '',
    'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  a: TIntArray;',
    'begin',
    '  FillChar(a[0], Length(a), 0);',
    'end;',
    'end.');

  // A call through a procedural variable resolves to no procedure declaration.
  cMoveUnresolvedOperand: array[0..24] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TIntArray = array of Integer;',
    '  TMoveProc = procedure(const aSource; var aDest; aCount: SizeInt);',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'var',
    '  Mover: TMoveProc;',
    '',
    'procedure Run;',
    'var',
    '  a, b: TIntArray;',
    '  y: Integer;',
    'begin',
    '  Mover(a[0], b[0], Length(a));',
    '  y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  cFloatEqualNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f: Double;',
    '  y: Integer;',
    'begin',
    '  if f = 0.0 then',
    '    y := 1;',
    'end;',
    'end.');

  // Ordering is well defined on floats, and integer equality is exact.
  cFloatEqualCompliant: array[0..21] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f: Double;',
    '  m: Currency;',
    '  i, y: Integer;',
    'begin',
    '  if f > 0.0 then',
    '    y := 1;',
    '  if i = 0 then',
    '    y := 1;',
    '  if m = 0 then',
    '    y := 1;',
    'end;',
    'end.');

  cFloatNotEqual: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f: Double;',
    '  y: Integer;',
    'begin',
    '  if f <> 0.0 then',
    '    y := 1;',
    'end;',
    'end.');

  // A generic template operand carries no type until the specialization.
  cFloatEqualUnresolvedOperand: array[0..22] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'generic function Same<T>(const aLeft, aRight: T): boolean;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'generic function Same<T>(const aLeft, aRight: T): boolean;',
    'begin',
    '  Result := aLeft = aRight;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  cIntDivNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f: Double;',
    '  i, j: Integer;',
    'begin',
    '  f := i div j;',
    'end;',
    'end.');

  cIntDivCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  f: Double;',
    '  i, j, k: Integer;',
    'begin',
    '  f := i / j;',
    '  k := i div j;',
    'end;',
    'end.');

  cIntDivUnresolvedOperand: array[0..22] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'generic function Ratio<T>(const aLeft, aRight: T): Double;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'generic function Ratio<T>(const aLeft, aRight: T): Double;',
    'begin',
    '  Result := aLeft div aRight;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');

  cMixedSignNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  l: Longint;',
    '  c: LongWord;',
    '  y: Integer;',
    'begin',
    '  if l < c then',
    '    y := 1;',
    'end;',
    'end.');

  { The sub-32-bit pair widens to a signed 32-bit value, the computed operand
    is already promoted, the char pair reaches the width test as a non-integer
    and the constant operand is decided by its value. }
  cMixedSignCompliant: array[0..30] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  b: Byte;',
    '  s: ShortInt;',
    '  l, m: Longint;',
    '  c: LongWord;',
    '  ch, cj: AnsiChar;',
    '  y: Integer;',
    'begin',
    '  if b < s then',
    '    y := 1;',
    '  if l < m then',
    '    y := 1;',
    '  if l + 1 < c then',
    '    y := 1;',
    '  if ch = cj then',
    '    y := 1;',
    '  if c > 0 then',
    '    y := 1;',
    '  if (l and $FF) < c then',
    '    y := 1;',
    'end;',
    'end.');

  // The same pair with the unsigned operand on the left.
  cMixedSignReversed: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    'var',
    '  l: Longint;',
    '  c: LongWord;',
    '  y: Integer;',
    'begin',
    '  if c > l then',
    '    y := 1;',
    'end;',
    'end.');

  cMixedSignUnresolvedOperand: array[0..22] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'generic function Below<T>(const aLeft, aRight: T): boolean;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'generic function Below<T>(const aLeft, aRight: T): boolean;',
    'begin',
    '  Result := aLeft < aRight;',
    'end;',
    '',
    'procedure Run;',
    'var',
    '  y: Integer;',
    'begin',
    '  y := 1;',
    '  y := y div 0;',
    'end;',
    'end.');


procedure TRulesEvalTest.RunRule(aRule: TRuleBase; const aFixture: string;
  aWithhold: boolean; const aCollector: TFpSonarIssueCollector;
  aPointerSize: integer = 0);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lConfig: TFpSonarConfig;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    // Both eval rules ship disabled; the dispatcher gets an explicit enable.
    lConfig := TFpSonarConfig.Default;
    // Rebuilt, not resized: an entry Default ever seeds would keep its params.
    SetLength(lConfig.Rules, 0);
    SetLength(lConfig.Rules, 1);
    lConfig.Rules[0].RuleId := aRule.Metadata.RuleId;
    lConfig.Rules[0].HasEnabled := True;
    lConfig.Rules[0].Enabled := True;
    lEngine.Config := lConfig;
    if aWithhold then
      // The silence sweep's degraded pass: real-RTL chain, no unit paths.
      lEngine.Analyze(aFixture, cMode, cDefines, [], [], True, SizeOf(Pointer),
        aCollector)
    else if aPointerSize <> 0 then
      lEngine.Analyze(aFixture, cMode, cDefines, [ExtractFileDir(aFixture)],
        [ExtractFileDir(aFixture)], False, aPointerSize, aCollector)
    else
      lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesEvalTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesEvalTest.FirstById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Exit(i);
end;


function TRulesEvalTest.RuleCount(aRule: TRuleBase; aWithhold: boolean;
  const aSource: array of string): Integer;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lId: string;

begin
  // The registry RunRule builds owns and frees aRule, so read the id first.
  lId := aRule.Metadata.RuleId;
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('probe.pas', aSource), aWithhold, lc);
      Result := CountById(lc, lId);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesEvalTest.CheckEvalRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), False, lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('arg count', Length(aArgs), Length(lc.Issues[k].MessageArgs));
      for m := 0 to High(aArgs) do
        AssertEquals('arg ' + IntToStr(m), aArgs[m],
          lc.Issues[k].MessageArgs[m]);
    finally
      lc.Free;
    end;

    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), False, lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesEvalTest.NewDivisionByZeroConstant: TRuleBase;

begin
  Result := TRuleDivisionByZeroConstant.Create(TRuleMetadata.Make(
    cDivisionByZeroConstantId, rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, ''));
end;


function TRulesEvalTest.NewConstantConditionAlwaysTrueOrFalse: TRuleBase;

begin
  Result := TRuleConstantConditionAlwaysTrueOrFalse.Create(TRuleMetadata.Make(
    cConstantConditionAlwaysTrueOrFalseId, rtSem, rfResolver, sevMajor,
    itCodeSmell, cfMedium, False, ''));
end;


function TRulesEvalTest.NewComparisonAlwaysTrueForType: TRuleBase;

begin
  Result := TRuleComparisonAlwaysTrueForType.Create(TRuleMetadata.Make(
    cComparisonAlwaysTrueForTypeId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, False, ''));
end;


function TRulesEvalTest.NewConstantOutOfRangeForTarget: TRuleBase;

begin
  Result := TRuleConstantOutOfRangeForTarget.Create(TRuleMetadata.Make(
    cConstantOutOfRangeForTargetId, rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, ''));
end;


function TRulesEvalTest.NewConstantOverflowInExpression: TRuleBase;

begin
  Result := TRuleConstantOverflowInExpression.Create(TRuleMetadata.Make(
    cConstantOverflowInExpressionId, rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, ''));
end;


function TRulesEvalTest.NewShiftCountExceedsWidth: TRuleBase;

begin
  Result := TRuleShiftCountExceedsWidth.Create(TRuleMetadata.Make(
    cShiftCountExceedsWidthId, rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, ''));
end;


function TRulesEvalTest.NewSetElementOutOfRange: TRuleBase;

begin
  Result := TRuleSetElementOutOfRange.Create(TRuleMetadata.Make(
    cSetElementOutOfRangeId, rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, ''));
end;


function TRulesEvalTest.NewEnumOrdinalOutOfRange: TRuleBase;

begin
  Result := TRuleEnumOrdinalOutOfRange.Create(TRuleMetadata.Make(
    cEnumOrdinalOutOfRangeId, rtSem, rfResolver, sevCritical, itBug, cfHigh,
    False, ''));
end;


function TRulesEvalTest.NewArrayIndexConstantOutOfBounds: TRuleBase;

begin
  Result := TRuleArrayIndexConstantOutOfBounds.Create(TRuleMetadata.Make(
    cArrayIndexConstantOutOfBoundsId, rtSem, rfResolver, sevCritical, itBug,
    cfHigh, False, ''));
end;


function TRulesEvalTest.NewSizeOfOnReferenceType: TRuleBase;

begin
  Result := TRuleSizeOfOnReferenceType.Create(TRuleMetadata.Make(
    cSizeOfOnReferenceTypeId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, ''));
end;


function TRulesEvalTest.NewMoveFillCharSizeMismatch: TRuleBase;

begin
  Result := TRuleMoveFillCharSizeMismatch.Create(TRuleMetadata.Make(
    cMoveFillCharSizeMismatchId, rtSem, rfResolver, sevCritical, itBug,
    cfMedium, False, ''));
end;


function TRulesEvalTest.NewFloatEqualityComparison: TRuleBase;

begin
  Result := TRuleFloatEqualityComparison.Create(TRuleMetadata.Make(
    cFloatEqualityComparisonId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, False, ''));
end;


function TRulesEvalTest.NewIntegerDivisionAssignedToFloat: TRuleBase;

begin
  Result := TRuleIntegerDivisionAssignedToFloat.Create(TRuleMetadata.Make(
    cIntegerDivisionAssignedToFloatId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, ''));
end;


function TRulesEvalTest.NewMixedSignedUnsignedComparison: TRuleBase;

begin
  Result := TRuleMixedSignedUnsignedComparison.Create(TRuleMetadata.Make(
    cMixedSignedUnsignedComparisonId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, False, ''));
end;


procedure TRulesEvalTest.CheckSilentWithLiveSibling(aRule: TRuleBase;
  const aId: string; const aSource: array of string);

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('operand.pas', aSource);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lPath, False, lc);
      AssertEquals('an unfoldable operand is silent', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDivisionByZeroConstant, lPath, False, lc);
      AssertEquals('the resolver was live', 1,
        CountById(lc, cDivisionByZeroConstantId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesEvalTest.DivisionByZeroConstantPositions;

begin
  // Noncompliant: 'y := x div 0;' on line 14. Compliant: a variable divisor,
  // which the evaluator cannot fold => silent.
  CheckEvalRuleSrc(NewDivisionByZeroConstant, NewDivisionByZeroConstant,
    cDivisionByZeroConstantId, 14, ['div'],
    cDivByZeroNoncompliant, cDivByZeroCompliant);
end;


procedure TRulesEvalTest.DivisionByZeroConstantFloatDivisor;

begin
  // 'f := g / 0.0;' on line 14; the compliant half divides by 2.0.
  CheckEvalRuleSrc(NewDivisionByZeroConstant, NewDivisionByZeroConstant,
    cDivisionByZeroConstantId, 14, ['/'],
    cDivByZeroFloat, cDivByZeroFloatCompliant);
end;


procedure TRulesEvalTest.DivisionByZeroConstantModuloDivisor;

begin
  // 'y := x mod 0;' on line 14.
  CheckEvalRuleSrc(NewDivisionByZeroConstant, NewDivisionByZeroConstant,
    cDivisionByZeroConstantId, 14, ['mod'],
    cDivByZeroModulo, cDivByZeroCompliant);
end;


procedure TRulesEvalTest.DivisionByZeroConstantDegradesWithoutResolver;

begin
  // The positive control first: the same source fires once with resolution
  // intact.
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewDivisionByZeroConstant, False, cDivByZeroNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewDivisionByZeroConstant, True, cDivByZeroNoncompliant));
end;


procedure TRulesEvalTest.DivisionByZeroConstantSilentOnUnresolvedOperand;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('operand.pas', cDivByZeroUnresolvedOperand);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDivisionByZeroConstant, lPath, False, lc);
      AssertEquals('an unfoldable divisor is silent', 0,
        CountById(lc, cDivisionByZeroConstantId));
    finally
      lc.Free;
    end;
    // The resolver was live on that very file: the sibling rule fires on it.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConstantConditionAlwaysTrueOrFalse, lPath, False, lc);
      AssertEquals('the resolver was live', 1,
        CountById(lc, cConstantConditionAlwaysTrueOrFalseId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesEvalTest.ConstantConditionAlwaysTrueOrFalsePositions;

begin
  // Noncompliant: 'if cDebug then' with cDebug = False, on line 16.
  // Compliant: 'if i > 0 then' over a variable => silent.
  CheckEvalRuleSrc(NewConstantConditionAlwaysTrueOrFalse,
    NewConstantConditionAlwaysTrueOrFalse,
    cConstantConditionAlwaysTrueOrFalseId, 16, ['if', 'False'],
    cConstantConditionNoncompliant, cConstantConditionCompliant);
end;


procedure TRulesEvalTest.ConstantConditionAlwaysTrueOrFalseReportsLoopKeywords;

begin
  // 'while cNever do' on line 17, cNever = False.
  CheckEvalRuleSrc(NewConstantConditionAlwaysTrueOrFalse,
    NewConstantConditionAlwaysTrueOrFalse,
    cConstantConditionAlwaysTrueOrFalseId, 17, ['while', 'False'],
    cConstantConditionFalseWhile, cConstantConditionCompliant);
  // 'until cAlways;' on line 19, cAlways = True.
  CheckEvalRuleSrc(NewConstantConditionAlwaysTrueOrFalse,
    NewConstantConditionAlwaysTrueOrFalse,
    cConstantConditionAlwaysTrueOrFalseId, 19, ['repeat', 'True'],
    cConstantConditionTrueRepeat, cConstantConditionCompliant);
end;


procedure TRulesEvalTest.ConstantConditionAlwaysTrueOrFalseInfiniteLoopsAreSilent;

begin
  AssertEquals('while True and repeat .. until False are deliberate', 0,
    RuleCount(NewConstantConditionAlwaysTrueOrFalse, False,
      cConstantConditionInfiniteLoops));
end;


procedure TRulesEvalTest.ConstantConditionAlwaysTrueOrFalseDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewConstantConditionAlwaysTrueOrFalse, False,
      cConstantConditionNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewConstantConditionAlwaysTrueOrFalse, True,
      cConstantConditionNoncompliant));
end;


procedure TRulesEvalTest.ConstantConditionAlwaysTrueOrFalseSilentOnUnresolvedOperand;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('operand.pas', cConstantConditionUnresolvedOperand);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConstantConditionAlwaysTrueOrFalse, lPath, False, lc);
      AssertEquals('an unfoldable condition is silent', 0,
        CountById(lc, cConstantConditionAlwaysTrueOrFalseId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDivisionByZeroConstant, lPath, False, lc);
      AssertEquals('the resolver was live', 1,
        CountById(lc, cDivisionByZeroConstantId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesEvalTest.ConstantConditionAlwaysTrueOrFalseMeasuresIntrinsicFold;

begin
  AssertEquals('SizeOf(Pointer) = 8 folds and is reported, see DW-38', 1,
    RuleCount(NewConstantConditionAlwaysTrueOrFalse, False, cIntrinsicFold));
end;


procedure TRulesEvalTest.ComparisonAlwaysTrueForTypePositions;

begin
  // 'if b > 255 then' on line 15; 255 is the top of Byte.
  CheckEvalRuleSrc(NewComparisonAlwaysTrueForType,
    NewComparisonAlwaysTrueForType, cComparisonAlwaysTrueForTypeId, 15,
    ['Byte', '255', 'False'], cCompareNoncompliant, cCompareCompliant);
end;


procedure TRulesEvalTest.ComparisonAlwaysTrueForTypeUnsignedLowerBound;

begin
  // 'if b < 0 then' on line 15: the constant is in range, the verdict is not.
  CheckEvalRuleSrc(NewComparisonAlwaysTrueForType,
    NewComparisonAlwaysTrueForType, cComparisonAlwaysTrueForTypeId, 15,
    ['Byte', '0', 'False'], cCompareLowerBound, cCompareCompliant);
end;


procedure TRulesEvalTest.ComparisonAlwaysTrueForTypeDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewComparisonAlwaysTrueForType, False, cCompareNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewComparisonAlwaysTrueForType, True, cCompareNoncompliant));
end;


procedure TRulesEvalTest.ComparisonAlwaysTrueForTypeSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewComparisonAlwaysTrueForType,
    cComparisonAlwaysTrueForTypeId, cCompareUnresolvedOperand);
end;


procedure TRulesEvalTest.ConstantOutOfRangeForTargetPositions;

begin
  // 'b := 300;' on line 13; the compliant half assigns 200.
  CheckEvalRuleSrc(NewConstantOutOfRangeForTarget,
    NewConstantOutOfRangeForTarget, cConstantOutOfRangeForTargetId, 13,
    ['300', 'Byte'], cOutOfRangeNoncompliant, cOutOfRangeCompliant);
end;


procedure TRulesEvalTest.ConstantOutOfRangeForTargetIsTargetDependent;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('target.pas', cOutOfRangeTargetDependent);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConstantOutOfRangeForTarget, lPath, False, lc, 8);
      AssertEquals('128 fits Byte on a 64-bit target', 0,
        CountById(lc, cConstantOutOfRangeForTargetId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConstantOutOfRangeForTarget, lPath, False, lc, 4);
      AssertEquals('256 does not fit Byte on a 32-bit target', 1,
        CountById(lc, cConstantOutOfRangeForTargetId));
      k := FirstById(lc, cConstantOutOfRangeForTargetId);
      AssertEquals('the assignment row', 16, lc.Issues[k].StartLine);
      AssertEquals('the folded value', '256', lc.Issues[k].MessageArgs[0]);
      AssertEquals('the target type', 'Byte', lc.Issues[k].MessageArgs[1]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesEvalTest.ConstantOutOfRangeForTargetDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewConstantOutOfRangeForTarget, False, cOutOfRangeNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewConstantOutOfRangeForTarget, True, cOutOfRangeNoncompliant));
end;


procedure TRulesEvalTest.ConstantOutOfRangeForTargetSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewConstantOutOfRangeForTarget,
    cConstantOutOfRangeForTargetId, cOutOfRangeUnresolvedOperand);
end;


procedure TRulesEvalTest.ConstantOverflowInExpressionPositions;

begin
  // 'if High(Int64) * High(Int64) > 0 then' on line 14; the compliant half
  // adds 1, which the evaluator widens instead of overflowing.
  CheckEvalRuleSrc(NewConstantOverflowInExpression,
    NewConstantOverflowInExpression, cConstantOverflowInExpressionId, 14,
    ['*'], cOverflowNoncompliant, cOverflowCompliant);
end;


procedure TRulesEvalTest.ConstantOverflowInExpressionDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewConstantOverflowInExpression, False, cOverflowNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewConstantOverflowInExpression, True, cOverflowNoncompliant));
end;


procedure TRulesEvalTest.ConstantOverflowInExpressionSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewConstantOverflowInExpression,
    cConstantOverflowInExpressionId, cOverflowUnresolvedOperand);
end;


procedure TRulesEvalTest.ShiftCountExceedsWidthPositions;

begin
  // 'y := l shl 40;' on line 14 over a 32-bit Longint; the compliant half
  // shifts by 8.
  CheckEvalRuleSrc(NewShiftCountExceedsWidth, NewShiftCountExceedsWidth,
    cShiftCountExceedsWidthId, 14, ['40', 'Longint'],
    cShiftNoncompliant, cShiftCompliant);
end;


procedure TRulesEvalTest.ShiftCountExceedsWidthDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewShiftCountExceedsWidth, False, cShiftNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewShiftCountExceedsWidth, True, cShiftNoncompliant));
end;


procedure TRulesEvalTest.ShiftCountExceedsWidthSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewShiftCountExceedsWidth,
    cShiftCountExceedsWidthId, cShiftUnresolvedOperand);
end;


procedure TRulesEvalTest.SetElementOutOfRangePositions;

begin
  // 'if b in [1, 300] then' on line 15; the compliant half tests [1, 200].
  CheckEvalRuleSrc(NewSetElementOutOfRange, NewSetElementOutOfRange,
    cSetElementOutOfRangeId, 15, ['Byte'], cSetNoncompliant, cSetCompliant);
end;


procedure TRulesEvalTest.SetElementOutOfRangeDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewSetElementOutOfRange, False, cSetNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewSetElementOutOfRange, True, cSetNoncompliant));
end;


procedure TRulesEvalTest.SetElementOutOfRangeSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewSetElementOutOfRange,
    cSetElementOutOfRangeId, cSetUnresolvedOperand);
end;


procedure TRulesEvalTest.EnumOrdinalOutOfRangePositions;

begin
  // 'if TThree(7) = tA then' on line 17; the compliant half casts 2.
  CheckEvalRuleSrc(NewEnumOrdinalOutOfRange, NewEnumOrdinalOutOfRange,
    cEnumOrdinalOutOfRangeId, 17, ['7', 'TThree'],
    cEnumNoncompliant, cEnumCompliant);
end;


procedure TRulesEvalTest.EnumOrdinalOutOfRangeDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewEnumOrdinalOutOfRange, False, cEnumNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewEnumOrdinalOutOfRange, True, cEnumNoncompliant));
end;


procedure TRulesEvalTest.EnumOrdinalOutOfRangeSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewEnumOrdinalOutOfRange,
    cEnumOrdinalOutOfRangeId, cEnumUnresolvedOperand);
end;


procedure TRulesEvalTest.ArrayIndexConstantOutOfBoundsPositions;

begin
  // 'a[10] := 1;' on line 13 over array[0..3]; the compliant half indexes 2
  // and adds a dynamic array, which carries no static range.
  CheckEvalRuleSrc(NewArrayIndexConstantOutOfBounds,
    NewArrayIndexConstantOutOfBounds, cArrayIndexConstantOutOfBoundsId, 13,
    ['10', 'a'], cArrayNoncompliant, cArrayCompliant);
end;


procedure TRulesEvalTest.ArrayIndexConstantOutOfBoundsDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewArrayIndexConstantOutOfBounds, False, cArrayNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewArrayIndexConstantOutOfBounds, True, cArrayNoncompliant));
end;


procedure TRulesEvalTest.ArrayIndexConstantOutOfBoundsSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewArrayIndexConstantOutOfBounds,
    cArrayIndexConstantOutOfBoundsId, cArrayUnresolvedOperand);
end;


procedure TRulesEvalTest.SizeOfOnReferenceTypePositions;

begin
  // 'n := SizeOf(o);' on line 14 over a TObject; the compliant half sizes a
  // ShortString, a record and an ordinal type.
  CheckEvalRuleSrc(NewSizeOfOnReferenceType, NewSizeOfOnReferenceType,
    cSizeOfOnReferenceTypeId, 14, ['TObject'],
    cSizeOfNoncompliant, cSizeOfCompliant);
end;


procedure TRulesEvalTest.SizeOfOnReferenceTypeReportsStringAndDynamicArray;

begin
  AssertEquals('the dynamic array, the AnsiString and the interface report', 3,
    RuleCount(NewSizeOfOnReferenceType, False, cSizeOfStringAndDynArray));
end;


procedure TRulesEvalTest.SizeOfOnReferenceTypeIgnoresUserRoutine;

begin
  AssertEquals('the built-in SizeOf reports', 1,
    RuleCount(NewSizeOfOnReferenceType, False, cSizeOfNoncompliant));
  CheckSilentWithLiveSibling(NewSizeOfOnReferenceType,
    cSizeOfOnReferenceTypeId, cSizeOfUserRoutine);
end;


procedure TRulesEvalTest.SizeOfOnReferenceTypeDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewSizeOfOnReferenceType, False, cSizeOfNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewSizeOfOnReferenceType, True, cSizeOfNoncompliant));
end;


procedure TRulesEvalTest.SizeOfOnReferenceTypeSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewSizeOfOnReferenceType,
    cSizeOfOnReferenceTypeId, cSizeOfUnresolvedOperand);
end;


procedure TRulesEvalTest.MoveFillCharSizeMismatchPositions;

begin
  // 'Move(a[0], b[0], Length(a));' on line 21 over an array of Integer; the
  // compliant half multiplies by the element size and moves a byte array.
  CheckEvalRuleSrc(NewMoveFillCharSizeMismatch, NewMoveFillCharSizeMismatch,
    cMoveFillCharSizeMismatchId, 21, ['Move', 'Length'],
    cMoveNoncompliant, cMoveCompliant);
end;


procedure TRulesEvalTest.MoveFillCharSizeMismatchReportsPointerCount;

begin
  // 'Move(p^, q^, SizeOf(p));' on line 24 sizes the pointer, not the record.
  CheckEvalRuleSrc(NewMoveFillCharSizeMismatch, NewMoveFillCharSizeMismatch,
    cMoveFillCharSizeMismatchId, 24, ['Move', 'SizeOf'],
    cMovePointerCount, cMoveCompliant);
end;


procedure TRulesEvalTest.MoveFillCharSizeMismatchReadsFillCharSecondArgument;

begin
  AssertEquals('FillChar carries its byte count as the second argument', 1,
    RuleCount(NewMoveFillCharSizeMismatch, False, cFillCharCount));
end;


procedure TRulesEvalTest.MoveFillCharSizeMismatchIgnoresTypedRoutine;

begin
  CheckSilentWithLiveSibling(NewMoveFillCharSizeMismatch,
    cMoveFillCharSizeMismatchId, cMoveTypedRoutine);
end;


procedure TRulesEvalTest.MoveFillCharSizeMismatchDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewMoveFillCharSizeMismatch, False, cMoveNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewMoveFillCharSizeMismatch, True, cMoveNoncompliant));
end;


procedure TRulesEvalTest.MoveFillCharSizeMismatchSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewMoveFillCharSizeMismatch,
    cMoveFillCharSizeMismatchId, cMoveUnresolvedOperand);
end;


procedure TRulesEvalTest.FloatEqualityComparisonPositions;

begin
  // 'if f = 0.0 then' on line 14; the compliant half orders the float and
  // compares an integer for equality.
  CheckEvalRuleSrc(NewFloatEqualityComparison, NewFloatEqualityComparison,
    cFloatEqualityComparisonId, 14, ['Double'],
    cFloatEqualNoncompliant, cFloatEqualCompliant);
end;


procedure TRulesEvalTest.FloatEqualityComparisonReportsInequality;

begin
  AssertEquals('<> is as exact as =', 1,
    RuleCount(NewFloatEqualityComparison, False, cFloatNotEqual));
end;


procedure TRulesEvalTest.FloatEqualityComparisonDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewFloatEqualityComparison, False, cFloatEqualNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewFloatEqualityComparison, True, cFloatEqualNoncompliant));
end;


procedure TRulesEvalTest.FloatEqualityComparisonSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewFloatEqualityComparison,
    cFloatEqualityComparisonId, cFloatEqualUnresolvedOperand);
end;


procedure TRulesEvalTest.IntegerDivisionAssignedToFloatPositions;

begin
  // 'f := i div j;' on line 14; the compliant half divides with / and assigns
  // the div to an integer.
  CheckEvalRuleSrc(NewIntegerDivisionAssignedToFloat,
    NewIntegerDivisionAssignedToFloat, cIntegerDivisionAssignedToFloatId, 14,
    ['Double'], cIntDivNoncompliant, cIntDivCompliant);
end;


procedure TRulesEvalTest.IntegerDivisionAssignedToFloatDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewIntegerDivisionAssignedToFloat, False, cIntDivNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewIntegerDivisionAssignedToFloat, True, cIntDivNoncompliant));
end;


procedure TRulesEvalTest.IntegerDivisionAssignedToFloatSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewIntegerDivisionAssignedToFloat,
    cIntegerDivisionAssignedToFloatId, cIntDivUnresolvedOperand);
end;


procedure TRulesEvalTest.MixedSignedUnsignedComparisonPositions;

begin
  // 'if l < c then' on line 15 over a Longint and a LongWord; the compliant
  // half carries the four near-miss shapes.
  CheckEvalRuleSrc(NewMixedSignedUnsignedComparison,
    NewMixedSignedUnsignedComparison, cMixedSignedUnsignedComparisonId, 15,
    ['Longint', 'LongWord'], cMixedSignNoncompliant, cMixedSignCompliant);
end;


procedure TRulesEvalTest.MixedSignedUnsignedComparisonNamesSignedOperandFirst;

begin
  // 'if c > l then' on line 15 reports the same pair in the message's order.
  CheckEvalRuleSrc(NewMixedSignedUnsignedComparison,
    NewMixedSignedUnsignedComparison, cMixedSignedUnsignedComparisonId, 15,
    ['Longint', 'LongWord'], cMixedSignReversed, cMixedSignCompliant);
end;


procedure TRulesEvalTest.MixedSignedUnsignedComparisonDegradesWithoutResolver;

begin
  AssertEquals('resolved => one issue', 1,
    RuleCount(NewMixedSignedUnsignedComparison, False,
      cMixedSignNoncompliant));
  AssertEquals('resolution withheld => silent', 0,
    RuleCount(NewMixedSignedUnsignedComparison, True, cMixedSignNoncompliant));
end;


procedure TRulesEvalTest.MixedSignedUnsignedComparisonSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewMixedSignedUnsignedComparison,
    cMixedSignedUnsignedComparisonId, cMixedSignUnresolvedOperand);
end;


procedure TRulesEvalTest.EvalHarnessSurvivesRangeErrors;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('rangeerrors.pas', cRangeErrorSurvival);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDivisionByZeroConstant, lPath, False, lc);
      AssertEquals('the range errors do not abort the analysis', 1,
        CountById(lc, cDivisionByZeroConstantId));
      k := FirstById(lc, cDivisionByZeroConstantId);
      AssertEquals('the issue is on the div row', 31, lc.Issues[k].StartLine);
    finally
      lc.Free;
    end;
    { The excluded branch holds `if cAlwaysTrue then` and the compiled body
      holds `if cCompiled then`; only the compiled one may be reported. }
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConstantConditionAlwaysTrueOrFalse, lPath, False, lc);
      AssertEquals('only the compiled condition is reported', 1,
        CountById(lc, cConstantConditionAlwaysTrueOrFalseId));
      k := FirstById(lc, cConstantConditionAlwaysTrueOrFalseId);
      AssertEquals('the issue is on the compiled if row', 32,
        lc.Issues[k].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesEvalTest.EvalRulesSelfRegisterGlobally;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cDivisionByZeroConstantId);
  AssertNotNull('DivisionByZeroConstant registered', lRule);
  AssertFalse('DivisionByZeroConstant ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cConstantConditionAlwaysTrueOrFalseId);
  AssertNotNull('ConstantConditionAlwaysTrueOrFalse registered', lRule);
  AssertFalse('ConstantConditionAlwaysTrueOrFalse ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cComparisonAlwaysTrueForTypeId);
  AssertNotNull('ComparisonAlwaysTrueForType registered', lRule);
  AssertFalse('ComparisonAlwaysTrueForType ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cConstantOutOfRangeForTargetId);
  AssertNotNull('ConstantOutOfRangeForTarget registered', lRule);
  AssertFalse('ConstantOutOfRangeForTarget ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cConstantOverflowInExpressionId);
  AssertNotNull('ConstantOverflowInExpression registered', lRule);
  AssertFalse('ConstantOverflowInExpression ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cShiftCountExceedsWidthId);
  AssertNotNull('ShiftCountExceedsWidth registered', lRule);
  AssertFalse('ShiftCountExceedsWidth ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cSetElementOutOfRangeId);
  AssertNotNull('SetElementOutOfRange registered', lRule);
  AssertFalse('SetElementOutOfRange ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cEnumOrdinalOutOfRangeId);
  AssertNotNull('EnumOrdinalOutOfRange registered', lRule);
  AssertFalse('EnumOrdinalOutOfRange ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cArrayIndexConstantOutOfBoundsId);
  AssertNotNull('ArrayIndexConstantOutOfBounds registered', lRule);
  AssertFalse('ArrayIndexConstantOutOfBounds ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cSizeOfOnReferenceTypeId);
  AssertNotNull('SizeOfOnReferenceType registered', lRule);
  AssertFalse('SizeOfOnReferenceType ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cMoveFillCharSizeMismatchId);
  AssertNotNull('MoveFillCharSizeMismatch registered', lRule);
  AssertFalse('MoveFillCharSizeMismatch ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cFloatEqualityComparisonId);
  AssertNotNull('FloatEqualityComparison registered', lRule);
  AssertFalse('FloatEqualityComparison ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cIntegerDivisionAssignedToFloatId);
  AssertNotNull('IntegerDivisionAssignedToFloat registered', lRule);
  AssertFalse('IntegerDivisionAssignedToFloat ships disabled',
    lRule.Metadata.DefaultEnabled);
  lRule := RuleRegistry.FindById(cMixedSignedUnsignedComparisonId);
  AssertNotNull('MixedSignedUnsignedComparison registered', lRule);
  AssertFalse('MixedSignedUnsignedComparison ships disabled',
    lRule.Metadata.DefaultEnabled);
end;


initialization
  RegisterTest(TRulesEvalTest);

end.
