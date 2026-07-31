{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the string, character and encoding (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesStrings;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Strings, FpSonar.Rules.Casts, FpSonar.Rules.Eval,
  FpSonar.Rules.Calls,
  UtstFixtures;

type
  { SEM-tier string-rule position, per-mode, degradation and registration tests. }
  TRulesStringsTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector. aWithhold
    // analyzes with resolution withheld.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      aWithhold: boolean; const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // How many issues aRule reports over aSource, materialised to a temp dir.
    function RuleCount(aRule: TRuleBase; aWithhold: boolean;
      const aSource: array of string): Integer;
    // Asserts aRule fires once at aLine, column 1, with key rule.<aId>.message
    // and message args = aArgs.
    procedure CheckIssueAt(aRule: TRuleBase; const aId: string; aLine: Integer;
      const aArgs: array of string; const aSource: array of string);
    // Asserts aRule is silent on aSource while aSibling fires once on the same
    // file, which is what proves the resolver was live.
    procedure CheckSilentWithLiveSibling(aRule, aSibling: TRuleBase;
      const aId, aSiblingId: string; const aSource: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewPCharOfTemporaryString: TRuleBase;
    function NewImplicitStringConversionWithDataLoss: TRuleBase;
    function NewLengthUsedAsByteCount: TRuleBase;
    function NewUnicodeToAnsiCast: TRuleBase;
    function NewCharToCharPointerCast: TRuleBase;
    function NewMoveFillCharSizeMismatch: TRuleBase;
    function NewCopyWithZeroIndex: TRuleBase;
    function NewPosResultComparedToZeroBased: TRuleBase;
    function NewShortStringTruncation: TRuleBase;
    function NewCharComparedToString: TRuleBase;
    function NewStringFirstCharByIndex: TRuleBase;
    function NewComparisonAlwaysTrueForType: TRuleBase;
    function NewRawByteStringCodePageMix: TRuleBase;
    function NewStringConcatInLoop: TRuleBase;
    function NewStrToIntWithoutGuard: TRuleBase;
    function NewWideStringOnNonWindows: TRuleBase;
    function NewSetLengthWithoutFill: TRuleBase;
  published
    procedure PCharOfTemporaryStringPositions;
    procedure PCharOfTemporaryStringSeesEveryCallShape;
    procedure PCharOfTemporaryStringSeesBuiltInStringCall;
    procedure PCharOfTemporaryStringPositionsPerMode;
    procedure PCharOfTemporaryStringDegradesWithoutResolver;
    procedure PCharOfTemporaryStringSilentOnUnresolvedOperand;
    procedure ImplicitStringConversionWithDataLossPositions;
    procedure ImplicitStringConversionWithDataLossPositionsPerMode;
    procedure ImplicitStringConversionWithDataLossDegradesWithoutResolver;
    procedure ImplicitStringConversionWithDataLossSilentOnNonStringSource;
    procedure ImplicitStringConversionDisjointFromUnicodeToAnsiCast;
    procedure PCharOfTemporaryStringDisjointFromCharToCharPointerCast;
    procedure LengthUsedAsByteCountPositions;
    procedure LengthUsedAsByteCountFillCharPosition;
    procedure LengthUsedAsByteCountPositionsPerMode;
    procedure LengthUsedAsByteCountDegradesWithoutResolver;
    procedure LengthUsedAsByteCountSilentOnShadowedLength;
    procedure LengthUsedAsByteCountDisjointFromMoveFillCharSizeMismatch;
    procedure BareStringClassifiesTheSameInEveryMode;
    procedure CopyWithZeroIndexPositions;
    procedure CopyWithZeroIndexSeesBothArities;
    procedure CopyWithZeroIndexPositionsPerMode;
    procedure CopyWithZeroIndexDegradesWithoutResolver;
    procedure CopyWithZeroIndexSilentOnNonFoldingIndex;
    procedure CopyWithZeroIndexDisjointFromStringFirstCharByIndex;
    procedure PosResultComparedToZeroBasedPositions;
    procedure PosResultComparedToZeroBasedSeesEveryComparedShape;
    procedure PosResultComparedToZeroBasedPositionsPerMode;
    procedure PosResultComparedToZeroBasedDegradesWithoutResolver;
    procedure PosResultComparedToZeroBasedSilentOnMethodNamedPos;
    procedure ShortStringTruncationPositions;
    procedure ShortStringTruncationPositionsPerMode;
    procedure ShortStringTruncationDegradesWithoutResolver;
    procedure ShortStringTruncationSilentOnNonConstantSource;
    procedure ShortStringTruncationDisjointFromImplicitStringConversion;
    procedure CharComparedToStringPositions;
    procedure CharComparedToStringSeesEveryComparedShape;
    procedure CharComparedToStringPositionsPerMode;
    procedure CharComparedToStringDegradesWithoutResolver;
    procedure CharComparedToStringSilentOnStringVariable;
    procedure CharComparedToStringSeesNonAsciiLiteralAsTwoChars;
    procedure ZeroBasedComparisonsDisjointFromComparisonAlwaysTrueForType;
    procedure RawByteStringCodePageMixPositions;
    procedure RawByteStringCodePageMixSeesBothDirections;
    procedure RawByteStringCodePageMixSeesCodePagedArgument;
    procedure RawByteStringCodePageMixPositionsPerMode;
    procedure RawByteStringCodePageMixDegradesWithoutResolver;
    procedure RawByteStringCodePageMixSilentOnPlainAnsiString;
    procedure RawByteStringCodePageMixDisjointFromImplicitStringConversion;
    procedure RawByteStringCodePageMixSilentOnWrittenCast;
    procedure StringConcatInLoopPositions;
    procedure StringConcatInLoopSeesEveryLoopShape;
    procedure StringConcatInLoopPositionsPerMode;
    procedure StringConcatInLoopDegradesWithoutResolver;
    procedure StringConcatInLoopSilentOnDifferentTarget;
    procedure StrToIntWithoutGuardPositions;
    procedure StrToIntWithoutGuardSeesUnprotectedTryShapes;
    procedure StrToIntWithoutGuardPositionsPerMode;
    procedure StrToIntWithoutGuardDegradesWithoutResolver;
    procedure StrToIntWithoutGuardSilentOnLiteralArgument;
    procedure StrToIntWithoutGuardSilentOnMethodNamedStrToInt;
    procedure WideStringOnNonWindowsPositions;
    procedure WideStringOnNonWindowsSeesEveryDeclarationKind;
    procedure WideStringOnNonWindowsPositionsPerMode;
    procedure WideStringOnNonWindowsDegradesWithoutResolver;
    procedure WideStringOnNonWindowsSilentOnUnicodeString;
    procedure WideStringOnNonWindowsSilentOnWindowsOnlyDeclaration;
    procedure SetLengthWithoutFillPositions;
    procedure SetLengthWithoutFillPositionsPerMode;
    procedure SetLengthWithoutFillCountsReadAndWriteInOneStatement;
    procedure SetLengthWithoutFillSilentOnFilledStorage;
    procedure SetLengthWithoutFillSilentOnGrownStorage;
    procedure SetLengthWithoutFillSilentOnInitialisedStorage;
    procedure SetLengthWithoutFillSilentOnCallHandover;
    procedure SetLengthWithoutFillSilentOnTwoResizeSites;
    procedure SetLengthWithoutFillSilentOnAddressEscape;
    procedure SetLengthWithoutFillSilentOnAbsoluteAlias;
    procedure SetLengthWithoutFillSilentOnNestedRoutineMention;
    procedure SetLengthWithoutFillDegradesWithoutResolver;
    procedure SetLengthWithoutFillSilentOnInlineAssembler;
    procedure StringsRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cPCharOfTemporaryStringId = 'PCharOfTemporaryString';
  cImplicitStringConversionWithDataLossId =
    'ImplicitStringConversionWithDataLoss';
  cLengthUsedAsByteCountId = 'LengthUsedAsByteCount';
  cUnicodeToAnsiCastId = 'UnicodeToAnsiCast';
  cCharToCharPointerCastId = 'CharToCharPointerCast';
  cMoveFillCharSizeMismatchId = 'MoveFillCharSizeMismatch';
  cCopyWithZeroIndexId = 'CopyWithZeroIndex';
  cPosResultComparedToZeroBasedId = 'PosResultComparedToZeroBased';
  cShortStringTruncationId = 'ShortStringTruncation';
  cCharComparedToStringId = 'CharComparedToString';
  cStringFirstCharByIndexId = 'StringFirstCharByIndex';
  cComparisonAlwaysTrueForTypeId = 'ComparisonAlwaysTrueForType';
  cRawByteStringCodePageMixId = 'RawByteStringCodePageMix';
  cStringConcatInLoopId = 'StringConcatInLoop';
  cStrToIntWithoutGuardId = 'StrToIntWithoutGuard';
  cWideStringOnNonWindowsId = 'WideStringOnNonWindows';
  cSetLengthWithoutFillId = 'SetLengthWithoutFill';

  // Line 2 of every fixture below; InMode swaps it for one of the other two.
  cModeObjfpc = '{$mode objfpc}{$H+}';
  cModeDelphi = '{$mode delphi}';
  cModeDelphiUnicode = '{$mode delphiunicode}';

  // Embedded string-rule fixtures: line i+1 == [i]. The positive cases name
  // AnsiString/UnicodeString/PAnsiChar explicitly, whose classification does
  // not depend on the analysed unit's mode.

  cPCharNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function MakeName: AnsiString;',
    'procedure Run;',
    'implementation',
    'function MakeName: AnsiString;',
    'begin',
    '  Result := '''';',
    'end;',
    'procedure Run;',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(MakeName);',
    'end;',
    'end.');

  cPCharConcatNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  x, y: AnsiString;',
    '  p: PAnsiChar;',
    'begin',
    '  x := '''';',
    '  p := PAnsiChar(x + y);',
    'end;',
    'end.');

  { A written argument list, then a dotted callee: the other two ways a call
    result reaches a cast operand. }
  cPCharCallArgsNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function MakeName(const aSeed: AnsiString): AnsiString;',
    'procedure Run;',
    'implementation',
    'function MakeName(const aSeed: AnsiString): AnsiString;',
    'begin',
    '  Result := aSeed;',
    'end;',
    'procedure Run;',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(MakeName(''x''));',
    'end;',
    'end.');

  cPCharQualifiedNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBox = class',
    '    function MakeName: AnsiString;',
    '  end;',
    'procedure Run(aBox: TBox);',
    'implementation',
    'function TBox.MakeName: AnsiString;',
    'begin',
    '  Result := '''';',
    'end;',
    'procedure Run(aBox: TBox);',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(aBox.MakeName);',
    'end;',
    'end.');

  { A built-in string function, in both its spellings: the result is a
    temporary exactly as a user function's result is. }
  cPCharCopyNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  x: AnsiString;',
    '  p: PAnsiChar;',
    'begin',
    '  x := '''';',
    '  p := PAnsiChar(Copy(x, 1, 2));',
    'end;',
    'end.');

  cPCharConcatCallNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  x, y: AnsiString;',
    '  p: PAnsiChar;',
    'begin',
    '  x := '''';',
    '  p := PAnsiChar(Concat(x, y));',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: a variable, a const argument, a
    literal, a field and an indexed element. None of them is a temporary. Live
    carries a wide-to-ANSI assignment. }
  cPCharCompliant: array[0..54] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TNames = array of AnsiString;',
    '  TBox = class',
    '    Name: AnsiString;',
    '  end;',
    'procedure OnVariable;',
    'procedure OnConstArg(const aName: AnsiString);',
    'procedure OnLiteral;',
    'procedure OnField(aBox: TBox);',
    'procedure OnElement(const aNames: TNames);',
    'implementation',
    'procedure OnVariable;',
    'var',
    '  s: AnsiString;',
    '  p: PAnsiChar;',
    'begin',
    '  s := '''';',
    '  p := PAnsiChar(s);',
    'end;',
    'procedure OnConstArg(const aName: AnsiString);',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(aName);',
    'end;',
    'procedure OnLiteral;',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(''abc'');',
    'end;',
    'procedure OnField(aBox: TBox);',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(aBox.Name);',
    'end;',
    'procedure OnElement(const aNames: TNames);',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(aNames[0]);',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The cast operand is an untyped const argument, so its type does not
    resolve; the wide-to-ANSI assignment is the live sibling. }
  cPCharUnresolved: array[0..15] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run(const aValue);',
    'implementation',
    'procedure Run(const aValue);',
    'var',
    '  p: PAnsiChar;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  p := PAnsiChar(aValue);',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cImplicitNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: a written cast, a widening
    assignment and a same-encoding assignment. }
  cImplicitCompliant: array[0..40] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Widening;',
    'procedure CastWritten;',
    'procedure SameEncoding;',
    'implementation',
    'procedure Widening;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  a := '''';',
    '  u := a;',
    'end;',
    'procedure CastWritten;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := AnsiString(u);',
    'end;',
    'procedure SameEncoding;',
    'var',
    '  a, b: AnsiString;',
    'begin',
    '  b := '''';',
    '  a := b;',
    'end;',
    'function MakeName: AnsiString;',
    'begin',
    '  Result := '''';',
    'end;',
    'procedure Live;',
    'var',
    '  p: PAnsiChar;',
    'begin',
    '  p := PAnsiChar(MakeName);',
    'end;',
    'end.');

  cCastWritten: array[0..13] of string = (
    'unit disjoint;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := AnsiString(u);',
    'end;',
    'end.');

  { A Variant source resolves to no string kind, so the rule has no encoding
    pair to compare; the temporary cast is the live sibling. }
  cImplicitNonString: array[0..19] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function MakeName: AnsiString;',
    'procedure Run;',
    'implementation',
    'function MakeName: AnsiString;',
    'begin',
    '  Result := '''';',
    'end;',
    'procedure Run;',
    'var',
    '  a: AnsiString;',
    '  v: Variant;',
    '  p: PAnsiChar;',
    'begin',
    '  a := v;',
    '  p := PAnsiChar(MakeName);',
    'end;',
    'end.');

  // The fixture declares Move itself, at unit level.
  cLengthNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'procedure Run;',
    'implementation',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  u, d: UnicodeString;',
    'begin',
    '  Move(u[1], d[1], Length(u));',
    'end;',
    'end.');

  cLengthFillCharNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
    'procedure Run;',
    'implementation',
    'procedure FillChar(var aDest; aCount: SizeInt; aValue: Byte);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  u: UnicodeString;',
    'begin',
    '  FillChar(u[1], Length(u), 0);',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: an ANSI string count, a count
    already scaled by the character size, an array count and a method named
    Move. }
  cLengthCompliant: array[0..52] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntArray = array of Integer;',
    '  TGrid = class',
    '    procedure Move(aFrom, aTo, aCount: SizeInt);',
    '  end;',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'procedure OnAnsi;',
    'procedure OnScaledCount;',
    'procedure OnArray;',
    'procedure OnMethod(aGrid: TGrid);',
    'implementation',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    'procedure TGrid.Move(aFrom, aTo, aCount: SizeInt);',
    'begin',
    'end;',
    'procedure OnAnsi;',
    'var',
    '  a, b: AnsiString;',
    'begin',
    '  Move(a[1], b[1], Length(a));',
    'end;',
    'procedure OnScaledCount;',
    'var',
    '  u, d: UnicodeString;',
    'begin',
    '  Move(u[1], d[1], Length(u) * SizeOf(WideChar));',
    'end;',
    'procedure OnArray;',
    'var',
    '  x, y: TIntArray;',
    'begin',
    '  Move(x[0], y[0], Length(x));',
    'end;',
    'procedure OnMethod(aGrid: TGrid);',
    'var',
    '  u: UnicodeString;',
    'begin',
    '  aGrid.Move(1, 2, Length(u));',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cLengthArrayCount: array[0..17] of string = (
    'unit disjoint;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntArray = array of Integer;',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'procedure Run;',
    'implementation',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  x, y: TIntArray;',
    'begin',
    '  Move(x[0], y[0], Length(x));',
    'end;',
    'end.');

  { A user-declared Length shadows the built-in, so the count is not a
    resolved character count; the wide-to-ANSI assignment is the live sibling. }
  cLengthShadowed: array[0..22] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'function Length(const aValue: UnicodeString): SizeInt;',
    'procedure Run;',
    'implementation',
    'procedure Move(const aSource; var aDest; aCount: SizeInt);',
    'begin',
    'end;',
    'function Length(const aValue: UnicodeString): SizeInt;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  u, d: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  Move(u[1], d[1], Length(u));',
    '  a := u;',
    'end;',
    'end.');

  { The operands are declared bare `string`, whose classification the analysed
    unit's mode does not decide. }
  cBareString: array[0..13] of string = (
    'unit bare;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  a: string;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cCopyNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, d: AnsiString;',
    'begin',
    '  s := '''';',
    '  d := Copy(s, 0, 2);',
    'end;',
    'end.');

  // The start-index-only arity of the string Copy.
  cCopyTwoArgNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, d: AnsiString;',
    'begin',
    '  s := '''';',
    '  d := Copy(s, 0);',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: the correct start index, an index
    that does not fold and the array Copy, which is 0-based-correct. Live
    carries a wide-to-ANSI assignment. }
  cCopyCompliant: array[0..39] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TIntArray = array of Integer;',
    'procedure OnCorrectIndex;',
    'procedure OnVariableIndex;',
    'procedure OnArrayCopy;',
    'implementation',
    'procedure OnCorrectIndex;',
    'var',
    '  s, d: AnsiString;',
    'begin',
    '  s := '''';',
    '  d := Copy(s, 1, 2);',
    'end;',
    'procedure OnVariableIndex;',
    'var',
    '  s, d: AnsiString;',
    '  i: Integer;',
    'begin',
    '  s := '''';',
    '  i := 0;',
    '  d := Copy(s, i, 2);',
    'end;',
    'procedure OnArrayCopy;',
    'var',
    '  x, y: TIntArray;',
    'begin',
    '  y := Copy(x, 0, 2);',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The start index is a variable, so it does not const-fold; the wide-to-ANSI
    assignment is the live sibling. }
  cCopyNonFoldingIndex: array[0..17] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, d: AnsiString;',
    '  u: UnicodeString;',
    '  i: Integer;',
    'begin',
    '  s := '''';',
    '  i := 0;',
    '  d := Copy(s, i, 2);',
    '  u := '''';',
    '  s := u;',
    'end;',
    'end.');

  cCopyStringIndex: array[0..13] of string = (
    'unit disjoint;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: AnsiString;',
    '  c: AnsiChar;',
    'begin',
    '  s := '''';',
    '  c := s[1];',
    'end;',
    'end.');

  cPosNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  x := '''';',
    '  s := '''';',
    '  if Pos(x, s) >= 0 then',
    '    s := '''';',
    'end;',
    'end.');

  cPosMirroredNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  x := '''';',
    '  s := '''';',
    '  if 0 <= Pos(x, s) then',
    '    s := '''';',
    'end;',
    'end.');

  cPosMinusOneNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  x := '''';',
    '  s := '''';',
    '  if Pos(x, s) = -1 then',
    '    s := '''';',
    'end;',
    'end.');

  // One routine per entry of the fixed comparison table.
  cPosEveryShape: array[0..52] of string = (
    'unit shapes;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure AtLeastZero;',
    'procedure BelowZero;',
    'procedure AboveMinusOne;',
    'procedure AtMostMinusOne;',
    'procedure EqualMinusOne;',
    'procedure NotEqualMinusOne;',
    'implementation',
    'procedure AtLeastZero;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) >= 0 then',
    '    s := '''';',
    'end;',
    'procedure BelowZero;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) < 0 then',
    '    s := '''';',
    'end;',
    'procedure AboveMinusOne;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) > -1 then',
    '    s := '''';',
    'end;',
    'procedure AtMostMinusOne;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) <= -1 then',
    '    s := '''';',
    'end;',
    'procedure EqualMinusOne;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) = -1 then',
    '    s := '''';',
    'end;',
    'procedure NotEqualMinusOne;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) <> -1 then',
    '    s := '''';',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: the two correct idioms and the
    1-based lower bound. Live carries a wide-to-ANSI assignment. }
  cPosCompliant: array[0..36] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure OnAboveZero;',
    'procedure OnEqualZero;',
    'procedure OnAtLeastOne;',
    'implementation',
    'procedure OnAboveZero;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) > 0 then',
    '    s := '''';',
    'end;',
    'procedure OnEqualZero;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) = 0 then',
    '    s := '''';',
    'end;',
    'procedure OnAtLeastOne;',
    'var',
    '  x, s: AnsiString;',
    'begin',
    '  if Pos(x, s) >= 1 then',
    '    s := '''';',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { A method carrying the RTL Pos signature is not the unit-level routine, so
    only its owner separates it; the wide-to-ANSI assignment is the live
    sibling. }
  cPosMethodNamed: array[0..23] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TGrid = class',
    '    function Pos(const aSub, aStr: AnsiString): Integer;',
    '  end;',
    'procedure Run(aGrid: TGrid);',
    'implementation',
    'function TGrid.Pos(const aSub, aStr: AnsiString): Integer;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run(aGrid: TGrid);',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  if aGrid.Pos(a, a) >= 0 then',
    '    a := '''';',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cShortStringNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: string[4];',
    'begin',
    '  s := ''abcdefgh'';',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: a constant that fits, a
    non-constant source and a capacity written as a named const, whose
    LengthExpr text does not convert. Live carries a wide-to-ANSI assignment. }
  cShortStringCompliant: array[0..37] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'const',
    '  cMax = 4;',
    'procedure OnFits;',
    'procedure OnVariableSource;',
    'procedure OnNamedCapacity;',
    'implementation',
    'procedure OnFits;',
    'var',
    '  s: string[4];',
    'begin',
    '  s := ''abcd'';',
    'end;',
    'procedure OnVariableSource;',
    'var',
    '  s: string[4];',
    '  t: AnsiString;',
    'begin',
    '  t := ''abcdefgh'';',
    '  s := t;',
    'end;',
    'procedure OnNamedCapacity;',
    'var',
    '  s: string[cMax];',
    'begin',
    '  s := ''abcdefgh'';',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The assigned value is a variable, so no length folds; the wide-to-ANSI
    assignment is the live sibling. }
  cShortStringVariableSource: array[0..17] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: string[4];',
    '  t: AnsiString;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  t := ''abcdefgh'';',
    '  s := t;',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cCharNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    'begin',
    '  c := ''a'';',
    '  if c = ''ab'' then',
    '    s := '''';',
    'end;',
    'end.');

  cCharEmptyNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    'begin',
    '  c := ''a'';',
    '  if c = '''' then',
    '    s := '''';',
    'end;',
    'end.');

  cCharInequalityNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    'begin',
    '  c := ''a'';',
    '  if c <> ''ab'' then',
    '    s := '''';',
    'end;',
    'end.');

  // The string constant is the left operand, so the query reads it mirrored.
  cCharMirroredNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    'begin',
    '  c := ''a'';',
    '  if ''ab'' = c then',
    '    s := '''';',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: a single-character literal, which
    resolves to a char, and a string variable, which does not fold. Live
    carries a wide-to-ANSI assignment. }
  cCharCompliant: array[0..33] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure OnCharLiteral;',
    'procedure OnStringVariable;',
    'implementation',
    'procedure OnCharLiteral;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    'begin',
    '  c := ''a'';',
    '  if c = ''a'' then',
    '    s := '''';',
    'end;',
    'procedure OnStringVariable;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    'begin',
    '  c := ''a'';',
    '  s := '''';',
    '  if c = s then',
    '    s := '''';',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  // The compared literal is one printed character held in two UTF-8 bytes.
  cCharNonAsciiLiteral: array[0..19] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  c: AnsiChar;',
    '  w: WideChar;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  c := ''a'';',
    '  w := ''b'';',
    '  if w = ''' + #$C3 + #$A9 + ''' then',
    '    a := '''';',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The compared text is a variable, so no length folds; the wide-to-ANSI
    assignment is the live sibling. }
  cCharStringVariable: array[0..19] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  c: AnsiChar;',
    '  s: AnsiString;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  c := ''a'';',
    '  s := '''';',
    '  if c = s then',
    '    a := '''';',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cRawByteNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  r: RawByteString;',
    '  c: AnsiString(1252);',
    'begin',
    '  r := c;',
    'end;',
    'end.');

  // The code-paged declaration is the assignment target rather than its source.
  cRawByteReversed: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  r: RawByteString;',
    '  c: AnsiString(1252);',
    'begin',
    '  c := r;',
    'end;',
    'end.');

  { The written cast is the conversion the rule asks for, on either side; the
    wide-to-ANSI cast below them is the live sibling. }
  cRawByteWrittenCast: array[0..16] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  r: RawByteString;',
    '  c: AnsiString(1252);',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  c := AnsiString(r);',
    '  c := RawByteString(r);',
    '  a := AnsiString(u);',
    'end;',
    'end.');

  { The code-paged operand is an argument declared through a named type, so the
    argument arm and the two-link alias chain are both read. }
  cRawByteArgument: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TW1252 = type AnsiString(1252);',
    'procedure Run(const aText: TW1252);',
    'implementation',
    'procedure Run(const aText: TW1252);',
    'var',
    '  r: RawByteString;',
    'begin',
    '  r := aText;',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: a target whose declaration writes
    no code page. Live carries a wide-to-ANSI assignment. }
  cRawByteCompliant: array[0..20] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure OnPlainAnsi;',
    'implementation',
    'procedure OnPlainAnsi;',
    'var',
    '  r: RawByteString;',
    '  a: AnsiString;',
    'begin',
    '  a := r;',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { Neither declaration writes a code page; the wide-to-ANSI assignment is the
    live sibling. }
  cRawBytePlainAnsi: array[0..15] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  r: RawByteString;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  a := r;',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cConcatNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, x: AnsiString;',
    '  i: Integer;',
    'begin',
    '  x := '''';',
    '  for i := 1 to 3 do',
    '    s := s + x;',
    'end;',
    'end.');

  cConcatWhile: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, x: AnsiString;',
    '  b: Boolean;',
    'begin',
    '  b := True;',
    '  while b do',
    '    s := s + x;',
    'end;',
    'end.');

  cConcatRepeat: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, x: AnsiString;',
    '  b: Boolean;',
    'begin',
    '  b := True;',
    '  repeat',
    '    s := s + x;',
    '  until b;',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: the same assignment outside any
    loop, a different accumulation target and an integer accumulation. Live
    carries a wide-to-ANSI assignment. }
  cConcatCompliant: array[0..36] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure OnOutsideLoop;',
    'procedure OnDifferentTarget;',
    'procedure OnIntegerTarget;',
    'implementation',
    'procedure OnOutsideLoop;',
    'var',
    '  s, x: AnsiString;',
    'begin',
    '  s := s + x;',
    'end;',
    'procedure OnDifferentTarget;',
    'var',
    '  s, t, x: AnsiString;',
    '  i: Integer;',
    'begin',
    '  for i := 1 to 3 do',
    '    s := t + x;',
    'end;',
    'procedure OnIntegerTarget;',
    'var',
    '  n, i: Integer;',
    'begin',
    '  for i := 1 to 3 do',
    '    n := n + 1;',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The accumulated string and the left operand are different declarations; the
    wide-to-ANSI assignment is the live sibling. }
  cConcatDifferentTarget: array[0..17] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s, t, x: AnsiString;',
    '  i: Integer;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  for i := 1 to 3 do',
    '    s := t + x;',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cStrToIntNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: AnsiString;',
    '  n: Integer;',
    'begin',
    '  s := '''';',
    '  n := StrToInt(s);',
    'end;',
    'end.');

  // A try..finally handles nothing, so the call it wraps is still unguarded.
  cStrToIntInFinally: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: AnsiString;',
    '  n: Integer;',
    'begin',
    '  s := '''';',
    '  try',
    '    n := 0;',
    '  finally',
    '    n := StrToInt(s);',
    '  end;',
    'end;',
    'end.');

  // A handler does not guard its own body.
  cStrToIntInHandler: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: AnsiString;',
    '  n: Integer;',
    'begin',
    '  s := '''';',
    '  try',
    '    n := 0;',
    '  except',
    '    n := StrToInt(s);',
    '  end;',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: an enclosing handler, a literal
    argument, a named constant argument and StrToIntDef. Live carries a
    wide-to-ANSI assignment. }
  cStrToIntCompliant: array[0..51] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'const',
    '  cText = ''12'';',
    'procedure OnHandler;',
    'procedure OnLiteral;',
    'procedure OnNamedConst;',
    'procedure OnDefault;',
    'implementation',
    'procedure OnHandler;',
    'var',
    '  s: AnsiString;',
    '  n: Integer;',
    'begin',
    '  s := '''';',
    '  try',
    '    n := StrToInt(s);',
    '  except',
    '    n := 0;',
    '  end;',
    'end;',
    'procedure OnLiteral;',
    'var',
    '  n: Integer;',
    'begin',
    '  n := StrToInt(''12'');',
    'end;',
    'procedure OnNamedConst;',
    'var',
    '  n: Integer;',
    'begin',
    '  n := StrToInt(cText);',
    'end;',
    'procedure OnDefault;',
    'var',
    '  s: AnsiString;',
    '  n: Integer;',
    'begin',
    '  s := '''';',
    '  n := StrToIntDef(s, 0);',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The argument folds to a constant; the wide-to-ANSI assignment is the live
    sibling. }
  cStrToIntLiteral: array[0..16] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  n: Integer;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  n := StrToInt(''12'');',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { A method carrying the RTL StrToInt signature is not the unit-level routine,
    so only its owner separates it; the wide-to-ANSI assignment is the live
    sibling. }
  cStrToIntMethodNamed: array[0..26] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  TParser = class',
    '    function StrToInt(const aValue: AnsiString): Longint;',
    '  end;',
    'procedure Run(aParser: TParser);',
    'implementation',
    'function TParser.StrToInt(const aValue: AnsiString): Longint;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run(aParser: TParser);',
    'var',
    '  s: AnsiString;',
    '  n: Integer;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  s := '''';',
    '  n := aParser.StrToInt(s);',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cWideNoncompliant: array[0..6] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'var',
    '  w: WideString;',
    'implementation',
    'end.');

  // One declaration per kind the walk reaches, plus an alias of WideString.
  cWideEveryKind: array[0..18] of string = (
    'unit shapes;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TW = WideString;',
    '  TBox = class',
    '    Field: WideString;',
    '  end;',
    'var',
    '  Global: WideString;',
    'procedure Run(aText: WideString);',
    'implementation',
    'procedure Run(aText: WideString);',
    'var',
    '  lLocal: WideString;',
    '  lAlias: TW;',
    'begin',
    'end;',
    'end.');

  { One routine per near-miss of the matrix: a UnicodeString local and a
    WideString local the analysed defines exclude. Live carries a wide-to-ANSI
    assignment. }
  cWideCompliant: array[0..27] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure OnUnicodeString;',
    'procedure OnWindowsOnly;',
    'implementation',
    'procedure OnUnicodeString;',
    'var',
    '  lText: UnicodeString;',
    'begin',
    'end;',
    'procedure OnWindowsOnly;',
    'var',
    '{$ifdef WINDOWS}',
    '  lWide: WideString;',
    '{$endif}',
    '  lCount: Integer;',
    'begin',
    'end;',
    'procedure Live;',
    'var',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { UnicodeString is a different base type; the wide-to-ANSI assignment is the
    live sibling. }
  cWideUnicodeString: array[0..14] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lText: UnicodeString;',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { The only WideString declaration sits behind WINDOWS, which the harness's
    defines exclude, so it is not in the tree at all. }
  cWideWindowsOnly: array[0..16] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '{$ifdef WINDOWS}',
    '  lWide: WideString;',
    '{$endif}',
    '  a: AnsiString;',
    '  u: UnicodeString;',
    'begin',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  { Every fill fixture ends on the two-line conversion that keeps the resolver
    visibly live; the resize is on line 13 and the unfilled read on line 14. }
  cFillNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillFilled: array[0..20] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    '  i: Integer;',
    'begin',
    '  SetLength(lS, 4);',
    '  for i := 1 to 4 do',
    '    lS[i] := '' '';',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  // The read is judged before the write, so line 14 reports.
  cFillReadAndWrite: array[0..17] of string = (
    'unit readwrite;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  lS[1] := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillGrown: array[0..18] of string = (
    'unit grown;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  lS := ''ab'';',
    '  SetLength(lS, 4);',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  // The declaration carries its own content, which the resize preserves.
  cFillInitialised: array[0..17] of string = (
    'unit initialised;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString = ''abcd'';',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillHandover: array[0..22] of string = (
    'unit handover;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Fill(var aBuf: AnsiString);',
    'begin',
    '  aBuf := ''....'';',
    'end;',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  Fill(lS);',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillTwoSites: array[0..18] of string = (
    'unit twosites;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  SetLength(lS, 8);',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillAddressEscape: array[0..19] of string = (
    'unit escape;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    '  p: Pointer;',
    'begin',
    '  SetLength(lS, 4);',
    '  p := @lS;',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillAbsoluteAlias: array[0..19] of string = (
    'unit alias;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  lAlias: AnsiString absolute lS;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  lAlias := ''....'';',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  cFillNestedMention: array[0..22] of string = (
    'unit nested;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    '  procedure Inner;',
    '  begin',
    '    lS := ''....'';',
    '  end;',
    'begin',
    '  SetLength(lS, 4);',
    '  Inner;',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

  // The asm statement cannot be classified, which takes the whole routine out.
  cFillInlineAssembler: array[0..20] of string = (
    'unit operand;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lS: AnsiString;',
    '  c: AnsiChar;',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  SetLength(lS, 4);',
    '  asm',
    '    nop',
    '  end;',
    '  c := lS[1];',
    '  u := '''';',
    '  a := u;',
    'end;',
    'end.');

// aSource with its line-2 mode directive replaced by aMode.
function InMode(const aSource: array of string;
  const aMode: string): TFpSonarStringArray;

var
  i: Integer;

begin
  SetLength(Result, Length(aSource));
  for i := 0 to High(aSource) do
    Result[i] := aSource[i];
  Result[1] := aMode;
end;


procedure TRulesStringsTest.RunRule(aRule: TRuleBase; const aFixture: string;
  aWithhold: boolean; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    if aWithhold then
      lEngine.Analyze(aFixture, cMode, cDefines, [], [], True, SizeOf(Pointer),
        aCollector)
    else
      lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesStringsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesStringsTest.FirstById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      begin
        Result := i;
        Exit;
      end;
end;


function TRulesStringsTest.RuleCount(aRule: TRuleBase; aWithhold: boolean;
  const aSource: array of string): Integer;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lId: string;

begin
  // Read before RunRule, whose registry takes ownership of aRule.
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


procedure TRulesStringsTest.CheckIssueAt(aRule: TRuleBase; const aId: string;
  aLine: Integer; const aArgs: array of string;
  const aSource: array of string);

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('probe.pas', aSource), False, lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('arg count', Length(aArgs),
        Length(lc.Issues[k].MessageArgs));
      for m := 0 to High(aArgs) do
        AssertEquals('arg ' + IntToStr(m), aArgs[m],
          lc.Issues[k].MessageArgs[m]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesStringsTest.CheckSilentWithLiveSibling(aRule,
  aSibling: TRuleBase; const aId, aSiblingId: string;
  const aSource: array of string);

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
      AssertEquals('an unresolved fact is silent', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aSibling, lPath, False, lc);
      AssertEquals('the resolver was live', 1, CountById(lc, aSiblingId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesStringsTest.NewPCharOfTemporaryString: TRuleBase;

begin
  Result := TRulePCharOfTemporaryString.Create(TRuleMetadata.Make(
    cPCharOfTemporaryStringId, rtSem, rfResolver, sevCritical, itBug, cfMedium,
    True, ''));
end;


function TRulesStringsTest.NewImplicitStringConversionWithDataLoss: TRuleBase;

begin
  Result := TRuleImplicitStringConversionWithDataLoss.Create(
    TRuleMetadata.Make(cImplicitStringConversionWithDataLossId, rtSem,
    rfResolver, sevMajor, itBug, cfMedium, True, ''));
end;


function TRulesStringsTest.NewLengthUsedAsByteCount: TRuleBase;

begin
  Result := TRuleLengthUsedAsByteCount.Create(TRuleMetadata.Make(
    cLengthUsedAsByteCountId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesStringsTest.NewUnicodeToAnsiCast: TRuleBase;

begin
  Result := TRuleUnicodeToAnsiCast.Create(TRuleMetadata.Make(
    cUnicodeToAnsiCastId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesStringsTest.NewCharToCharPointerCast: TRuleBase;

begin
  Result := TRuleCharToCharPointerCast.Create(TRuleMetadata.Make(
    cCharToCharPointerCastId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesStringsTest.NewMoveFillCharSizeMismatch: TRuleBase;

begin
  Result := TRuleMoveFillCharSizeMismatch.Create(TRuleMetadata.Make(
    cMoveFillCharSizeMismatchId, rtSem, rfResolver, sevCritical, itBug,
    cfMedium, True, ''));
end;


function TRulesStringsTest.NewCopyWithZeroIndex: TRuleBase;

begin
  Result := TRuleCopyWithZeroIndex.Create(TRuleMetadata.Make(
    cCopyWithZeroIndexId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesStringsTest.NewPosResultComparedToZeroBased: TRuleBase;

begin
  Result := TRulePosResultComparedToZeroBased.Create(TRuleMetadata.Make(
    cPosResultComparedToZeroBasedId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesStringsTest.NewShortStringTruncation: TRuleBase;

begin
  Result := TRuleShortStringTruncation.Create(TRuleMetadata.Make(
    cShortStringTruncationId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesStringsTest.NewCharComparedToString: TRuleBase;

begin
  Result := TRuleCharComparedToString.Create(TRuleMetadata.Make(
    cCharComparedToStringId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesStringsTest.NewStringFirstCharByIndex: TRuleBase;

begin
  Result := TRuleStringFirstCharByIndex.Create(TRuleMetadata.Make(
    cStringFirstCharByIndexId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesStringsTest.NewComparisonAlwaysTrueForType: TRuleBase;

begin
  Result := TRuleComparisonAlwaysTrueForType.Create(TRuleMetadata.Make(
    cComparisonAlwaysTrueForTypeId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfMedium, True, ''));
end;


function TRulesStringsTest.NewRawByteStringCodePageMix: TRuleBase;

begin
  Result := TRuleRawByteStringCodePageMix.Create(TRuleMetadata.Make(
    cRawByteStringCodePageMixId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesStringsTest.NewStringConcatInLoop: TRuleBase;

begin
  Result := TRuleStringConcatInLoop.Create(TRuleMetadata.Make(
    cStringConcatInLoopId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStringsTest.NewStrToIntWithoutGuard: TRuleBase;

begin
  Result := TRuleStrToIntWithoutGuard.Create(TRuleMetadata.Make(
    cStrToIntWithoutGuardId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesStringsTest.NewWideStringOnNonWindows: TRuleBase;

begin
  Result := TRuleWideStringOnNonWindows.Create(TRuleMetadata.Make(
    cWideStringOnNonWindowsId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfMedium, True, ''));
end;


function TRulesStringsTest.NewSetLengthWithoutFill: TRuleBase;

begin
  Result := TRuleSetLengthWithoutFill.Create(TRuleMetadata.Make(
    cSetLengthWithoutFillId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, True, ''));
end;


procedure TRulesStringsTest.PCharOfTemporaryStringPositions;

begin
  // Noncompliant: 'p := PAnsiChar(MakeName);' (line 15); the arg is the target
  // pointer-type name. The concatenation shape is the second temporary the
  // story names, and it fires at line 12 of its own fixture.
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 15,
    ['PAnsiChar'], cPCharNoncompliant);
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 12,
    ['PAnsiChar'], cPCharConcatNoncompliant);
  CheckSilentWithLiveSibling(NewPCharOfTemporaryString,
    NewImplicitStringConversionWithDataLoss, cPCharOfTemporaryStringId,
    cImplicitStringConversionWithDataLossId, cPCharCompliant);
end;


procedure TRulesStringsTest.PCharOfTemporaryStringSeesEveryCallShape;

begin
  // A written argument list carries the reference on the callee and a dotted
  // callee on its rightmost name.
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 15,
    ['PAnsiChar'], cPCharCallArgsNoncompliant);
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 18,
    ['PAnsiChar'], cPCharQualifiedNoncompliant);
end;


procedure TRulesStringsTest.PCharOfTemporaryStringSeesBuiltInStringCall;

begin
  // A built-in call resolves to a symbol reference carrying its built-in data,
  // never to a TPasProcedure.
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 12,
    ['PAnsiChar'], cPCharCopyNoncompliant);
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 12,
    ['PAnsiChar'], cPCharConcatCallNoncompliant);
end;


procedure TRulesStringsTest.PCharOfTemporaryStringPositionsPerMode;

begin
  // The fixture differs across the three modes in its {$mode} line alone, and
  // the assertion is positive in each.
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 15,
    ['PAnsiChar'], InMode(cPCharNoncompliant, cModeObjfpc));
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 15,
    ['PAnsiChar'], InMode(cPCharNoncompliant, cModeDelphi));
  CheckIssueAt(NewPCharOfTemporaryString, cPCharOfTemporaryStringId, 15,
    ['PAnsiChar'], InMode(cPCharNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.PCharOfTemporaryStringDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewPCharOfTemporaryString, True, cPCharNoncompliant));
end;


procedure TRulesStringsTest.PCharOfTemporaryStringSilentOnUnresolvedOperand;

begin
  CheckSilentWithLiveSibling(NewPCharOfTemporaryString,
    NewImplicitStringConversionWithDataLoss, cPCharOfTemporaryStringId,
    cImplicitStringConversionWithDataLossId, cPCharUnresolved);
end;


procedure TRulesStringsTest.ImplicitStringConversionWithDataLossPositions;

begin
  // Noncompliant: 'a := u;' (line 12); the args are the source and target type
  // names, in that order.
  CheckIssueAt(NewImplicitStringConversionWithDataLoss,
    cImplicitStringConversionWithDataLossId, 12,
    ['UnicodeString', 'AnsiString'], cImplicitNoncompliant);
  CheckSilentWithLiveSibling(NewImplicitStringConversionWithDataLoss,
    NewPCharOfTemporaryString, cImplicitStringConversionWithDataLossId,
    cPCharOfTemporaryStringId, cImplicitCompliant);
end;


procedure TRulesStringsTest.ImplicitStringConversionWithDataLossPositionsPerMode;

begin
  CheckIssueAt(NewImplicitStringConversionWithDataLoss,
    cImplicitStringConversionWithDataLossId, 12,
    ['UnicodeString', 'AnsiString'], InMode(cImplicitNoncompliant, cModeObjfpc));
  CheckIssueAt(NewImplicitStringConversionWithDataLoss,
    cImplicitStringConversionWithDataLossId, 12,
    ['UnicodeString', 'AnsiString'], InMode(cImplicitNoncompliant, cModeDelphi));
  CheckIssueAt(NewImplicitStringConversionWithDataLoss,
    cImplicitStringConversionWithDataLossId, 12,
    ['UnicodeString', 'AnsiString'],
    InMode(cImplicitNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.
  ImplicitStringConversionWithDataLossDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewImplicitStringConversionWithDataLoss, True,
    cImplicitNoncompliant));
end;


procedure TRulesStringsTest.
  ImplicitStringConversionWithDataLossSilentOnNonStringSource;

begin
  CheckSilentWithLiveSibling(NewImplicitStringConversionWithDataLoss,
    NewPCharOfTemporaryString, cImplicitStringConversionWithDataLossId,
    cPCharOfTemporaryStringId, cImplicitNonString);
end;


procedure TRulesStringsTest.ImplicitStringConversionDisjointFromUnicodeToAnsiCast;

begin
  // A written cast makes the right side resolve ANSI.
  AssertEquals('written cast => implicit rule silent', 0,
    RuleCount(NewImplicitStringConversionWithDataLoss, False, cCastWritten));
  AssertEquals('written cast => UnicodeToAnsiCast fires', 1,
    RuleCount(NewUnicodeToAnsiCast, False, cCastWritten));
end;


procedure TRulesStringsTest.PCharOfTemporaryStringDisjointFromCharToCharPointerCast;

begin
  // Both rules test the same pointer-to-char target and are held apart by the
  // operand kind alone: ltkString here, ltkChar there.
  AssertEquals('string operand => CharToCharPointerCast silent', 0,
    RuleCount(NewCharToCharPointerCast, False, cPCharNoncompliant));
  AssertEquals('string operand => PCharOfTemporaryString fires', 1,
    RuleCount(NewPCharOfTemporaryString, False, cPCharNoncompliant));
end;


procedure TRulesStringsTest.LengthUsedAsByteCountPositions;

begin
  // Noncompliant: 'Move(u[1], d[1], Length(u));' (line 14); the args are the
  // counted string's type and the routine that expects bytes.
  CheckIssueAt(NewLengthUsedAsByteCount, cLengthUsedAsByteCountId, 14,
    ['UnicodeString', 'Move'], cLengthNoncompliant);
  CheckSilentWithLiveSibling(NewLengthUsedAsByteCount,
    NewImplicitStringConversionWithDataLoss, cLengthUsedAsByteCountId,
    cImplicitStringConversionWithDataLossId, cLengthCompliant);
end;


procedure TRulesStringsTest.LengthUsedAsByteCountFillCharPosition;

begin
  // FillChar counts at argument 1 rather than 2.
  CheckIssueAt(NewLengthUsedAsByteCount, cLengthUsedAsByteCountId, 14,
    ['UnicodeString', 'FillChar'], cLengthFillCharNoncompliant);
end;


procedure TRulesStringsTest.LengthUsedAsByteCountPositionsPerMode;

begin
  CheckIssueAt(NewLengthUsedAsByteCount, cLengthUsedAsByteCountId, 14,
    ['UnicodeString', 'Move'], InMode(cLengthNoncompliant, cModeObjfpc));
  CheckIssueAt(NewLengthUsedAsByteCount, cLengthUsedAsByteCountId, 14,
    ['UnicodeString', 'Move'], InMode(cLengthNoncompliant, cModeDelphi));
  CheckIssueAt(NewLengthUsedAsByteCount, cLengthUsedAsByteCountId, 14,
    ['UnicodeString', 'Move'], InMode(cLengthNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.LengthUsedAsByteCountDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewLengthUsedAsByteCount, True, cLengthNoncompliant));
end;


procedure TRulesStringsTest.LengthUsedAsByteCountSilentOnShadowedLength;

begin
  CheckSilentWithLiveSibling(NewLengthUsedAsByteCount,
    NewImplicitStringConversionWithDataLoss, cLengthUsedAsByteCountId,
    cImplicitStringConversionWithDataLossId, cLengthShadowed);
end;


procedure TRulesStringsTest.
  LengthUsedAsByteCountDisjointFromMoveFillCharSizeMismatch;

begin
  // An array count is the sibling's population; a string's collapsed type
  // element is never a TPasArrayType. Asserted in both directions.
  AssertEquals('array count => this rule silent', 0,
    RuleCount(NewLengthUsedAsByteCount, False, cLengthArrayCount));
  AssertEquals('array count => MoveFillCharSizeMismatch fires', 1,
    RuleCount(NewMoveFillCharSizeMismatch, False, cLengthArrayCount));
end;


procedure TRulesStringsTest.BareStringClassifiesTheSameInEveryMode;

var
  lObjfpc, lDelphi, lDelphiUnicode: Integer;

begin
  // The count over a bare-string fixture, measured in each of the three modes.
  lObjfpc := RuleCount(NewImplicitStringConversionWithDataLoss, False,
    InMode(cBareString, cModeObjfpc));
  lDelphi := RuleCount(NewImplicitStringConversionWithDataLoss, False,
    InMode(cBareString, cModeDelphi));
  lDelphiUnicode := RuleCount(NewImplicitStringConversionWithDataLoss, False,
    InMode(cBareString, cModeDelphiUnicode));
  AssertEquals('delphi matches objfpc', lObjfpc, lDelphi);
  AssertEquals('delphiunicode matches objfpc', lObjfpc, lDelphiUnicode);
  AssertEquals('bare string classifies ANSI, so the assignment is reported',
    1, lObjfpc);
end;


procedure TRulesStringsTest.CopyWithZeroIndexPositions;

begin
  // Noncompliant: 'd := Copy(s, 0, 2);' (line 11); the arg is the copied
  // string's type name.
  CheckIssueAt(NewCopyWithZeroIndex, cCopyWithZeroIndexId, 11,
    ['AnsiString'], cCopyNoncompliant);
  CheckSilentWithLiveSibling(NewCopyWithZeroIndex,
    NewImplicitStringConversionWithDataLoss, cCopyWithZeroIndexId,
    cImplicitStringConversionWithDataLossId, cCopyCompliant);
end;


procedure TRulesStringsTest.CopyWithZeroIndexSeesBothArities;

begin
  CheckIssueAt(NewCopyWithZeroIndex, cCopyWithZeroIndexId, 11,
    ['AnsiString'], cCopyTwoArgNoncompliant);
end;


procedure TRulesStringsTest.CopyWithZeroIndexPositionsPerMode;

begin
  CheckIssueAt(NewCopyWithZeroIndex, cCopyWithZeroIndexId, 11, ['AnsiString'],
    InMode(cCopyNoncompliant, cModeObjfpc));
  CheckIssueAt(NewCopyWithZeroIndex, cCopyWithZeroIndexId, 11, ['AnsiString'],
    InMode(cCopyNoncompliant, cModeDelphi));
  CheckIssueAt(NewCopyWithZeroIndex, cCopyWithZeroIndexId, 11, ['AnsiString'],
    InMode(cCopyNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.CopyWithZeroIndexDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewCopyWithZeroIndex, True, cCopyNoncompliant));
end;


procedure TRulesStringsTest.CopyWithZeroIndexSilentOnNonFoldingIndex;

begin
  CheckSilentWithLiveSibling(NewCopyWithZeroIndex,
    NewImplicitStringConversionWithDataLoss, cCopyWithZeroIndexId,
    cImplicitStringConversionWithDataLossId, cCopyNonFoldingIndex);
end;


procedure TRulesStringsTest.CopyWithZeroIndexDisjointFromStringFirstCharByIndex;

begin
  // The two 1-based-index rules partition their sites: this one needs a
  // pekFuncParams built-in call, the sibling a pekArrayParams access.
  AssertEquals('an indexed access => this rule silent', 0,
    RuleCount(NewCopyWithZeroIndex, False, cCopyStringIndex));
  AssertEquals('an indexed access => StringFirstCharByIndex fires', 1,
    RuleCount(NewStringFirstCharByIndex, False, cCopyStringIndex));
  AssertEquals('a zero-index Copy => StringFirstCharByIndex silent', 0,
    RuleCount(NewStringFirstCharByIndex, False, cCopyNoncompliant));
end;


procedure TRulesStringsTest.PosResultComparedToZeroBasedPositions;

begin
  // Noncompliant: 'if Pos(x, s) >= 0 then' (line 12); the args are the routine
  // name and the constant it is compared to.
  CheckIssueAt(NewPosResultComparedToZeroBased,
    cPosResultComparedToZeroBasedId, 12, ['Pos', '0'], cPosNoncompliant);
  CheckSilentWithLiveSibling(NewPosResultComparedToZeroBased,
    NewImplicitStringConversionWithDataLoss, cPosResultComparedToZeroBasedId,
    cImplicitStringConversionWithDataLossId, cPosCompliant);
end;


procedure TRulesStringsTest.PosResultComparedToZeroBasedSeesEveryComparedShape;

begin
  // The mirrored operand order reports the same constant, and a comparison
  // against -1 reports that constant instead.
  CheckIssueAt(NewPosResultComparedToZeroBased,
    cPosResultComparedToZeroBasedId, 12, ['Pos', '0'],
    cPosMirroredNoncompliant);
  CheckIssueAt(NewPosResultComparedToZeroBased,
    cPosResultComparedToZeroBasedId, 12, ['Pos', '-1'],
    cPosMinusOneNoncompliant);
  AssertEquals('every entry of the comparison table fires', 6,
    RuleCount(NewPosResultComparedToZeroBased, False, cPosEveryShape));
end;


procedure TRulesStringsTest.PosResultComparedToZeroBasedPositionsPerMode;

begin
  CheckIssueAt(NewPosResultComparedToZeroBased,
    cPosResultComparedToZeroBasedId, 12, ['Pos', '0'],
    InMode(cPosNoncompliant, cModeObjfpc));
  CheckIssueAt(NewPosResultComparedToZeroBased,
    cPosResultComparedToZeroBasedId, 12, ['Pos', '0'],
    InMode(cPosNoncompliant, cModeDelphi));
  CheckIssueAt(NewPosResultComparedToZeroBased,
    cPosResultComparedToZeroBasedId, 12, ['Pos', '0'],
    InMode(cPosNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.PosResultComparedToZeroBasedDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewPosResultComparedToZeroBased, True, cPosNoncompliant));
end;


procedure TRulesStringsTest.PosResultComparedToZeroBasedSilentOnMethodNamedPos;

begin
  CheckSilentWithLiveSibling(NewPosResultComparedToZeroBased,
    NewImplicitStringConversionWithDataLoss, cPosResultComparedToZeroBasedId,
    cImplicitStringConversionWithDataLossId, cPosMethodNamed);
end;


procedure TRulesStringsTest.ShortStringTruncationPositions;

begin
  // Noncompliant: 's := ''abcdefgh'';' (line 10); the args are the target and
  // the length declared on its string[N].
  CheckIssueAt(NewShortStringTruncation, cShortStringTruncationId, 10,
    ['s', '4'], cShortStringNoncompliant);
  CheckSilentWithLiveSibling(NewShortStringTruncation,
    NewImplicitStringConversionWithDataLoss, cShortStringTruncationId,
    cImplicitStringConversionWithDataLossId, cShortStringCompliant);
end;


procedure TRulesStringsTest.ShortStringTruncationPositionsPerMode;

begin
  CheckIssueAt(NewShortStringTruncation, cShortStringTruncationId, 10,
    ['s', '4'], InMode(cShortStringNoncompliant, cModeObjfpc));
  CheckIssueAt(NewShortStringTruncation, cShortStringTruncationId, 10,
    ['s', '4'], InMode(cShortStringNoncompliant, cModeDelphi));
  CheckIssueAt(NewShortStringTruncation, cShortStringTruncationId, 10,
    ['s', '4'], InMode(cShortStringNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.ShortStringTruncationDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewShortStringTruncation, True, cShortStringNoncompliant));
end;


procedure TRulesStringsTest.ShortStringTruncationSilentOnNonConstantSource;

begin
  CheckSilentWithLiveSibling(NewShortStringTruncation,
    NewImplicitStringConversionWithDataLoss, cShortStringTruncationId,
    cImplicitStringConversionWithDataLossId, cShortStringVariableSource);
end;


procedure TRulesStringsTest.
  ShortStringTruncationDisjointFromImplicitStringConversion;

begin
  // The truncated source is an ANSI literal, so the conversion rule has no
  // wide source to report. Asserted in both directions.
  AssertEquals('an ANSI literal => the conversion rule is silent', 0,
    RuleCount(NewImplicitStringConversionWithDataLoss, False,
    cShortStringNoncompliant));
  AssertEquals('an ANSI literal => ShortStringTruncation fires', 1,
    RuleCount(NewShortStringTruncation, False, cShortStringNoncompliant));
end;


procedure TRulesStringsTest.CharComparedToStringPositions;

begin
  // Noncompliant: 'if c = ''ab'' then' (line 12); the args are the char
  // operand's type and the constant's character count.
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '2'], cCharNoncompliant);
  CheckSilentWithLiveSibling(NewCharComparedToString,
    NewImplicitStringConversionWithDataLoss, cCharComparedToStringId,
    cImplicitStringConversionWithDataLossId, cCharCompliant);
end;


procedure TRulesStringsTest.CharComparedToStringSeesEveryComparedShape;

begin
  // The empty constant, the inequality operator and the mirrored operand
  // order all report the same way.
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '0'], cCharEmptyNoncompliant);
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '2'], cCharInequalityNoncompliant);
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '2'], cCharMirroredNoncompliant);
end;


procedure TRulesStringsTest.CharComparedToStringPositionsPerMode;

begin
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '2'], InMode(cCharNoncompliant, cModeObjfpc));
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '2'], InMode(cCharNoncompliant, cModeDelphi));
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 12,
    ['AnsiChar', '2'], InMode(cCharNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.CharComparedToStringDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewCharComparedToString, True, cCharNoncompliant));
end;


procedure TRulesStringsTest.CharComparedToStringSilentOnStringVariable;

begin
  CheckSilentWithLiveSibling(NewCharComparedToString,
    NewImplicitStringConversionWithDataLoss, cCharComparedToStringId,
    cImplicitStringConversionWithDataLossId, cCharStringVariable);
end;


procedure TRulesStringsTest.CharComparedToStringSeesNonAsciiLiteralAsTwoChars;

begin
  // Without {$codepage utf8} the scanner reads the two UTF-8 bytes as two
  // characters, which is also how the compiler compares them.
  CheckIssueAt(NewCharComparedToString, cCharComparedToStringId, 15,
    ['WideChar', '2'], cCharNonAsciiLiteral);
end;


procedure TRulesStringsTest.
  ZeroBasedComparisonsDisjointFromComparisonAlwaysTrueForType;

begin
  { The type-range rule cannot settle either verdict: Longint's range leaves
    'Pos(...) >= 0' satisfiable, and a string constant is not an integer
    limit. }
  AssertEquals('the Pos shape => the range rule is silent', 0,
    RuleCount(NewComparisonAlwaysTrueForType, False, cPosNoncompliant));
  AssertEquals('the char shape => the range rule is silent', 0,
    RuleCount(NewComparisonAlwaysTrueForType, False, cCharNoncompliant));
  AssertEquals('the Pos shape => PosResultComparedToZeroBased fires', 1,
    RuleCount(NewPosResultComparedToZeroBased, False, cPosNoncompliant));
  AssertEquals('the char shape => CharComparedToString fires', 1,
    RuleCount(NewCharComparedToString, False, cCharNoncompliant));
end;


procedure TRulesStringsTest.RawByteStringCodePageMixPositions;

begin
  // Noncompliant: 'r := c;' (line 11); the args are the RawByteString
  // declaration and the code page written on the other side.
  CheckIssueAt(NewRawByteStringCodePageMix, cRawByteStringCodePageMixId, 11,
    ['r', '1252'], cRawByteNoncompliant);
  CheckSilentWithLiveSibling(NewRawByteStringCodePageMix,
    NewImplicitStringConversionWithDataLoss, cRawByteStringCodePageMixId,
    cImplicitStringConversionWithDataLossId, cRawByteCompliant);
end;


procedure TRulesStringsTest.RawByteStringCodePageMixSeesBothDirections;

begin
  CheckIssueAt(NewRawByteStringCodePageMix, cRawByteStringCodePageMixId, 11,
    ['r', '1252'], cRawByteReversed);
end;


procedure TRulesStringsTest.RawByteStringCodePageMixSeesCodePagedArgument;

begin
  CheckIssueAt(NewRawByteStringCodePageMix, cRawByteStringCodePageMixId, 12,
    ['r', '1252'], cRawByteArgument);
end;


procedure TRulesStringsTest.RawByteStringCodePageMixPositionsPerMode;

begin
  CheckIssueAt(NewRawByteStringCodePageMix, cRawByteStringCodePageMixId, 11,
    ['r', '1252'], InMode(cRawByteNoncompliant, cModeObjfpc));
  CheckIssueAt(NewRawByteStringCodePageMix, cRawByteStringCodePageMixId, 11,
    ['r', '1252'], InMode(cRawByteNoncompliant, cModeDelphi));
  CheckIssueAt(NewRawByteStringCodePageMix, cRawByteStringCodePageMixId, 11,
    ['r', '1252'], InMode(cRawByteNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.RawByteStringCodePageMixDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewRawByteStringCodePageMix, True, cRawByteNoncompliant));
end;


procedure TRulesStringsTest.RawByteStringCodePageMixSilentOnPlainAnsiString;

begin
  CheckSilentWithLiveSibling(NewRawByteStringCodePageMix,
    NewImplicitStringConversionWithDataLoss, cRawByteStringCodePageMixId,
    cImplicitStringConversionWithDataLossId, cRawBytePlainAnsi);
end;


procedure TRulesStringsTest.
  RawByteStringCodePageMixDisjointFromImplicitStringConversion;

begin
  // Both sides of a code-page mix are ANSI-encoded, which is the source
  // encoding the sibling needs. Asserted in both directions.
  AssertEquals('a code-page mix => the implicit rule is silent', 0,
    RuleCount(NewImplicitStringConversionWithDataLoss, False,
    cRawByteNoncompliant));
  AssertEquals('a code-page mix => RawByteStringCodePageMix fires', 1,
    RuleCount(NewRawByteStringCodePageMix, False, cRawByteNoncompliant));
end;


procedure TRulesStringsTest.RawByteStringCodePageMixSilentOnWrittenCast;

begin
  CheckSilentWithLiveSibling(NewRawByteStringCodePageMix, NewUnicodeToAnsiCast,
    cRawByteStringCodePageMixId, cUnicodeToAnsiCastId, cRawByteWrittenCast);
end;


procedure TRulesStringsTest.StringConcatInLoopPositions;

begin
  // Noncompliant: 's := s + x;' inside a for loop (line 13); the arg is the
  // accumulated declaration.
  CheckIssueAt(NewStringConcatInLoop, cStringConcatInLoopId, 13, ['s'],
    cConcatNoncompliant);
  CheckSilentWithLiveSibling(NewStringConcatInLoop,
    NewImplicitStringConversionWithDataLoss, cStringConcatInLoopId,
    cImplicitStringConversionWithDataLossId, cConcatCompliant);
end;


procedure TRulesStringsTest.StringConcatInLoopSeesEveryLoopShape;

begin
  CheckIssueAt(NewStringConcatInLoop, cStringConcatInLoopId, 13, ['s'],
    cConcatWhile);
  CheckIssueAt(NewStringConcatInLoop, cStringConcatInLoopId, 13, ['s'],
    cConcatRepeat);
end;


procedure TRulesStringsTest.StringConcatInLoopPositionsPerMode;

begin
  CheckIssueAt(NewStringConcatInLoop, cStringConcatInLoopId, 13, ['s'],
    InMode(cConcatNoncompliant, cModeObjfpc));
  CheckIssueAt(NewStringConcatInLoop, cStringConcatInLoopId, 13, ['s'],
    InMode(cConcatNoncompliant, cModeDelphi));
  CheckIssueAt(NewStringConcatInLoop, cStringConcatInLoopId, 13, ['s'],
    InMode(cConcatNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.StringConcatInLoopDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewStringConcatInLoop, True, cConcatNoncompliant));
end;


procedure TRulesStringsTest.StringConcatInLoopSilentOnDifferentTarget;

begin
  CheckSilentWithLiveSibling(NewStringConcatInLoop,
    NewImplicitStringConversionWithDataLoss, cStringConcatInLoopId,
    cImplicitStringConversionWithDataLossId, cConcatDifferentTarget);
end;


procedure TRulesStringsTest.StrToIntWithoutGuardPositions;

begin
  // Noncompliant: 'n := StrToInt(s);' (line 13); the arg is the converted
  // declaration.
  CheckIssueAt(NewStrToIntWithoutGuard, cStrToIntWithoutGuardId, 13, ['s'],
    cStrToIntNoncompliant);
  CheckSilentWithLiveSibling(NewStrToIntWithoutGuard,
    NewImplicitStringConversionWithDataLoss, cStrToIntWithoutGuardId,
    cImplicitStringConversionWithDataLossId, cStrToIntCompliant);
end;


procedure TRulesStringsTest.StrToIntWithoutGuardSeesUnprotectedTryShapes;

begin
  CheckIssueAt(NewStrToIntWithoutGuard, cStrToIntWithoutGuardId, 16, ['s'],
    cStrToIntInFinally);
  CheckIssueAt(NewStrToIntWithoutGuard, cStrToIntWithoutGuardId, 16, ['s'],
    cStrToIntInHandler);
end;


procedure TRulesStringsTest.StrToIntWithoutGuardPositionsPerMode;

begin
  CheckIssueAt(NewStrToIntWithoutGuard, cStrToIntWithoutGuardId, 13, ['s'],
    InMode(cStrToIntNoncompliant, cModeObjfpc));
  CheckIssueAt(NewStrToIntWithoutGuard, cStrToIntWithoutGuardId, 13, ['s'],
    InMode(cStrToIntNoncompliant, cModeDelphi));
  CheckIssueAt(NewStrToIntWithoutGuard, cStrToIntWithoutGuardId, 13, ['s'],
    InMode(cStrToIntNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.StrToIntWithoutGuardDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewStrToIntWithoutGuard, True, cStrToIntNoncompliant));
end;


procedure TRulesStringsTest.StrToIntWithoutGuardSilentOnLiteralArgument;

begin
  CheckSilentWithLiveSibling(NewStrToIntWithoutGuard,
    NewImplicitStringConversionWithDataLoss, cStrToIntWithoutGuardId,
    cImplicitStringConversionWithDataLossId, cStrToIntLiteral);
end;


procedure TRulesStringsTest.StrToIntWithoutGuardSilentOnMethodNamedStrToInt;

begin
  CheckSilentWithLiveSibling(NewStrToIntWithoutGuard,
    NewImplicitStringConversionWithDataLoss, cStrToIntWithoutGuardId,
    cImplicitStringConversionWithDataLossId, cStrToIntMethodNamed);
end;


procedure TRulesStringsTest.WideStringOnNonWindowsPositions;

begin
  // Noncompliant: 'w: WideString;' (line 5); the arg is the declaration name.
  CheckIssueAt(NewWideStringOnNonWindows, cWideStringOnNonWindowsId, 5, ['w'],
    cWideNoncompliant);
  CheckSilentWithLiveSibling(NewWideStringOnNonWindows,
    NewImplicitStringConversionWithDataLoss, cWideStringOnNonWindowsId,
    cImplicitStringConversionWithDataLossId, cWideCompliant);
end;


procedure TRulesStringsTest.WideStringOnNonWindowsSeesEveryDeclarationKind;

begin
  // A section variable, a class field, an argument, a routine local and a
  // local declared through an alias of WideString.
  AssertEquals('every declaration kind the walk reaches', 5,
    RuleCount(NewWideStringOnNonWindows, False, cWideEveryKind));
end;


procedure TRulesStringsTest.WideStringOnNonWindowsPositionsPerMode;

begin
  CheckIssueAt(NewWideStringOnNonWindows, cWideStringOnNonWindowsId, 5, ['w'],
    InMode(cWideNoncompliant, cModeObjfpc));
  CheckIssueAt(NewWideStringOnNonWindows, cWideStringOnNonWindowsId, 5, ['w'],
    InMode(cWideNoncompliant, cModeDelphi));
  CheckIssueAt(NewWideStringOnNonWindows, cWideStringOnNonWindowsId, 5, ['w'],
    InMode(cWideNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.WideStringOnNonWindowsDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewWideStringOnNonWindows, True, cWideNoncompliant));
end;


procedure TRulesStringsTest.WideStringOnNonWindowsSilentOnUnicodeString;

begin
  CheckSilentWithLiveSibling(NewWideStringOnNonWindows,
    NewImplicitStringConversionWithDataLoss, cWideStringOnNonWindowsId,
    cImplicitStringConversionWithDataLossId, cWideUnicodeString);
end;


procedure TRulesStringsTest.
  WideStringOnNonWindowsSilentOnWindowsOnlyDeclaration;

begin
  CheckSilentWithLiveSibling(NewWideStringOnNonWindows,
    NewImplicitStringConversionWithDataLoss, cWideStringOnNonWindowsId,
    cImplicitStringConversionWithDataLossId, cWideWindowsOnly);
end;


procedure TRulesStringsTest.SetLengthWithoutFillPositions;

begin
  CheckIssueAt(NewSetLengthWithoutFill, cSetLengthWithoutFillId, 14, ['lS'],
    cFillNoncompliant);
end;


procedure TRulesStringsTest.SetLengthWithoutFillPositionsPerMode;

begin
  CheckIssueAt(NewSetLengthWithoutFill, cSetLengthWithoutFillId, 14, ['lS'],
    InMode(cFillNoncompliant, cModeObjfpc));
  CheckIssueAt(NewSetLengthWithoutFill, cSetLengthWithoutFillId, 14, ['lS'],
    InMode(cFillNoncompliant, cModeDelphi));
  CheckIssueAt(NewSetLengthWithoutFill, cSetLengthWithoutFillId, 14, ['lS'],
    InMode(cFillNoncompliant, cModeDelphiUnicode));
end;


procedure TRulesStringsTest.
  SetLengthWithoutFillCountsReadAndWriteInOneStatement;

begin
  CheckIssueAt(NewSetLengthWithoutFill, cSetLengthWithoutFillId, 14, ['lS'],
    cFillReadAndWrite);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnFilledStorage;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillFilled);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnGrownStorage;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillGrown);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnInitialisedStorage;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillInitialised);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnCallHandover;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillHandover);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnTwoResizeSites;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillTwoSites);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnAddressEscape;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillAddressEscape);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnAbsoluteAlias;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillAbsoluteAlias);
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnNestedRoutineMention;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillNestedMention);
end;


procedure TRulesStringsTest.SetLengthWithoutFillDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewSetLengthWithoutFill, True, cFillNoncompliant));
end;


procedure TRulesStringsTest.SetLengthWithoutFillSilentOnInlineAssembler;

begin
  CheckSilentWithLiveSibling(NewSetLengthWithoutFill,
    NewImplicitStringConversionWithDataLoss, cSetLengthWithoutFillId,
    cImplicitStringConversionWithDataLossId, cFillInlineAssembler);
end;


procedure TRulesStringsTest.StringsRulesSelfRegisterGlobally;

begin
  // The production initialization registered all twelve SEM string rules into
  // the global registry.
  AssertTrue('PCharOfTemporaryString registered',
    RuleRegistry.FindById(cPCharOfTemporaryStringId) <> nil);
  AssertFalse('PCharOfTemporaryString ships disabled',
    RuleRegistry.FindById(cPCharOfTemporaryStringId).Metadata.DefaultEnabled);
  AssertTrue('ImplicitStringConversionWithDataLoss registered',
    RuleRegistry.FindById(cImplicitStringConversionWithDataLossId) <> nil);
  AssertFalse('ImplicitStringConversionWithDataLoss ships disabled',
    RuleRegistry.FindById(
    cImplicitStringConversionWithDataLossId).Metadata.DefaultEnabled);
  AssertTrue('LengthUsedAsByteCount registered',
    RuleRegistry.FindById(cLengthUsedAsByteCountId) <> nil);
  AssertFalse('LengthUsedAsByteCount ships disabled',
    RuleRegistry.FindById(cLengthUsedAsByteCountId).Metadata.DefaultEnabled);
  AssertTrue('CopyWithZeroIndex registered',
    RuleRegistry.FindById(cCopyWithZeroIndexId) <> nil);
  AssertFalse('CopyWithZeroIndex ships disabled',
    RuleRegistry.FindById(cCopyWithZeroIndexId).Metadata.DefaultEnabled);
  AssertTrue('PosResultComparedToZeroBased registered',
    RuleRegistry.FindById(cPosResultComparedToZeroBasedId) <> nil);
  AssertFalse('PosResultComparedToZeroBased ships disabled',
    RuleRegistry.FindById(
    cPosResultComparedToZeroBasedId).Metadata.DefaultEnabled);
  AssertTrue('ShortStringTruncation registered',
    RuleRegistry.FindById(cShortStringTruncationId) <> nil);
  AssertFalse('ShortStringTruncation ships disabled',
    RuleRegistry.FindById(cShortStringTruncationId).Metadata.DefaultEnabled);
  AssertTrue('CharComparedToString registered',
    RuleRegistry.FindById(cCharComparedToStringId) <> nil);
  AssertFalse('CharComparedToString ships disabled',
    RuleRegistry.FindById(cCharComparedToStringId).Metadata.DefaultEnabled);
  AssertTrue('RawByteStringCodePageMix registered',
    RuleRegistry.FindById(cRawByteStringCodePageMixId) <> nil);
  AssertFalse('RawByteStringCodePageMix ships disabled',
    RuleRegistry.FindById(
    cRawByteStringCodePageMixId).Metadata.DefaultEnabled);
  AssertTrue('StringConcatInLoop registered',
    RuleRegistry.FindById(cStringConcatInLoopId) <> nil);
  AssertFalse('StringConcatInLoop ships disabled',
    RuleRegistry.FindById(cStringConcatInLoopId).Metadata.DefaultEnabled);
  AssertTrue('StrToIntWithoutGuard registered',
    RuleRegistry.FindById(cStrToIntWithoutGuardId) <> nil);
  AssertFalse('StrToIntWithoutGuard ships disabled',
    RuleRegistry.FindById(cStrToIntWithoutGuardId).Metadata.DefaultEnabled);
  AssertTrue('WideStringOnNonWindows registered',
    RuleRegistry.FindById(cWideStringOnNonWindowsId) <> nil);
  AssertFalse('WideStringOnNonWindows ships disabled',
    RuleRegistry.FindById(cWideStringOnNonWindowsId).Metadata.DefaultEnabled);
  AssertTrue('SetLengthWithoutFill registered',
    RuleRegistry.FindById(cSetLengthWithoutFillId) <> nil);
  AssertFalse('SetLengthWithoutFill ships disabled',
    RuleRegistry.FindById(cSetLengthWithoutFillId).Metadata.DefaultEnabled);
end;


initialization
  RegisterTest(TRulesStringsTest);

end.
