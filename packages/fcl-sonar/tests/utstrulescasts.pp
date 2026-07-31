{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the type-cast (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesCasts;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Casts, UtstFixtures;

type
  { SEM-tier cast-rule position + registration tests. }
  TRulesCastsTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires once at aDeclLine, column 1, with key
    // rule.<aId>.message and message args aArgs, and zero on the compliant
    // fixture.
    procedure CheckCastRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Asserts aRule fires twice at aLine1 then aLine2, each with the single arg
    // aArg, and zero on the compliant fixture.
    procedure CheckTwoIssueRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aLine1, aLine2: Integer; const aArg: string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewCharToCharPointerCast: TRuleBase;
    function NewObjectCastNotInHierarchy: TRuleBase;
    function NewRedundantCast: TRuleBase;
    function NewObjectCastBeforeFree: TRuleBase;
    function NewUnicodeToAnsiCast: TRuleBase;
    function NewPlatformDependentCast: TRuleBase;
    function NewPlatformDependentTruncation: TRuleBase;
  published
    procedure CharToCharPointerCastPositions;
    procedure ObjectCastNotInHierarchyPositions;
    procedure RedundantCastPositions;
    procedure ObjectCastBeforeFreePositions;
    procedure UnicodeToAnsiCastPositions;
    procedure PlatformDependentCastPositions;
    procedure PlatformDependentTruncationPositions;
    procedure CastRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cCharToCharPointerCastId = 'CharToCharPointerCast';
  cObjectCastNotInHierarchyId = 'ObjectCastNotInHierarchy';
  cRedundantCastId = 'RedundantCast';
  cObjectCastBeforeFreeId = 'ObjectCastBeforeFree';
  cUnicodeToAnsiCastId = 'UnicodeToAnsiCast';
  cPlatformDependentCastId = 'PlatformDependentCast';
  cPlatformDependentTruncationId = 'PlatformDependentTruncation';

  // Embedded cast-rule fixtures: line i+1 == [i].

  cCharToCharPointerCastNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseChar;',
    'var',
    '  c: Char;',
    '  p: PChar;',
    'begin',
    '  p := PChar(c);',
    'end;',
    'end.');

  cCharToCharPointerCastCompliant: array[0..13] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseChar;',
    'var',
    '  c: Char;',
    '  s: string;',
    '  p: PChar;',
    'begin',
    '  p := PChar(s);',
    '  p := PChar(@c);',
    'end;',
    'end.');

  cObjectCastNotInHierarchyNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TCar = class(TObject)',
    '  end;',
    '  TDog = class(TObject)',
    '  end;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  c: TCar;',
    '  d: TDog;',
    'begin',
    '  d := TDog(c);',
    'end;',
    'end.');

  cObjectCastNotInHierarchyCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBase = class(TObject)',
    '  end;',
    '  TDerived = class(TBase)',
    '  end;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  b: TBase;',
    '  d: TDerived;',
    'begin',
    '  if b is TDerived then',
    '    d := TDerived(b);',
    'end;',
    'end.');

  cRedundantCastNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseIt;',
    'var',
    '  i: Integer;',
    '  j: Integer;',
    'begin',
    '  j := Integer(i);',
    'end;',
    'end.');

  cRedundantCastCompliant: array[0..22] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TMyInt = Integer;',
    '  TBase = class(TObject)',
    '  end;',
    '  TDerived = class(TBase)',
    '  end;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  i: Integer;',
    '  b: Byte;',
    '  a: TMyInt;',
    '  base: TBase;',
    '  d: TDerived;',
    'begin',
    '  i := Integer(b);',
    '  a := TMyInt(i);',
    '  d := TDerived(base);',
    'end;',
    'end.');

  cObjectCastBeforeFreeNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  TFoo = class(TObject)',
    '  end;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  obj: TObject;',
    'begin',
    '  TFoo(obj).Free;',
    '  FreeAndNil(TFoo(obj));',
    'end;',
    'end.');

  cObjectCastBeforeFreeCompliant: array[0..20] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  TFoo = class(TObject)',
    '    procedure Done;',
    '  end;',
    'implementation',
    'procedure TFoo.Done;',
    'begin',
    '  inherited Free;',
    'end;',
    'procedure UseIt;',
    'var',
    '  obj: TObject;',
    'begin',
    '  obj.Free;',
    '  FreeAndNil(obj);',
    'end;',
    'end.');

  cUnicodeToAnsiCastNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseIt;',
    'var',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    'begin',
    '  a := AnsiString(u);',
    'end;',
    'end.');

  cUnicodeToAnsiCastCompliant: array[0..15] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  u: UnicodeString;',
    '  a: AnsiString;',
    '  w: WideString;',
    'begin',
    '  u := UnicodeString(a);',
    '  w := WideString(u);',
    '  a := UTF8Encode(u);',
    'end;',
    'end.');

  cPlatformDependentCastNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseIt;',
    'var',
    '  p: Pointer;',
    '  n: Integer;',
    'begin',
    '  n := Integer(p);',
    'end;',
    'end.');

  cPlatformDependentCastCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseIt;',
    'var',
    '  p: Pointer;',
    '  n: PtrInt;',
    '  m: Int64;',
    '  j: Integer;',
    '  b: Byte;',
    'begin',
    '  n := PtrInt(p);',
    '  m := Int64(p);',
    '  j := Integer(b);',
    'end;',
    'end.');

  cPlatformDependentTruncationNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseIt;',
    'var',
    '  p: PtrInt;',
    '  w: Int64;',
    '  n: Integer;',
    '  m: Integer;',
    'begin',
    '  n := Integer(p);',
    '  m := Integer(w);',
    'end;',
    'end.');

  cPlatformDependentTruncationCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'procedure UseIt;',
    'var',
    '  w: Int64;',
    '  i: Integer;',
    '  b: Byte;',
    '  m: Int64;',
    '  p: PtrInt;',
    '  n: Integer;',
    'begin',
    '  m := Int64(w);',
    '  p := PtrInt(i);',
    '  n := Integer(b);',
    'end;',
    'end.');

procedure TRulesCastsTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Analyze(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
      aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesCastsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesCastsTest.FirstById(
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


function TRulesCastsTest.NewCharToCharPointerCast: TRuleBase;

begin
  Result := TRuleCharToCharPointerCast.Create(TRuleMetadata.Make(
    cCharToCharPointerCastId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCastsTest.NewObjectCastNotInHierarchy: TRuleBase;

begin
  Result := TRuleObjectCastNotInHierarchy.Create(TRuleMetadata.Make(
    cObjectCastNotInHierarchyId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCastsTest.NewRedundantCast: TRuleBase;

begin
  Result := TRuleRedundantCast.Create(TRuleMetadata.Make(
    cRedundantCastId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCastsTest.NewObjectCastBeforeFree: TRuleBase;

begin
  Result := TRuleObjectCastBeforeFree.Create(TRuleMetadata.Make(
    cObjectCastBeforeFreeId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCastsTest.NewUnicodeToAnsiCast: TRuleBase;

begin
  Result := TRuleUnicodeToAnsiCast.Create(TRuleMetadata.Make(
    cUnicodeToAnsiCastId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCastsTest.NewPlatformDependentCast: TRuleBase;

begin
  Result := TRulePlatformDependentCast.Create(TRuleMetadata.Make(
    cPlatformDependentCastId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCastsTest.NewPlatformDependentTruncation: TRuleBase;

begin
  Result := TRulePlatformDependentTruncation.Create(TRuleMetadata.Make(
    cPlatformDependentTruncationId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


procedure TRulesCastsTest.CheckCastRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: one issue at the cast line, column 1, carrying aArgs as the
    // message args.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
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

    // Compliant: the FP guards stay silent; the fixture must resolve clean.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesCastsTest.CheckTwoIssueRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aLine1, aLine2: Integer; const aArg: string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: two issues in statement-then-expression order, each at its
    // cast line, column 1, with arg [aArg].
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('two issues for ' + aId, 2, CountById(lc, aId));
      // Collected in statement-then-expression order.
      k := FirstById(lc, aId);
      AssertEquals('first cast line', aLine1, lc.Issues[k].StartLine);
      AssertEquals('first start col', 1, lc.Issues[k].StartCol);
      AssertEquals('first key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('first arg count', 1, Length(lc.Issues[k].MessageArgs));
      AssertEquals('first arg', aArg, lc.Issues[k].MessageArgs[0]);
      // The second issue is the other offending cast (next index of this rule).
      AssertEquals('second cast line', aLine2, lc.Issues[k + 1].StartLine);
      AssertEquals('second start col', 1, lc.Issues[k + 1].StartCol);
      AssertEquals('second arg', aArg, lc.Issues[k + 1].MessageArgs[0]);
    finally
      lc.Free;
    end;

    // Compliant: the FP guards stay silent; the fixture must resolve clean.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesCastsTest.CharToCharPointerCastPositions;

begin
  // Noncompliant: 'p := PChar(c);' (cast line 10, probe-locked); arg is the
  // target pointer-type name.
  CheckCastRuleSrc(NewCharToCharPointerCast, NewCharToCharPointerCast,
    cCharToCharPointerCastId, 10, ['PChar'],
    cCharToCharPointerCastNoncompliant, cCharToCharPointerCastCompliant);
end;


procedure TRulesCastsTest.ObjectCastNotInHierarchyPositions;

begin
  // Noncompliant: 'd := TDog(c);' between sibling classes (cast line 15,
  // probe-locked); args are the source then target class names.
  CheckCastRuleSrc(NewObjectCastNotInHierarchy, NewObjectCastNotInHierarchy,
    cObjectCastNotInHierarchyId, 15, ['TCar', 'TDog'],
    cObjectCastNotInHierarchyNoncompliant, cObjectCastNotInHierarchyCompliant);
end;


procedure TRulesCastsTest.RedundantCastPositions;

begin
  // Noncompliant: 'j := Integer(i);' where i:Integer (cast line 10,
  // probe-locked); arg is the target type name.
  CheckCastRuleSrc(NewRedundantCast, NewRedundantCast, cRedundantCastId, 10,
    ['Integer'],
    cRedundantCastNoncompliant, cRedundantCastCompliant);
end;


procedure TRulesCastsTest.ObjectCastBeforeFreePositions;

begin
  // Noncompliant: 'TFoo(obj).Free;' (line 13) and 'FreeAndNil(TFoo(obj));'
  // (line 14), both probe-locked; arg is the target class name.
  CheckTwoIssueRuleSrc(NewObjectCastBeforeFree, NewObjectCastBeforeFree,
    cObjectCastBeforeFreeId, 13, 14, 'TFoo',
    cObjectCastBeforeFreeNoncompliant, cObjectCastBeforeFreeCompliant);
end;


procedure TRulesCastsTest.UnicodeToAnsiCastPositions;

begin
  // Noncompliant: 'a := AnsiString(u);' where u:UnicodeString (cast line 10,
  // probe-locked); arg is the target ANSI type name.
  CheckCastRuleSrc(NewUnicodeToAnsiCast, NewUnicodeToAnsiCast,
    cUnicodeToAnsiCastId, 10, ['AnsiString'],
    cUnicodeToAnsiCastNoncompliant, cUnicodeToAnsiCastCompliant);
end;


procedure TRulesCastsTest.PlatformDependentCastPositions;

begin
  // Noncompliant: 'n := Integer(p);' where p:Pointer (cast line 10,
  // probe-locked); arg is the fixed-width integer side's name.
  CheckCastRuleSrc(NewPlatformDependentCast, NewPlatformDependentCast,
    cPlatformDependentCastId, 10, ['Integer'],
    cPlatformDependentCastNoncompliant, cPlatformDependentCastCompliant);
end;


procedure TRulesCastsTest.PlatformDependentTruncationPositions;

begin
  // Noncompliant covers both wide-source arms: 'n := Integer(p);' where p:PtrInt
  // (cast line 12) and 'm := Integer(w);' where w:Int64 (cast line 13); arg is
  // the narrow (target) integer side's name.
  CheckTwoIssueRuleSrc(NewPlatformDependentTruncation, NewPlatformDependentTruncation,
    cPlatformDependentTruncationId, 12, 13, 'Integer',
    cPlatformDependentTruncationNoncompliant, cPlatformDependentTruncationCompliant);
end;


procedure TRulesCastsTest.CastRulesSelfRegisterGlobally;

begin
  // The production initialization registered all seven SEM cast rules into the
  // global registry.
  AssertTrue('CharToCharPointerCast registered',
    RuleRegistry.FindById(cCharToCharPointerCastId) <> nil);
  AssertTrue('ObjectCastNotInHierarchy registered',
    RuleRegistry.FindById(cObjectCastNotInHierarchyId) <> nil);
  AssertTrue('RedundantCast registered',
    RuleRegistry.FindById(cRedundantCastId) <> nil);
  AssertTrue('ObjectCastBeforeFree registered',
    RuleRegistry.FindById(cObjectCastBeforeFreeId) <> nil);
  AssertTrue('UnicodeToAnsiCast registered',
    RuleRegistry.FindById(cUnicodeToAnsiCastId) <> nil);
  AssertTrue('PlatformDependentCast registered',
    RuleRegistry.FindById(cPlatformDependentCastId) <> nil);
  AssertTrue('PlatformDependentTruncation registered',
    RuleRegistry.FindById(cPlatformDependentTruncationId) <> nil);
end;


initialization
  RegisterTest(TRulesCastsTest);

end.
