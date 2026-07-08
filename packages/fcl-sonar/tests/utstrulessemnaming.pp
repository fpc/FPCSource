{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the semantic-naming (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesSemNaming;

{ The resolver-backed (SEM) semantic-naming rule tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.SemNaming, UtstFixtures;

type
  { SEM-tier semantic-naming rule position + registration tests. }
  TRulesSemNamingTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts the issue at index aK is at aLine, column 1, with key
    // rule.<aId>.message and MessageArgs deep-equal to aArgs.
    procedure AssertIssueAt(const aCollector: TFpSonarIssueCollector; aK: Integer;
      const aId: string; aLine: Integer; const aArgs: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the unit's
    // self-registration; empty key defaults to rule.<RuleId>.message).
    function NewConsistentNameCasing: TRuleBase;
    function NewDescendantNamingConvention: TRuleBase;
  published
    procedure ConsistentNameCasingPositions;
    procedure DescendantNamingConventionPositions;
    procedure SemNamingRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cConsistentNameCasingId = 'ConsistentNameCasing';
  cDescendantNamingConventionId = 'DescendantNamingConvention';

  // Embedded SEM naming-rule fixtures (Approach A rollout): line i+1 == [i].

  cConsistentNameCasingNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  MyValue: Integer;',
    'begin',
    '  myvalue := 1; // ConsistentNameCasing  (use-site casing differs from decl ''MyValue'')',
    'end;',
    'end.');

  cConsistentNameCasingCompliant: array[0..14] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  MyValue: Integer;',
    '  s: string;',
    'begin',
    '  MyValue := MyValue + 1; // casing matches the declaration ''MyValue'' -> silent',
    '  s := IntToStr(MyValue);  // resolvable library ref ''IntToStr'' at correct casing -> silent',
    'end;',
    'end.');

  cDescendantNamingConventionNoncompliant: array[0..9] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  ParseError = class(Exception); // DescendantNamingConvention  (descends Exception, fails ^E)',
    '  Foo = interface // DescendantNamingConvention  (descends IInterface, fails ^I)',
    '  end;',
    'implementation',
    'end.');

  cDescendantNamingConventionCompliant: array[0..11] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  EParseError = class(Exception); // matches ^E for Exception descendants -> silent',
    '  IFoo = interface // matches ^I for interfaces -> silent',
    '  end;',
    '  TFoo = class(TObject) // descends only TObject (the ^T convention is OFF by default) -> silent',
    '  end;',
    'implementation',
    'end.');

procedure TRulesSemNamingTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesSemNamingTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesSemNamingTest.FirstById(
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


procedure TRulesSemNamingTest.AssertIssueAt(
  const aCollector: TFpSonarIssueCollector; aK: Integer; const aId: string;
  aLine: Integer; const aArgs: array of string);

var
  j: Integer;

begin
  AssertEquals('start line', aLine, aCollector.Issues[aK].StartLine);
  AssertEquals('start col', 1, aCollector.Issues[aK].StartCol);
  AssertEquals('end line', aLine, aCollector.Issues[aK].EndLine);
  AssertEquals('end col', 1, aCollector.Issues[aK].EndCol);
  AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
    aCollector.Issues[aK].MessageKey);
  AssertEquals('arg count', Length(aArgs),
    Length(aCollector.Issues[aK].MessageArgs));
  for j := 0 to High(aArgs) do
    AssertEquals('arg ' + IntToStr(j), aArgs[j],
      aCollector.Issues[aK].MessageArgs[j]);
end;


function TRulesSemNamingTest.NewConsistentNameCasing: TRuleBase;

begin
  Result := TRuleConsistentNameCasing.Create(TRuleMetadata.Make(
    cConsistentNameCasingId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesSemNamingTest.NewDescendantNamingConvention: TRuleBase;

begin
  Result := TRuleDescendantNamingConvention.Create(TRuleMetadata.Make(
    cDescendantNamingConventionId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


procedure TRulesSemNamingTest.ConsistentNameCasingPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: the 'myvalue' use-site (row 10) diverges from the declaration
    // 'MyValue', so the args are [used, canonical] = ['myvalue', 'MyValue'].
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConsistentNameCasing,
        lFix.Add('noncompliant.pas', cConsistentNameCasingNoncompliant), lc);
      AssertEquals('one issue', 1, CountById(lc, cConsistentNameCasingId));
      AssertIssueAt(lc, FirstById(lc, cConsistentNameCasingId),
        cConsistentNameCasingId, 10, ['myvalue', 'MyValue']);
    finally
      lc.Free;
    end;

    // Compliant: a casing-matching reference stays silent (and the fixture MUST
    // resolve clean — the silent-skip canary).
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConsistentNameCasing,
        lFix.Add('compliant.pas', cConsistentNameCasingCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, cConsistentNameCasingId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesSemNamingTest.DescendantNamingConventionPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: a class 'ParseError' (row 6) descending Exception fails ^E, and
    // an interface 'Foo' (row 7) descending IInterface fails ^I — two issues in
    // declaration order, each with [typeName, pattern].
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDescendantNamingConvention,
        lFix.Add('noncompliant.pas', cDescendantNamingConventionNoncompliant), lc);
      AssertEquals('two issues', 2, CountById(lc, cDescendantNamingConventionId));
      k := FirstById(lc, cDescendantNamingConventionId);
      AssertIssueAt(lc, k, cDescendantNamingConventionId, 6, ['ParseError', '^E']);
      AssertIssueAt(lc, k + 1, cDescendantNamingConventionId, 7, ['Foo', '^I']);
    finally
      lc.Free;
    end;

    // Compliant: EParseError matches ^E, IFoo matches ^I, and TFoo descends only
    // TObject (the ^T convention is off by default) — all silent. The fixture MUST
    // resolve clean (the silent-skip canary).
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDescendantNamingConvention,
        lFix.Add('compliant.pas', cDescendantNamingConventionCompliant), lc);
      AssertEquals('compliant => zero', 0,
        CountById(lc, cDescendantNamingConventionId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesSemNamingTest.SemNamingRulesSelfRegisterGlobally;

begin
  // The production initialization registered both SEM naming rules into the GLOBAL
  // registry (this is what the CLI process runs).
  AssertTrue('ConsistentNameCasing registered',
    RuleRegistry.FindById(cConsistentNameCasingId) <> nil);
  AssertTrue('DescendantNamingConvention registered',
    RuleRegistry.FindById(cDescendantNamingConventionId) <> nil);
end;


initialization
  RegisterTest(TRulesSemNamingTest);

end.
