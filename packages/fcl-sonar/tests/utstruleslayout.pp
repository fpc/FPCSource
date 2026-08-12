{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the LEX-tier layout rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesLayout;

{ The 5 LEX layout rules + the line-text feed. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Layout, UtstFixtures;

type
  { Layout-rule position + registration tests. }
  TRulesLayoutTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    // As RunRule, but the fixture source is supplied inline (one array element
    // per source line) and materialised to a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewTrailing: TRuleBase;
    function NewTabs: TRuleBase;
    function NewLineLong: TRuleBase;
    function NewNumUnderscores: TRuleBase;
    function NewGrouping: TRuleBase;
  published
    procedure NoTrailingWhitespacePositions;
    procedure NoTabsPositions;
    procedure LineTooLongPositions;
    procedure LongNumericLiteralUnderscoresPositions;
    procedure DigitGroupingStandardPositions;
    procedure RulesSelfRegisterGlobally;
    procedure RunIsDeterministic;
  end;


implementation

const
  cMode = 'OBJFPC';
  cTrailingId = 'NoTrailingWhitespace';
  cTabsId = 'NoTabs';
  cLineLongId = 'LineTooLong';
  cNumId = 'LongNumericLiteralUnderscores';
  cGroupingId = 'DigitGroupingStandard';

  // Embedded layout-rule fixtures (Approach A rollout): line i+1 == [i].
  // Trailing spaces (NoTrailingWhitespace) and a leading tab (NoTabs) are
  // preserved verbatim INSIDE the string literals.

  cTrailingNoncompliant: array[0..8] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface   ',
    '',
    'implementation',
    '',
    'end.');

  cTrailingCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    '',
    'end.');

  cTabsNoncompliant: array[0..11] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'const',
    '	cX = 1;',
    '',
    'end.');

  cTabsCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Foo;',
    '',
    'implementation',
    '',
    'procedure Foo;',
    'begin',
    '  Foo;',
    'end;',
    '',
    'end.');

  cLineLongNoncompliant: array[0..10] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    '',
    'implementation',
    '',
    'end.');

  cLineLongCompliant: array[0..8] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cNumNoncompliant: array[0..11] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cBig = 1000000;',
    '',
    'implementation',
    '',
    'end.');

  cNumCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cBig = 1_000_000;',
    '',
    'implementation',
    '',
    'end.');

  cGroupingNoncompliant: array[0..11] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cBig = 10_00_00;',
    '',
    'implementation',
    '',
    'end.');

  cGroupingCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cBig = 1_000_000;',
    '',
    'implementation',
    '',
    'end.');

procedure TRulesLayoutTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


procedure TRulesLayoutTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  // Materialise the inline fixture (one array element per source line, in-line
  // tabs / trailing spaces preserved) to a temp dir, run, and delete.
  lFix := TTempFixtures.Create;
  try
    RunRule(aRule, lFix.Add(aName, aSrc), aCollector);
  finally
    lFix.Free;
  end;
end;


function TRulesLayoutTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesLayoutTest.FirstById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

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


function TRulesLayoutTest.NewTrailing: TRuleBase;

begin
  Result := TRuleNoTrailingWhitespace.Create(TRuleMetadata.Make(cTrailingId, rtLex,
    rfLineText, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesLayoutTest.NewTabs: TRuleBase;

begin
  Result := TRuleNoTabs.Create(TRuleMetadata.Make(cTabsId, rtLex, rfLineText,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesLayoutTest.NewLineLong: TRuleBase;

begin
  Result := TRuleLineTooLong.Create(TRuleMetadata.Make(cLineLongId, rtLex, rfLineText,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesLayoutTest.NewNumUnderscores: TRuleBase;

begin
  Result := TRuleLongNumericLiteralUnderscores.Create(TRuleMetadata.Make(cNumId,
    rtLex, rfTokenStream, sevInfo, itCodeSmell, cfHigh, True, ''));
end;


function TRulesLayoutTest.NewGrouping: TRuleBase;

begin
  Result := TRuleDigitGroupingStandard.Create(TRuleMetadata.Make(cGroupingId, rtLex,
    rfTokenStream, sevInfo, itCodeSmell, cfHigh, True, ''));
end;


procedure TRulesLayoutTest.NoTrailingWhitespacePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: line 5 = "interface" + 3 trailing spaces => run cols 10..12.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrailing, 'noncompliant.pas', cTrailingNoncompliant, lc);
    AssertEquals('one trailing-whitespace issue', 1, CountById(lc, cTrailingId));
    k := FirstById(lc, cTrailingId);
    AssertEquals('start line', 5, lc.Issues[k].StartLine);
    AssertEquals('start col', 10, lc.Issues[k].StartCol);
    AssertEquals('end line', 5, lc.Issues[k].EndLine);
    AssertEquals('end col', 12, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: no trailing whitespace anywhere.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrailing, 'compliant.pas', cTrailingCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cTrailingId));
  finally
    lc.Free;
  end;
end;


procedure TRulesLayoutTest.NoTabsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: line 10 indented with a leading TAB => point at (10, 1).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTabs, 'noncompliant.pas', cTabsNoncompliant, lc);
    AssertEquals('one tab issue', 1, CountById(lc, cTabsId));
    k := FirstById(lc, cTabsId);
    AssertEquals('start line', 10, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('point end line', 10, lc.Issues[k].EndLine);
    AssertEquals('point end col', 1, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: two-space indentation only, no tabs.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTabs, 'compliant.pas', cTabsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cTabsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesLayoutTest.LineTooLongPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: line 7 is 121 chars => point at column maxLength+1 = 121,
  // args [displayLen=121, maxLength=120].
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewLineLong, 'noncompliant.pas', cLineLongNoncompliant, lc);
    AssertEquals('one line-too-long issue', 1, CountById(lc, cLineLongId));
    k := FirstById(lc, cLineLongId);
    AssertEquals('start line', 7, lc.Issues[k].StartLine);
    AssertEquals('start col', 121, lc.Issues[k].StartCol);
    AssertEquals('end col', 121, lc.Issues[k].EndCol);
    AssertEquals('message resolves with display length and limit',
      'Line is 121 characters long; the limit is 120',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // Compliant: every line within the limit.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewLineLong, 'compliant.pas', cLineLongCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cLineLongId));
  finally
    lc.Free;
  end;
end;


procedure TRulesLayoutTest.LongNumericLiteralUnderscoresPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 1000000 on line 8 at col 10 => 7-digit lexeme cols 10..16.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNumUnderscores, 'noncompliant.pas', cNumNoncompliant, lc);
    AssertEquals('one long-numeric issue', 1, CountById(lc, cNumId));
    k := FirstById(lc, cNumId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 10, lc.Issues[k].StartCol);
    AssertEquals('end col (whole lexeme)', 16, lc.Issues[k].EndCol);
    AssertEquals('digit count arg',
      'Numeric literal with 7 digits should use _ separators',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // Compliant: 1_000_000 has separators => the rule does not fire.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNumUnderscores, 'compliant.pas', cNumCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNumId));
  finally
    lc.Free;
  end;
end;


procedure TRulesLayoutTest.DigitGroupingStandardPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 10_00_00 on line 8 at col 10 => irregular groups [2,2,2];
  // lexeme is 8 chars => cols 10..17.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewGrouping, 'noncompliant.pas', cGroupingNoncompliant, lc);
    AssertEquals('one grouping issue', 1, CountById(lc, cGroupingId));
    k := FirstById(lc, cGroupingId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 10, lc.Issues[k].StartCol);
    AssertEquals('end col (whole lexeme)', 17, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: 1_000_000 groups [1,3,3] are regular => no issue.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewGrouping, 'compliant.pas', cGroupingCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cGroupingId));
  finally
    lc.Free;
  end;
end;


procedure TRulesLayoutTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all 5 rules into the GLOBAL
  // registry (this is what the CLI process runs).
  AssertTrue('NoTrailingWhitespace registered',
    RuleRegistry.FindById(cTrailingId) <> nil);
  AssertTrue('NoTabs registered', RuleRegistry.FindById(cTabsId) <> nil);
  AssertTrue('LineTooLong registered',
    RuleRegistry.FindById(cLineLongId) <> nil);
  AssertTrue('LongNumericLiteralUnderscores registered',
    RuleRegistry.FindById(cNumId) <> nil);
  AssertTrue('DigitGroupingStandard registered',
    RuleRegistry.FindById(cGroupingId) <> nil);
end;


procedure TRulesLayoutTest.RunIsDeterministic;

var
  lFirst, lSecond: TFpSonarIssueCollector;
  i: Integer;

begin
  // Two runs of the same rule over the same noncompliant fixture must produce
  // identical issues (count, RuleId, positions, fingerprints).
  lFirst := TFpSonarIssueCollector.Create;
  lSecond := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewGrouping, 'noncompliant.pas', cGroupingNoncompliant, lFirst);
    RunRuleSrc(NewGrouping, 'noncompliant.pas', cGroupingNoncompliant, lSecond);

    AssertTrue('at least one issue', lFirst.Count > 0);
    AssertEquals('identical count', lFirst.Count, lSecond.Count);
    for i := 0 to lFirst.Count - 1 do
      begin
        AssertEquals('same RuleId at ' + IntToStr(i),
          lFirst.Issues[i].RuleId, lSecond.Issues[i].RuleId);
        AssertEquals('same start col at ' + IntToStr(i),
          lFirst.Issues[i].StartCol, lSecond.Issues[i].StartCol);
        AssertEquals('same fingerprint at ' + IntToStr(i),
          lFirst.Issues[i].Fingerprint, lSecond.Issues[i].Fingerprint);
        AssertTrue('fingerprinted at ' + IntToStr(i),
          lFirst.Issues[i].Fingerprint <> '');
      end;
  finally
    lFirst.Free;
    lSecond.Free;
  end;
end;


initialization
  RegisterTest(TRulesLayoutTest);

end.
