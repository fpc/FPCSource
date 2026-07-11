{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the CST-tier parenthesis rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesParens;

{ The CST-tier PARENTHESIS rule tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.Config, FpSonar.RuleFramework,
  FpSonar.Ingest, FpSonar.Rules.Parens, UtstFixtures;

type
  { CST-tier parenthesis-rule position + registration tests. }
  TParensRulesTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture
    // with aConfig threaded onto the engine; issues land in aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);
    // As RunRule, but the fixture source is supplied inline (one array element
    // per source line) and materialised to a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // A config carrying rules.RemoveRedundantParentheses.params.keepAroundOperators.
    function KeepAroundConfig(const aValue: string): TFpSonarConfig;
    // Fresh, separately-owned rule instances (metadata mirrors the unit's
    // self-registration, including RemoveRedundantParentheses's declared
    // keepAroundOperators param).
    function NewRemoveRedundantParentheses: TRuleBase;
    function NewParenthesizeAmbiguousNot: TRuleBase;
  published
    procedure RedundantSingleAtomAndDoubledFire;
    procedure RedundantNecessaryParensAreSilent;
    procedure RedundantKeepAroundOperatorsParsedAndInert;
    procedure AmbiguousNotFires;
    procedure AmbiguousNotCompliantSilent;
    procedure PrecedenceRankMirrorsFpcTable;
    procedure RunIsDeterministic;
    procedure ParensRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cRedundantId = 'RemoveRedundantParentheses';
  cAmbiguousNotId = 'ParenthesizeAmbiguousNot';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

  // Embedded parenthesis-rule fixtures (Approach A rollout): line i+1 == [i].

  cRedundantNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Test;',
    'var',
    '  a, b, y: Integer;',
    'begin',
    '  y := (a) + ((b));',
    '  y := Integer(a);',
    'end;',
    '',
    'end.');

  cRedundantCompliant: array[0..22] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Test;',
    'var',
    '  a, b, c, y: Integer;',
    '  ptr: Pointer;',
    '  pi: PInteger;',
    'begin',
    '  y := a * (b + c);',
    '  y := (a + b) * c;',
    '  y := a - (b - c);',
    '  y := Integer(a);',
    '  ptr := @(a);',
    '  y := (pi^);',
    'end;',
    '',
    'end.');

  cAmbiguousNotNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Test;',
    'var',
    '  a, b, x, y, r: Boolean;',
    'begin',
    '  r := not a and b;',
    '  r := not a or b;',
    '  r := not x = y;',
    'end;',
    '',
    'end.');

  cAmbiguousNotCompliant: array[0..18] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Test(aObj: TObject);',
    'var',
    '  a, b, r: Boolean;',
    'begin',
    '  r := (not a) and b;',
    '  r := not a;',
    '  if not Assigned(aObj) then',
    '    r := True;',
    'end;',
    '',
    'end.');

procedure TParensRulesTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := aConfig;
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TParensRulesTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aConfig: TFpSonarConfig;
  const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  // Materialise the inline fixture to a temp dir, run with aConfig, and delete.
  lFix := TTempFixtures.Create;
  try
    RunRule(aRule, lFix.Add(aName, aSrc), aConfig, aCollector);
  finally
    lFix.Free;
  end;
end;


function TParensRulesTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TParensRulesTest.FirstById(const aCollector: TFpSonarIssueCollector;
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


function TParensRulesTest.KeepAroundConfig(const aValue: string): TFpSonarConfig;

var
  lSetting: TFpSonarRuleSetting;
  lParam: TFpSonarRuleParam;

begin
  Result := TFpSonarConfig.Default;
  lSetting.RuleId := cRedundantId;
  lSetting.HasEnabled := False;
  lSetting.Enabled := True;
  lSetting.HasSeverity := False;
  lSetting.Severity := sevInfo;
  lParam.Key := 'keepAroundOperators';
  lParam.Kind := cpkStr;
  lParam.IntVal := 0;
  lParam.StrVal := aValue;
  lParam.BoolVal := False;
  SetLength(lParam.Targets, 0);
  SetLength(lSetting.Params, 1);
  lSetting.Params[0] := lParam;
  SetLength(Result.Rules, 1);
  Result.Rules[0] := lSetting;
end;


function TParensRulesTest.NewRemoveRedundantParentheses: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cRedundantId, rtTok, rfTokenStream, sevMinor,
    itCodeSmell, cfHigh, True, '');
  lMeta.AddParam('keepAroundOperators', rpkString);
  Result := TRuleRemoveRedundantParentheses.Create(lMeta);
end;


function TParensRulesTest.NewParenthesizeAmbiguousNot: TRuleBase;

begin
  Result := TRuleParenthesizeAmbiguousNot.Create(TRuleMetadata.Make(cAmbiguousNotId,
    rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


// Arm A (cast/call not flagged), arm B (canonical 2 issues) and exact positions.
// Fixture line 13 'y := (a) + ((b));', line 14
// 'y := Integer(a);': the '(a)' atom pair and the OUTER of '((b))' fire; the
// inner pair and the Integer(...) cast do not.
procedure TParensRulesTest.RedundantSingleAtomAndDoubledFire;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveRedundantParentheses, 'noncompliant.pas', cRedundantNoncompliant, TFpSonarConfig.Default, lc);
    AssertEquals('two redundant pairs', 2, CountById(lc, cRedundantId));
    // Arm A: the (a) opening paren at R13 C8.
    AssertEquals('arm A row', 13, lc.Issues[0].StartLine);
    AssertEquals('arm A col', 8, lc.Issues[0].StartCol);
    AssertEquals('arm A point line', 13, lc.Issues[0].EndLine);
    AssertEquals('arm A point col', 8, lc.Issues[0].EndCol);
    // Arm B: the OUTER opening paren of ((b)) at R13 C14.
    AssertEquals('arm B row', 13, lc.Issues[1].StartLine);
    AssertEquals('arm B col', 14, lc.Issues[1].StartCol);
  finally
    lc.Free;
  end;
end;


// Necessary/disambiguating parens stay silent: 'a * (b + c)',
// '(a + b) * c', 'a - (b - c)', a typecast Integer(a), @(a), and a deref '(pi^)'
// (arm A abstains on '^').
procedure TParensRulesTest.RedundantNecessaryParensAreSilent;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveRedundantParentheses, 'compliant.pas', cRedundantCompliant, TFpSonarConfig.Default, lc);
    AssertEquals('no redundant findings', 0, CountById(lc, cRedundantId));
  finally
    lc.Free;
  end;
end;


// keepAroundOperators is parsed/accepted (arm C deferred), so a config
// setting it neither errors nor changes behaviour (still 2 arm A/B findings).
procedure TParensRulesTest.RedundantKeepAroundOperatorsParsedAndInert;

var
  lc: TFpSonarIssueCollector;
  lError: string;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    // The declared param means a config setting it validates clean (no exit-2).
    AssertTrue('config validates clean',
      RuleRegistry.ValidateConfig(KeepAroundConfig('+,-'), lError));
    AssertEquals('no validation error', '', lError);
    RunRuleSrc(NewRemoveRedundantParentheses, 'noncompliant.pas', cRedundantNoncompliant, KeepAroundConfig('+,-'), lc);
    AssertEquals('behaviour unchanged', 2, CountById(lc, cRedundantId));
  finally
    lc.Free;
  end;
end;


// Fire + positions. Fixture 'not a and b' / 'not a or b' /
// 'not x = y' on lines 13/14/15: one issue at each 'not' (R.. C8).
procedure TParensRulesTest.AmbiguousNotFires;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewParenthesizeAmbiguousNot, 'noncompliant.pas', cAmbiguousNotNoncompliant, TFpSonarConfig.Default, lc);
    AssertEquals('three ambiguous nots', 3, CountById(lc, cAmbiguousNotId));
    AssertEquals('not 1 row', 13, lc.Issues[0].StartLine);
    AssertEquals('not 1 col', 8, lc.Issues[0].StartCol);
    AssertEquals('not 2 row', 14, lc.Issues[1].StartLine);
    AssertEquals('not 2 col', 8, lc.Issues[1].StartCol);
    AssertEquals('not 3 row', 15, lc.Issues[2].StartLine);
    AssertEquals('not 3 col', 8, lc.Issues[2].StartCol);
  finally
    lc.Free;
  end;
end;


// Compliant nots stay silent: '(not a) and b' (parenthesised),
// 'not a' (no following operator), 'not Assigned(aObj)' (call operand).
procedure TParensRulesTest.AmbiguousNotCompliantSilent;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewParenthesizeAmbiguousNot, 'compliant.pas', cAmbiguousNotCompliant, TFpSonarConfig.Default, lc);
    AssertEquals('no ambiguous-not findings', 0, CountById(lc, cAmbiguousNotId));
  finally
    lc.Free;
  end;
end;


// Locks the FPC operator-precedence table PrecedenceRank exposes (the heart of
// ParenHelper): 1 unary, 2 multiplying, 3 adding, 4 relational, 0 non-operator.
// Scans the ParenthesizeAmbiguousNot fixture (not/and/or/=) plus the
// RemoveRedundantParentheses compliant fixture (* + -).
procedure TParensRulesTest.PrecedenceRankMirrorsFpcTable;

var
  lScanner: TFpSonarScanner;
  lFix: TTempFixtures;
  lToks: TFpSonarTokenArray;
  i: Integer;
  lWord, lPunct: string;
  lSawNot, lSawAnd, lSawOr, lSawEq, lSawStar, lSawPlus, lSawIdent: Boolean;

begin
  lSawNot := False; lSawAnd := False; lSawOr := False; lSawEq := False;
  lSawStar := False; lSawPlus := False; lSawIdent := False;
  lFix := TTempFixtures.Create;
  lScanner := TFpSonarScanner.Create;
  try
    lToks := lScanner.ScanFile(
      lFix.Add('ambiguousnot.pas', cAmbiguousNotNoncompliant), cMode, cDefines);
    for i := 0 to High(lToks) do
      begin
        if lToks[i].IsKeyword then
          begin
            lWord := LowerCase(lToks[i].Text);
            if lWord = 'not' then
              begin AssertEquals('not=1', 1, PrecedenceRank(lToks[i])); lSawNot := True; end
            else if lWord = 'and' then
              begin AssertEquals('and=2', 2, PrecedenceRank(lToks[i])); lSawAnd := True; end
            else if lWord = 'or' then
              begin AssertEquals('or=3', 3, PrecedenceRank(lToks[i])); lSawOr := True; end;
          end
        else
          begin
            lPunct := lToks[i].Punct;
            if lPunct = '=' then
              begin AssertEquals('relational ==4', 4, PrecedenceRank(lToks[i])); lSawEq := True; end
            else if (lPunct = '') and (lToks[i].Text <> '')
              and not lToks[i].IsNumber and not lToks[i].IsString then
              begin AssertEquals('identifier=0', 0, PrecedenceRank(lToks[i])); lSawIdent := True; end;
          end;
      end;

    lToks := lScanner.ScanFile(
      lFix.Add('redundant.pas', cRedundantCompliant), cMode, cDefines);
    for i := 0 to High(lToks) do
      begin
        lPunct := lToks[i].Punct;
        if lPunct = '*' then
          begin AssertEquals('multiplying=2', 2, PrecedenceRank(lToks[i])); lSawStar := True; end
        else if lPunct = '+' then
          begin AssertEquals('adding=3', 3, PrecedenceRank(lToks[i])); lSawPlus := True; end;
      end;
  finally
    lScanner.Free;
    lFix.Free;
  end;
  AssertTrue('saw not', lSawNot);
  AssertTrue('saw and', lSawAnd);
  AssertTrue('saw or', lSawOr);
  AssertTrue('saw relational', lSawEq);
  AssertTrue('saw star', lSawStar);
  AssertTrue('saw plus', lSawPlus);
  AssertTrue('saw identifier', lSawIdent);
end;


// Repeated runs over the same fixture give identical findings.
procedure TParensRulesTest.RunIsDeterministic;

var
  lA, lB: TFpSonarIssueCollector;

begin
  lA := TFpSonarIssueCollector.Create;
  lB := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveRedundantParentheses, 'noncompliant.pas', cRedundantNoncompliant, TFpSonarConfig.Default, lA);
    RunRuleSrc(NewRemoveRedundantParentheses, 'noncompliant.pas', cRedundantNoncompliant, TFpSonarConfig.Default, lB);
    AssertEquals('same count', CountById(lA, cRedundantId),
      CountById(lB, cRedundantId));
    AssertEquals('same first row',
      lA.Issues[FirstById(lA, cRedundantId)].StartLine,
      lB.Issues[FirstById(lB, cRedundantId)].StartLine);
    AssertEquals('same first col',
      lA.Issues[FirstById(lA, cRedundantId)].StartCol,
      lB.Issues[FirstById(lB, cRedundantId)].StartCol);
  finally
    lA.Free;
    lB.Free;
  end;
end;


// Both rules self-register into the global registry (convention: assert
// via FindById, not a brittle total count).
procedure TParensRulesTest.ParensRulesSelfRegisterGlobally;

begin
  AssertTrue('RemoveRedundantParentheses registered',
    RuleRegistry.FindById(cRedundantId) <> nil);
  AssertTrue('ParenthesizeAmbiguousNot registered',
    RuleRegistry.FindById(cAmbiguousNotId) <> nil);
end;


initialization
  RegisterTest(TParensRulesTest);

end.
