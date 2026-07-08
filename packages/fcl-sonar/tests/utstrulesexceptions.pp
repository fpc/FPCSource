{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the exception-structure (AST) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesExceptions;

{ The three AST-tier exception-structure rules }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Exceptions, UtstFixtures;

type
  { AST-tier exception-rule position + registration tests. }
  TRulesExceptionsTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires exactly once at aDeclLine, column 1, with key
    // rule.<aId>.message and message args = aArgs; and zero on the compliant
    // fixture. Fixtures supplied inline (one array element per source line) and
    // materialised to a temp dir.
    procedure CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewNoEmptyFinally: TRuleBase;
    function NewExceptionsNotSwallowed: TRuleBase;
    function NewNoExplicitReRaise: TRuleBase;
  published
    procedure NoEmptyFinallyPositions;
    procedure ExceptionsNotSwallowedPositions;
    procedure NoExplicitReRaisePositions;
    procedure RulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cNoEmptyFinallyId = 'NoEmptyFinally';
  cExceptionsNotSwallowedId = 'ExceptionsNotSwallowed';
  cNoExplicitReRaiseId = 'NoExplicitReRaise';


  // Embedded exception-rule fixtures (Approach A rollout): line i+1 == [i].

  cNoEmptyFinallyNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '  end;',
    'end;',
    '',
    'end.');

  cNoEmptyFinallyCompliant: array[0..26] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A non-empty finally => #5 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '    WriteLn(''cleanup'');',
    '  end;',
    '  // An empty except must NOT trip #5 (this rule never touches except handlers).',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '  end;',
    'end;',
    '',
    'end.');

  cExceptionsNotSwallowedNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '  end;',
    'end;',
    '',
    'end.');

  cExceptionsNotSwallowedCompliant: array[0..33] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A non-empty except => #49 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    WriteLn(''handled'');',
    '  end;',
    '  // An on-handler makes the except block non-empty => #49 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      WriteLn(''handled'');',
    '  end;',
    '  // An empty finally must NOT trip #49 (this rule never touches finally).',
    '  try',
    '    WriteLn(''work'');',
    '  finally',
    '  end;',
    'end;',
    '',
    'end.');

  cNoExplicitReRaiseNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      raise E;',
    '  end;',
    'end;',
    '',
    'end.');

  cNoExplicitReRaiseCompliant: array[0..38] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // A bare `raise;` preserves the original stack => #21 stays silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      raise;',
    '  end;',
    '  // Raising a different exception is not a re-raise of the caught var => silent.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on E: Exception do',
    '      raise EInOutError.Create(''boom'');',
    '  end;',
    '  // A no-variable `on` handler (VarEl = nil) has nothing to compare against =>',
    '  // silent. Load-bearing FP guard: without the VarEl<>nil guard the rule would',
    '  // dereference a nil catch variable.',
    '  try',
    '    WriteLn(''work'');',
    '  except',
    '    on Exception do',
    '      WriteLn(''handled'');',
    '  end;',
    'end;',
    '',
    'end.');

procedure TRulesExceptionsTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesExceptionsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesExceptionsTest.FirstById(
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


procedure TRulesExceptionsTest.CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at aDeclLine, column 1 (AST nodes carry no
    // column), carrying aArgs as the message args.
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

    // Compliant: nothing flagged for the rule under test.
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


function TRulesExceptionsTest.NewNoEmptyFinally: TRuleBase;

begin
  Result := TRuleNoEmptyFinally.Create(TRuleMetadata.Make(cNoEmptyFinallyId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewExceptionsNotSwallowed: TRuleBase;

begin
  Result := TRuleExceptionsNotSwallowed.Create(TRuleMetadata.Make(
    cExceptionsNotSwallowedId, rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesExceptionsTest.NewNoExplicitReRaise: TRuleBase;

begin
  Result := TRuleNoExplicitReRaise.Create(TRuleMetadata.Make(cNoExplicitReRaiseId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


procedure TRulesExceptionsTest.NoEmptyFinallyPositions;

begin
  // Noncompliant: the empty 'finally' (line 16, CLI-probed); no args. The
  // compliant fixture's empty except is a load-bearing FP guard that must NOT
  // trip NoEmptyFinally.
  CheckStmtRuleSrc(NewNoEmptyFinally, NewNoEmptyFinally, cNoEmptyFinallyId, 16, [],
    cNoEmptyFinallyNoncompliant, cNoEmptyFinallyCompliant);
end;


procedure TRulesExceptionsTest.ExceptionsNotSwallowedPositions;

begin
  // Noncompliant: the empty 'except' (line 16, CLI-probed); no args. The
  // compliant fixture covers a non-empty except, an on-handled except, and an
  // empty finally that must NOT trip ExceptionsNotSwallowed.
  CheckStmtRuleSrc(NewExceptionsNotSwallowed, NewExceptionsNotSwallowed,
    cExceptionsNotSwallowedId, 16, [],
    cExceptionsNotSwallowedNoncompliant, cExceptionsNotSwallowedCompliant);
end;


procedure TRulesExceptionsTest.NoExplicitReRaisePositions;

begin
  // Noncompliant: 'raise E;' inside 'on E: Exception do' (line 18, CLI-probed);
  // arg is the catch-variable name. The compliant fixture covers a bare
  // 'raise;' and a raise of a different exception (load-bearing FP guards).
  CheckStmtRuleSrc(NewNoExplicitReRaise, NewNoExplicitReRaise, cNoExplicitReRaiseId,
    18, ['E'],
    cNoExplicitReRaiseNoncompliant, cNoExplicitReRaiseCompliant);
end;


procedure TRulesExceptionsTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all three exception rules into the
  // GLOBAL registry (this is what the CLI process runs).
  AssertTrue('NoEmptyFinally registered',
    RuleRegistry.FindById(cNoEmptyFinallyId) <> nil);
  AssertTrue('ExceptionsNotSwallowed registered',
    RuleRegistry.FindById(cExceptionsNotSwallowedId) <> nil);
  AssertTrue('NoExplicitReRaise registered',
    RuleRegistry.FindById(cNoExplicitReRaiseId) <> nil);
end;


initialization
  RegisterTest(TRulesExceptionsTest);

end.
