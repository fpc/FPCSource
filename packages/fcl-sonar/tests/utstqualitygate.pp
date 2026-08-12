{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the quality-gate evaluator

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstQualityGate;

{ Quality-gate evaluation tests (TFpSonarGateThresholds.Evaluate) }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config;

type
  { Quality-gate evaluation tests. }
  TQualityGateTest = class(TTestCase)
  private
    function MakeIssues(aInfo, aMinor, aMajor, aCritical,
      aBlocker: Integer): TFpSonarIssueArray;
  published
    procedure PassesUnderThresholds;
    procedure FailsWhenSeverityExceeded;
    procedure FailsWhenTotalExceeded;
    procedure UnlimitedNeverTrips;
    procedure BoundaryEqualPasses;
    procedure DeterministicCheckOrder;
  end;


implementation

// Builds an issue array with the requested per-severity counts (in severity
// order). Severity is the only field the gate reads.
function TQualityGateTest.MakeIssues(aInfo, aMinor, aMajor, aCritical,
  aBlocker: Integer): TFpSonarIssueArray;

  procedure AddN(var aArr: TFpSonarIssueArray; aCount: Integer;
    aSeverity: TFpSonarSeverity);

  var
    i: Integer;

  begin
    for i := 1 to aCount do
      begin
        SetLength(aArr, Length(aArr) + 1);
        aArr[High(aArr)] := TFpSonarIssue.Make('SynthRule', 'synthetic.pas', 1, 1, 1, 1,
          aSeverity, itCodeSmell, cfHigh, 'rule.SynthRule.message', []);
      end;
  end;

begin
  SetLength(Result, 0);
  AddN(Result, aInfo, sevInfo);
  AddN(Result, aMinor, sevMinor);
  AddN(Result, aMajor, sevMajor);
  AddN(Result, aCritical, sevCritical);
  AddN(Result, aBlocker, sevBlocker);
end;


procedure TQualityGateTest.PassesUnderThresholds;

var
  lOutcome: TFpSonarGateOutcome;

begin
  // Only Info/Minor/Major issues; default gate (blocker/critical = 0, rest -1).
  lOutcome := TFpSonarConfig.Default.Gate.Evaluate(MakeIssues(3, 2, 4, 0, 0));
  AssertFalse('gate passes under thresholds', lOutcome.Failed);
  AssertEquals('exit 0 on pass', 0, lOutcome.ExitCode);
end;


procedure TQualityGateTest.FailsWhenSeverityExceeded;

var
  lOutcome: TFpSonarGateOutcome;

begin
  // 1 Critical with the default gate (maxCritical = 0) => fail.
  lOutcome := TFpSonarConfig.Default.Gate.Evaluate(MakeIssues(0, 0, 0, 1, 0));
  AssertTrue('gate fails on a critical', lOutcome.Failed);
  AssertEquals('exit 1 on fail', 1, lOutcome.ExitCode);
  AssertTrue('reason mentions critical',
    Pos('critical', lOutcome.Reason) > 0);
end;


procedure TQualityGateTest.FailsWhenTotalExceeded;

var
  lGate: TFpSonarGateThresholds;
  lSev: TFpSonarSeverity;
  lOutcome: TFpSonarGateOutcome;

begin
  // All per-severity axes unlimited, total capped at 5.
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    lGate.MaxPerSeverity[lSev] := -1;
  lGate.MaxTotal := 5;

  lOutcome := lGate.Evaluate(MakeIssues(0, 6, 0, 0, 0));
  AssertTrue('gate fails on total', lOutcome.Failed);
  AssertEquals('exit 1 on fail', 1, lOutcome.ExitCode);
  AssertTrue('reason mentions total', Pos('total', lOutcome.Reason) > 0);
end;


procedure TQualityGateTest.UnlimitedNeverTrips;

var
  lGate: TFpSonarGateThresholds;
  lSev: TFpSonarSeverity;
  lOutcome: TFpSonarGateOutcome;

begin
  // Everything unlimited: even many Blockers pass.
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    lGate.MaxPerSeverity[lSev] := -1;
  lGate.MaxTotal := -1;

  lOutcome := lGate.Evaluate(MakeIssues(0, 0, 0, 0, 10));
  AssertFalse('all-unlimited gate never trips', lOutcome.Failed);
  AssertEquals('exit 0', 0, lOutcome.ExitCode);
end;


procedure TQualityGateTest.BoundaryEqualPasses;

var
  lGate: TFpSonarGateThresholds;
  lSev: TFpSonarSeverity;
  lOutcome: TFpSonarGateOutcome;

begin
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    lGate.MaxPerSeverity[lSev] := -1;
  lGate.MaxPerSeverity[sevMajor] := 2;
  lGate.MaxTotal := -1;

  // count == max passes (only count > max fails).
  lOutcome := lGate.Evaluate(MakeIssues(0, 0, 2, 0, 0));
  AssertFalse('2 Major with max 2 passes', lOutcome.Failed);

  // A 3rd Major trips it.
  lOutcome := lGate.Evaluate(MakeIssues(0, 0, 3, 0, 0));
  AssertTrue('3 Major with max 2 fails', lOutcome.Failed);
end;


procedure TQualityGateTest.DeterministicCheckOrder;

var
  lGate: TFpSonarGateThresholds;
  lSev: TFpSonarSeverity;
  lOutcome: TFpSonarGateOutcome;

begin
  // Critical axis AND total both exceeded; severity order precedes total, so the
  // reason must name critical, not total.
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    lGate.MaxPerSeverity[lSev] := -1;
  lGate.MaxPerSeverity[sevCritical] := 0;
  lGate.MaxTotal := 1;

  lOutcome := lGate.Evaluate(MakeIssues(0, 0, 0, 2, 0));
  AssertTrue('gate fails', lOutcome.Failed);
  AssertTrue('reason names critical (severity before total)',
    Pos('critical', lOutcome.Reason) > 0);
  AssertEquals('reason does not name total first', 0,
    Pos('total', lOutcome.Reason));
end;


initialization
  RegisterTest(TQualityGateTest);

end.
