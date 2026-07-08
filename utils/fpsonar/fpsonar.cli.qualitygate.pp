{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    quality-treshold evaluator: thresholds to a pass/fail outcome and exit code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.CLI.QualityGate;

{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types, FpSonar.Config;

type
  // The result of evaluating the gate: whether it failed, the process exit code
  // (0 pass / 1 fail), and a human reason naming the first exceeded axis.
  TFpSonarGateOutcome = record
    Failed: Boolean;
    ExitCode: Integer;
    Reason: string;
  end;

// Evaluates aGate against aIssues' (effective) severities. 
function EvaluateGate(const aIssues: TFpSonarIssueArray;
  const aGate: TFpSonarGateThresholds): TFpSonarGateOutcome;


implementation

uses
  SysUtils, FpSonar.Consts;

function EvaluateGate(const aIssues: TFpSonarIssueArray;
  const aGate: TFpSonarGateThresholds): TFpSonarGateOutcome;

var
  lCounts: array[TFpSonarSeverity] of Integer;
  lSev: TFpSonarSeverity;
  lTotal, i: Integer;

  // True iff this axis has a limit (>= 0) and the count strictly exceeds it.
  function Trips(aCount, aMax: Integer): Boolean;

  begin
    Result := (aMax >= 0) and (aCount > aMax);
  end;

begin
  Result.Failed := False;
  Result.ExitCode := 0;
  Result.Reason := '';

  // One pass: tally per-severity counts + the total.
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    lCounts[lSev] := 0;
  for i := 0 to High(aIssues) do
    Inc(lCounts[aIssues[i].Severity]);
  lTotal := Length(aIssues);

  // Severities first, in descending severity order (Blocker..Info); the enum is
  // declared ascending, so iterate High downto Low.
  for lSev := High(TFpSonarSeverity) downto Low(TFpSonarSeverity) do
    if Trips(lCounts[lSev], aGate.MaxPerSeverity[lSev]) then
      begin
        Result.Failed := True;
        Result.ExitCode := 1;
        Result.Reason := Format(
          SGateSeverityExceeded,
          [lCounts[lSev], SeverityName(lSev), aGate.MaxPerSeverity[lSev]]);
        Exit;
      end;

  // Total last.
  if Trips(lTotal, aGate.MaxTotal) then
    begin
      Result.Failed := True;
      Result.ExitCode := 1;
      Result.Reason := Format(
        SGateTotalExceeded,
        [lTotal, aGate.MaxTotal]);
    end;
end;


end.
