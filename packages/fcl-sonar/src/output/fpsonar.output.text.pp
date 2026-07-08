{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Human-readable text output adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Output.Text;

{ The human-readable text output adapter (default --format).

  Ouptut: a version-stamped header; one line per issue
  "<path>:<line>:<col>: <severity> <RuleId>: <message>";
  a "<N> issue(s)."  footer.
  Positions are 1-based (0 = absent -> the bare path, no :line:col).
}

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  FpSonar.Types, FpSonar.Output.Utils;

const
  // The text-format output version stamp for text reports.
  FpSonarTextReportVersion = '1';

// Renders the issues as one UTF-8, LineEnding-joined human-readable report:
// a version header, one line per issue in array order, and a count footer.
function FormatText(const aIssues: TFpSonarIssueArray): string;


implementation

// Builds the 'path[:line[:col]]' position prefix for one issue
function PositionPrefix(const aIssue: TFpSonarIssue): string;
begin
  Result := aIssue.FileName;
  if aIssue.StartLine > 0 then
  begin
    Result := Result + ':' + IntToStr(aIssue.StartLine);
    if aIssue.StartCol > 0 then
      Result := Result + ':' + IntToStr(aIssue.StartCol);
  end;
end;


function FormatText(const aIssues: TFpSonarIssueArray): string;
var
  i, lSuppressed: integer;
  lIssue: TFpSonarIssue;
begin
  // 1. Version-stamped header.
  Result := 'Fp-Sonar ' + FpSonarVersion + ' - text report v' +
    FpSonarTextReportVersion + LineEnding;

  // 2. One line per issue, in array order — NO re-sort.
  lSuppressed := 0;
  for i := 0 to High(aIssues) do
  begin
    lIssue := aIssues[i];
    Result := Result + PositionPrefix(lIssue) + ': ' +
      SeverityName(lIssue.Severity, False) + ' ' + lIssue.RuleId + ': ' +
      FormatMessage(lIssue.MessageKey, lIssue.MessageArgs);
    if lIssue.SuppressionSource <> ssActive then
    begin
      Result := Result + ' [suppressed: ' +
        SuppressionName(lIssue.SuppressionSource) + ']';
      Inc(lSuppressed);
    end;
    Result := Result + LineEnding;
  end;

  // 3. Count footer. The'(M suppressed)' clause appears ONLY when something is muted
  Result := Result + IntToStr(Length(aIssues)) + ' issue(s)';
  if lSuppressed > 0 then
    Result := Result + ' (' + IntToStr(lSuppressed) + ' suppressed)';
  Result := Result + '.';
end;


end.
