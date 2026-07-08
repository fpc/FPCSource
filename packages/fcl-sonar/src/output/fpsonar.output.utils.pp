{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Shared output-adapter helpers: severity and suppression-source names

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Output.Utils;

{ Helpers shared by the text / SARIF / SonarQube-JSON output adapters:
  the severity-name and suppression-source-name mappings. }

{$mode objfpc}{$H+}

interface

uses
  FpSonar.Types;

// The severity name. aUppercase=True returns the SonarQube vocabulary
function SeverityName(aSeverity: TFpSonarSeverity; aUppercase: boolean): string;

// The lowercase suppression-source name (nosonar/config/baseline/active).
function SuppressionName(aSource: TFpSonarSuppressionSource): string;


implementation

function SeverityName(aSeverity: TFpSonarSeverity; aUppercase: boolean): string;
const
  cLower: array[TFpSonarSeverity] of string =
    ('info', 'minor', 'major', 'critical', 'blocker');
  cUpper: array[TFpSonarSeverity] of string =
    ('INFO', 'MINOR', 'MAJOR', 'CRITICAL', 'BLOCKER');
begin
  if aUppercase then
    Result := cUpper[aSeverity]
  else
    Result := cLower[aSeverity];
end;


function SuppressionName(aSource: TFpSonarSuppressionSource): string;
begin
  case aSource of
    ssNoSonar: Result := 'nosonar';
    ssConfig: Result := 'config';
    ssBaseline: Result := 'baseline';
    else
      Result := 'active';
  end;
end;


end.
