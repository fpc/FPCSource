{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    SARIF 2.1.0 output adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Output.Sarif;

{ The SARIF 2.1.0 output adapter (--format sarif) the SonarQube-importable format }

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, FpJson.Data,
{$ELSE}
  SysUtils, fpjson,
{$ENDIF}
  FpSonar.Types, FpSonar.Output.Utils;

const
  // The SARIF 2.1.0 schema URI and the SARIF version string.
  FpSonarSarifSchema = 'https://json.schemastore.org/sarif-2.1.0-rtm.5.json';
  FpSonarSarifVersion = '2.1.0';

// Renders the issues as a valid SARIF 2.1.0 report (UTF-8 JSON string).
function FormatSarif(const aIssues: TFpSonarIssueArray): string;


implementation

// Maps a FpSonar severity to a SARIF result level.
function SarifLevel(aSeverity: TFpSonarSeverity): string;
begin
  case aSeverity of
    sevInfo: Result := 'note';
    sevMinor, sevMajor: Result := 'warning';
    sevCritical, sevBlocker: Result := 'error';
    else
      Result := 'warning';
  end;
end;


// Builds the SARIF physicalLocation object for one issue. Caller owns the returned object!
function BuildPhysicalLocation(const aIssue: TFpSonarIssue): TJSONObject;
var
  lArtifact: TJSONObject;
  lRegion: TJSONObject;
begin
  Result := TJSONObject.Create;
  lArtifact := TJSONObject.Create;
  // Path as the engine recorded it (no URI-encoding).
  lArtifact.Add('uri', aIssue.FileName);
  Result.Add('artifactLocation', lArtifact);

  // Region only when a position is present (StartLine > 0).
  if aIssue.StartLine > 0 then
  begin
    lRegion := TJSONObject.Create;
    lRegion.Add('startLine', aIssue.StartLine);
    if aIssue.StartCol > 0 then
      lRegion.Add('startColumn', aIssue.StartCol);
    if aIssue.EndLine > 0 then
      lRegion.Add('endLine', aIssue.EndLine);
    // SARIF endColumn is EXCLUSIVE; our EndCol is INCLUSIVE -> + 1.
    if aIssue.EndCol > 0 then
      lRegion.Add('endColumn', aIssue.EndCol + 1);
    Result.Add('region', lRegion);
  end;
end;


// Builds one SARIF result object for one issue (keys in fixed order).
function BuildResult(const aIssue: TFpSonarIssue): TJSONObject;
var
  lMessage: TJSONObject;
  lLocations: TJSONArray;
  lLocation: TJSONObject;
  lProperties: TJSONObject;
  lFingerprints: TJSONObject;
  lSuppressions: TJSONArray;
  lSuppression: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('ruleId', aIssue.RuleId);                 // bare PascalCase
  Result.Add('level', SarifLevel(aIssue.Severity));

  lMessage := TJSONObject.Create;
  lMessage.Add('text', FormatMessage(aIssue.MessageKey, aIssue.MessageArgs));
  Result.Add('message', lMessage);

  lLocations := TJSONArray.Create;
  lLocation := TJSONObject.Create;
  lLocation.Add('physicalLocation', BuildPhysicalLocation(aIssue));
  lLocations.Add(lLocation);
  Result.Add('locations', lLocations);

  lProperties := TJSONObject.Create;
  lProperties.Add('fpSonarSeverity', SeverityName(aIssue.Severity, False)); // fidelity
  Result.Add('properties', lProperties);

  lFingerprints := TJSONObject.Create;
  lFingerprints.Add('fpSonarFingerprint/v1', aIssue.Fingerprint);
  Result.Add('partialFingerprints', lFingerprints);

  // A muted issue carries the canonical SARIF suppression representation
  if aIssue.SuppressionSource <> ssActive then
  begin
    lSuppressions := TJSONArray.Create;
    lSuppression := TJSONObject.Create;
    lSuppression.Add('kind', 'external');
    lSuppression.Add('justification', SuppressionName(aIssue.SuppressionSource));
    lSuppressions.Add(lSuppression);
    Result.Add('suppressions', lSuppressions);
  end;
end;


function FormatSarif(const aIssues: TFpSonarIssueArray): string;
var
  lRoot: TJSONObject;
  lRuns: TJSONArray;
  lRun: TJSONObject;
  lTool: TJSONObject;
  lDriver: TJSONObject;
  lResults: TJSONArray;
  i: integer;
begin
  lRoot := TJSONObject.Create;
  try
    lRoot.Add('$schema', FpSonarSarifSchema);
    lRoot.Add('version', FpSonarSarifVersion);

    lRuns := TJSONArray.Create;
    lRun := TJSONObject.Create;

    lTool := TJSONObject.Create;
    lDriver := TJSONObject.Create;
    lDriver.Add('name', 'FpSonar');               // tool identity once
    lDriver.Add('version', FpSonarVersion);       // version stamp
    // driver.rules[] deliberately omitted.
    lTool.Add('driver', lDriver);
    lRun.Add('tool', lTool);

    lResults := TJSONArray.Create;
    // One result per issue, IN ARRAY ORDER — no re-sort.
    for i := 0 to High(aIssues) do
      lResults.Add(BuildResult(aIssues[i]));
    lRun.Add('results', lResults);

    lRuns.Add(lRun);
    lRoot.Add('runs', lRuns);

    Result := lRoot.FormatJSON;
  finally
    lRoot.Free;
  end;
end;


end.
