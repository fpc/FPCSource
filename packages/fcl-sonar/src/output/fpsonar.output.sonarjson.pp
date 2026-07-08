{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    SonarQube generic-issue-import JSON output adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Output.SonarJson;

(* The SonarQube generic-issue-import JSON adapter (--format sonar-json).  Emits:
    {
      "issues": [ { engineId, ruleId, severity, type, primaryLocation } ],
      "_fpsonar": { "format": "sonarqube-generic", "version": <stamp> }
    }
  *)

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
  // The SonarQube-adapter output-format version stamp for sonar-json.
  FpSonarSonarJsonVersion = '1';

// Renders the issues as SonarQube generic issue import JSON (UTF-8 string).
function FormatSonarJson(const aIssues: TFpSonarIssueArray): string;


implementation

// Maps a FpSonar issue type to the SonarQube type vocabulary.
function SonarType(aType: TFpSonarIssueType): string;
begin
  case aType of
    itBug: Result := 'BUG';
    itVulnerability: Result := 'VULNERABILITY';
    itCodeSmell: Result := 'CODE_SMELL';
    else
      Result := 'CODE_SMELL';
  end;
end;


// Builds the primaryLocation object for one issue. Caller owns the returned object.
function BuildPrimaryLocation(const aIssue: TFpSonarIssue): TJSONObject;
var
  lRange: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('message', FormatMessage(aIssue.MessageKey, aIssue.MessageArgs));
  Result.Add('filePath', aIssue.FileName);

  // textRange omitted entirely for a position-less diagnostic.
  if aIssue.StartLine > 0 then
  begin
    lRange := TJSONObject.Create;
    lRange.Add('startLine', aIssue.StartLine);
    if aIssue.EndLine > 0 then
      lRange.Add('endLine', aIssue.EndLine)
    else
      lRange.Add('endLine', aIssue.StartLine);
    // 0-based columns: start = StartCol - 1; end = EndCol (inclusive 1-based
    // -> 0-based exclusive). Emit each only when its source col is present.
    if aIssue.StartCol > 0 then
      lRange.Add('startColumn', aIssue.StartCol - 1);
    if aIssue.EndCol > 0 then
      lRange.Add('endColumn', aIssue.EndCol);
    Result.Add('textRange', lRange);
  end;
end;


// Builds one SonarQube issue object for one issue (keys in fixed order).
function BuildIssue(const aIssue: TFpSonarIssue): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('engineId', 'FpSonar');                        // tool identity
  Result.Add('ruleId', aIssue.RuleId);                       // bare PascalCase
  Result.Add('severity', SeverityName(aIssue.Severity, True));
  Result.Add('type', SonarType(aIssue.IssueType));
  Result.Add('primaryLocation', BuildPrimaryLocation(aIssue));
  if aIssue.SuppressionSource <> ssActive then
    Result.Add('fpSonarSuppression', SuppressionName(aIssue.SuppressionSource));
end;


function FormatSonarJson(const aIssues: TFpSonarIssueArray): string;
var
  lRoot: TJSONObject;
  lIssues: TJSONArray;
  lStamp: TJSONObject;
  i: integer;
begin
  lRoot := TJSONObject.Create;
  try
    lIssues := TJSONArray.Create;
    // One issue object per issue, IN ARRAY ORDER — no re-sort.
    for i := 0 to High(aIssues) do
      lIssues.Add(BuildIssue(aIssues[i]));
    lRoot.Add('issues', lIssues);

    lStamp := TJSONObject.Create;
    lStamp.Add('format', 'sonarqube-generic');
    lStamp.Add('version', FpSonarSonarJsonVersion);
    lRoot.Add('_fpsonar', lStamp);

    Result := lRoot.FormatJSON;
  finally
    lRoot.Free;
  end;
end;


end.
