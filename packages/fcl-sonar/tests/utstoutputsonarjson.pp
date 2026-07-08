{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the SonarQube generic-issue JSON output adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstOutputSonarJson;

{ The SonarQube generic issue JSON output adapter. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  FpSonar.Types, FpSonar.Output.SonarJson;

type
  { SonarQube generic issue JSON output adapter tests. }
  TOutputSonarJsonTest = class(TTestCase)
  private
    function SyntheticIssues: TFpSonarIssueArray;
    function MutedIssues: TFpSonarIssueArray;
  published
    procedure IssuesArrayAndEngineId;
    procedure SeverityAndTypeMaps;
    procedure LocationMessageAndColumnMapping;
    procedure PositionLessIssueHasNoTextRange;
    procedure VersionStampPresent;
    procedure OutputIsDeterministic;
    procedure ActiveIssueHasNoSuppressionKey;
    procedure SuppressedIssueCarriesSuppressionField;
  end;


implementation

function TOutputSonarJsonTest.SyntheticIssues: TFpSonarIssueArray;

begin
  RegisterMessage('rule.LowercaseKeywords.message',
    'keyword "%s" should be lowercase');

  SetLength(Result, 2);
  Result[0] := TFpSonarIssue.Make('LowercaseKeywords', 'src/foo.pas', 4, 7, 4, 11,
    sevMinor, itCodeSmell, cfHigh, 'rule.LowercaseKeywords.message', ['begin']);
  Result[0].Fingerprint := 'abc1230000000000';
  Result[1] := TFpSonarIssue.Make('ParseError', 'src/bad.pas', 0, 0, 0, 0,
    sevMajor, itCodeSmell, cfHigh, 'rule.ParseError.message', ['boom']);
  Result[1].Fingerprint := 'def4560000000000';
end;


// One active + one baseline-muted issue (reportability).
function TOutputSonarJsonTest.MutedIssues: TFpSonarIssueArray;

begin
  RegisterMessage('rule.LowercaseKeywords.message',
    'keyword "%s" should be lowercase');

  SetLength(Result, 2);
  Result[0] := TFpSonarIssue.Make('LowercaseKeywords', 'src/foo.pas', 4, 7, 4, 11,
    sevMinor, itCodeSmell, cfHigh, 'rule.LowercaseKeywords.message', ['begin']);
  Result[0].SuppressionSource := ssActive;
  Result[1] := TFpSonarIssue.Make('LowercaseKeywords', 'src/bar.pas', 9, 1, 9, 5,
    sevMinor, itCodeSmell, cfHigh, 'rule.LowercaseKeywords.message', ['end']);
  Result[1].SuppressionSource := ssBaseline;
end;


procedure TOutputSonarJsonTest.IssuesArrayAndEngineId;

var
  lData: TJSONData;
  lIssues: TJSONArray;

begin
  lData := GetJSON(FormatSonarJson(SyntheticIssues));
  try
    lIssues := lData.FindPath('issues') as TJSONArray;
    AssertEquals('issues array length', 2, lIssues.Count);
    AssertEquals('engineId (tool identity)', 'FpSonar',
      lIssues.Objects[0].Get('engineId', ''));
    AssertEquals('bare ruleId', 'LowercaseKeywords',
      lIssues.Objects[0].Get('ruleId', ''));
  finally
    lData.Free;
  end;
end;


procedure TOutputSonarJsonTest.SeverityAndTypeMaps;

var
  lData: TJSONData;
  lI0: TJSONObject;

begin
  lData := GetJSON(FormatSonarJson(SyntheticIssues));
  try
    lI0 := (lData.FindPath('issues') as TJSONArray).Objects[0];
    // sevMinor -> MINOR; itCodeSmell -> CODE_SMELL (exact maps).
    AssertEquals('severity map', 'MINOR', lI0.Get('severity', ''));
    AssertEquals('type map', 'CODE_SMELL', lI0.Get('type', ''));
  finally
    lData.Free;
  end;
end;


procedure TOutputSonarJsonTest.LocationMessageAndColumnMapping;

var
  lData: TJSONData;
  lI0: TJSONObject;

begin
  lData := GetJSON(FormatSonarJson(SyntheticIssues));
  try
    lI0 := (lData.FindPath('issues') as TJSONArray).Objects[0];
    AssertEquals('primaryLocation.filePath', 'src/foo.pas',
      lI0.FindPath('primaryLocation.filePath').AsString);
    AssertEquals('resolved message', 'keyword "begin" should be lowercase',
      lI0.FindPath('primaryLocation.message').AsString);
    // 0-based startColumn = StartCol - 1 = 6.
    AssertEquals('textRange.startColumn = StartCol - 1', 6,
      lI0.FindPath('primaryLocation.textRange.startColumn').AsInteger);
  finally
    lData.Free;
  end;
end;


procedure TOutputSonarJsonTest.PositionLessIssueHasNoTextRange;

var
  lData: TJSONData;
  lPrim1: TJSONObject;

begin
  lData := GetJSON(FormatSonarJson(SyntheticIssues));
  try
    lPrim1 := (lData.FindPath('issues') as TJSONArray).Objects[1]
      .Objects['primaryLocation'];
    AssertNull('position-less issue has no textRange', lPrim1.Find('textRange'));
  finally
    lData.Free;
  end;
end;


procedure TOutputSonarJsonTest.VersionStampPresent;

var
  lData: TJSONData;

begin
  lData := GetJSON(FormatSonarJson(SyntheticIssues));
  try
    AssertEquals('in-band _fpsonar version stamp', FpSonarSonarJsonVersion,
      lData.FindPath('_fpsonar.version').AsString);
  finally
    lData.Free;
  end;
end;


procedure TOutputSonarJsonTest.OutputIsDeterministic;

var
  lIssues: TFpSonarIssueArray;

begin
  lIssues := SyntheticIssues;
  AssertEquals('two calls produce byte-identical sonar-json',
    FormatSonarJson(lIssues), FormatSonarJson(lIssues));
end;


procedure TOutputSonarJsonTest.ActiveIssueHasNoSuppressionKey;

var
  lData: TJSONData;
  lIssues: TJSONArray;

begin
  // The all-active default set emits no fpSonarSuppression key.
  lData := GetJSON(FormatSonarJson(SyntheticIssues));
  try
    lIssues := lData.FindPath('issues') as TJSONArray;
    AssertNull('active issue has no fpSonarSuppression key',
      lIssues.Objects[0].Find('fpSonarSuppression'));
  finally
    lData.Free;
  end;
end;


procedure TOutputSonarJsonTest.SuppressedIssueCarriesSuppressionField;

var
  lData: TJSONData;
  lIssues: TJSONArray;

begin
  lData := GetJSON(FormatSonarJson(MutedIssues));
  try
    lIssues := lData.FindPath('issues') as TJSONArray;
    AssertNull('active issue still has no suppression key',
      lIssues.Objects[0].Find('fpSonarSuppression'));
    AssertEquals('muted issue carries its source', 'baseline',
      lIssues.Objects[1].Get('fpSonarSuppression', ''));
  finally
    lData.Free;
  end;
end;


initialization
  RegisterTest(TOutputSonarJsonTest);

end.
