{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the SARIF 2.1.0 output adapter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstOutputSarif;

{ The SARIF 2.1.0 output adapter. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, jsonparser,
  FpSonar.Types, FpSonar.Output.Sarif;

type
  { SARIF output adapter tests. }
  TOutputSarifTest = class(TTestCase)
  private
    function SyntheticIssues: TFpSonarIssueArray;
    function MutedIssues: TFpSonarIssueArray;
  published
    procedure TopLevelVersionAndSchema;
    procedure ToolDriverIdentityAndVersion;
    procedure ResultsCarryRuleLevelMessageFingerprint;
    procedure RegionPresenceFollowsPosition;
    procedure OutputIsDeterministic;
    procedure ActiveResultHasNoSuppressionsKey;
    procedure SuppressedResultCarriesSuppressionsArray;
  end;


implementation

function TOutputSarifTest.SyntheticIssues: TFpSonarIssueArray;

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
function TOutputSarifTest.MutedIssues: TFpSonarIssueArray;

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


procedure TOutputSarifTest.TopLevelVersionAndSchema;

var
  lData: TJSONData;

begin
  lData := GetJSON(FormatSarif(SyntheticIssues));
  try
    AssertEquals('top-level version', '2.1.0',
      lData.FindPath('version').AsString);
    AssertNotNull('$schema present', lData.FindPath('$schema'));
    AssertTrue('$schema points at the SARIF 2.1.0 schema',
      Pos('sarif-2.1.0', lData.FindPath('$schema').AsString) > 0);
  finally
    lData.Free;
  end;
end;


procedure TOutputSarifTest.ToolDriverIdentityAndVersion;

var
  lData: TJSONData;

begin
  lData := GetJSON(FormatSarif(SyntheticIssues));
  try
    AssertEquals('tool.driver.name (tool identity once)', 'FpSonar',
      lData.FindPath('runs[0].tool.driver.name').AsString);
    AssertEquals('tool.driver.version (version stamp)', FpSonarVersion,
      lData.FindPath('runs[0].tool.driver.version').AsString);
  finally
    lData.Free;
  end;
end;


procedure TOutputSarifTest.ResultsCarryRuleLevelMessageFingerprint;

var
  lData: TJSONData;
  lResults: TJSONArray;
  lR0: TJSONObject;

begin
  lData := GetJSON(FormatSarif(SyntheticIssues));
  try
    lResults := lData.FindPath('runs[0].results') as TJSONArray;
    AssertEquals('one result per issue', 2, lResults.Count);

    lR0 := lResults.Objects[0];
    AssertEquals('bare ruleId', 'LowercaseKeywords', lR0.Get('ruleId', ''));
    // sevMinor -> warning.
    AssertEquals('severity->level map', 'warning', lR0.Get('level', ''));
    AssertEquals('resolved message text', 'keyword "begin" should be lowercase',
      lR0.Objects['message'].Get('text', ''));
    // partialFingerprints carries the issue fingerprint under the v1 key.
    AssertEquals('partialFingerprints fpSonarFingerprint/v1', 'abc1230000000000',
      lR0.Objects['partialFingerprints'].Get('fpSonarFingerprint/v1', ''));
  finally
    lData.Free;
  end;
end;


procedure TOutputSarifTest.RegionPresenceFollowsPosition;

var
  lData: TJSONData;
  lResults: TJSONArray;
  lPhys0, lPhys1: TJSONObject;

begin
  lData := GetJSON(FormatSarif(SyntheticIssues));
  try
    lResults := lData.FindPath('runs[0].results') as TJSONArray;

    // Positioned issue: region present, startLine carried through.
    lPhys0 := lResults.Objects[0].FindPath('locations[0].physicalLocation')
      as TJSONObject;
    AssertNotNull('positioned issue has a region', lPhys0.Find('region'));
    AssertEquals('region.startLine', 4,
      lPhys0.FindPath('region.startLine').AsInteger);

    // Position-less issue: NO region.
    lPhys1 := lResults.Objects[1].FindPath('locations[0].physicalLocation')
      as TJSONObject;
    AssertNull('position-less issue has no region', lPhys1.Find('region'));
  finally
    lData.Free;
  end;
end;


procedure TOutputSarifTest.OutputIsDeterministic;

var
  lIssues: TFpSonarIssueArray;

begin
  lIssues := SyntheticIssues;
  AssertEquals('two calls produce byte-identical SARIF',
    FormatSarif(lIssues), FormatSarif(lIssues));
end;


procedure TOutputSarifTest.ActiveResultHasNoSuppressionsKey;

var
  lData: TJSONData;
  lResults: TJSONArray;

begin
  // The all-active default set emits NO suppressions key (absence => active, the
  // SARIF default — existing structure stays green).
  lData := GetJSON(FormatSarif(SyntheticIssues));
  try
    lResults := lData.FindPath('runs[0].results') as TJSONArray;
    AssertNull('active result has no suppressions key',
      lResults.Objects[0].Find('suppressions'));
  finally
    lData.Free;
  end;
end;


procedure TOutputSarifTest.SuppressedResultCarriesSuppressionsArray;

var
  lData: TJSONData;
  lResults: TJSONArray;
  lSupp: TJSONArray;

begin
  lData := GetJSON(FormatSarif(MutedIssues));
  try
    lResults := lData.FindPath('runs[0].results') as TJSONArray;
    // [0] active: no key; [1] muted: a suppressions array with the canonical shape.
    AssertNull('active result still has no suppressions key',
      lResults.Objects[0].Find('suppressions'));
    lSupp := lResults.Objects[1].Find('suppressions') as TJSONArray;
    AssertNotNull('muted result has a suppressions array', lSupp);
    AssertEquals('one suppression entry', 1, lSupp.Count);
    AssertEquals('kind is external', 'external',
      lSupp.Objects[0].Get('kind', ''));
    AssertEquals('justification names the source', 'baseline',
      lSupp.Objects[0].Get('justification', ''));
  finally
    lData.Free;
  end;
end;


initialization
  RegisterTest(TOutputSarifTest);

end.
