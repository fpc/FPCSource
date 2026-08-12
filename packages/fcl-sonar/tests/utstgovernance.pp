{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the suppression-source governance/composition layer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstGovernance;

{ tests for the unified suppression-source model }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PScanner, FpSonar.Ingest, FpSonar.Types, FpSonar.Config,
  FpSonar.Issues, FpSonar.Baseline;

type
  { Governance composition tests. }
  TGovernanceTest = class(TTestCase)
  private
    function MakeFpIssue(const aRuleId, aFile: string; aLine: Integer;
      const aFingerprint: string): TFpSonarIssue;
    function MakeGlob(const aRule, aPath: string): TFpSonarSuppressionGlob;
    function NoSonarMapOn(const aFile: string; aLine: Integer):
      TFpSonarSuppressionMap;
    function BaselineWith(const aFingerprint: string): TFpSonarBaseline;
    function StrictTotalGate: TFpSonarGateThresholds;
  published
    procedure PrecedenceNoSonarWinsOverConfigAndBaseline;
    procedure PrecedenceConfigWinsOverBaseline;
    procedure BaselineOnlyClassifiesBaseline;
    procedure NoMatchIsActive;
    procedure TrackNoSonarExemptFromNoSonar;
    procedure ClassifyPreservesOrderAndFingerprint;
    procedure EmptySourcesAllActiveIsIdentity;
    procedure ActiveIssuesReturnsActiveSubsetInOrder;
    procedure SourceNamesAreLowercase;
  end;


implementation

function TGovernanceTest.MakeFpIssue(const aRuleId, aFile: string;
  aLine: Integer; const aFingerprint: string): TFpSonarIssue;

begin
  Result := TFpSonarIssue.Make(aRuleId, aFile, aLine, 1, aLine, 1, sevMinor,
    itCodeSmell, cfHigh, 'rule.' + aRuleId + '.message', []);
  Result.Fingerprint := aFingerprint;
end;


function TGovernanceTest.MakeGlob(const aRule, aPath: string):
  TFpSonarSuppressionGlob;

begin
  Result.RulePattern := aRule;
  Result.PathPattern := aPath;
end;


function TGovernanceTest.NoSonarMapOn(const aFile: string; aLine: Integer):
  TFpSonarSuppressionMap;

var
  lToks: TFpSonarTokenArray;

begin
  SetLength(lToks, 1);
  lToks[0].Kind := tkComment;
  lToks[0].Text := '// NOSONAR';
  lToks[0].Row := aLine;
  lToks[0].Col := 1;
  lToks[0].FileName := aFile;
  Result := TFpSonarSuppressionMap.Create;
  Result.AddFile(aFile, lToks);
end;


function TGovernanceTest.BaselineWith(const aFingerprint: string):
  TFpSonarBaseline;

var
  lIssues: TFpSonarIssueArray;

begin
  SetLength(lIssues, 1);
  lIssues[0] := MakeFpIssue('Seed', 'src/seed.pas', 1, aFingerprint);
  Result := TFpSonarBaseline.FromIssues(lIssues);
end;


function TGovernanceTest.StrictTotalGate: TFpSonarGateThresholds;

var
  lSev: TFpSonarSeverity;

begin
  // Every per-severity axis unlimited (-1, never trips); total capped at 0 so
  // ANY issue fails the gate — and an empty active set passes it.
  for lSev := Low(TFpSonarSeverity) to High(TFpSonarSeverity) do
    Result.MaxPerSeverity[lSev] := -1;
  Result.MaxTotal := 0;
end;


procedure TGovernanceTest.PrecedenceNoSonarWinsOverConfigAndBaseline;

var
  lIssues, lOut: TFpSonarIssueArray;
  lMap: TFpSonarSuppressionMap;
  lGlobs: TFpSonarSuppressionGlobArray;
  lBaseline: TFpSonarBaseline;

begin
  // An issue matching ALL THREE sources must report the most-intentional one.
  SetLength(lIssues, 1);
  lIssues[0] := MakeFpIssue('RuleX', 'a.pas', 10, 'fp-all');
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('RuleX', '');
  lBaseline := BaselineWith('fp-all');
  lMap := NoSonarMapOn('a.pas', 10);
  try
    lOut := ClassifySuppressions(lIssues, lMap, lGlobs, lBaseline);
    AssertEquals('all-three match resolves to nosonar (highest precedence)',
      Ord(ssNoSonar), Ord(lOut[0].SuppressionSource));
  finally
    lMap.Free;
  end;
end;


procedure TGovernanceTest.PrecedenceConfigWinsOverBaseline;

var
  lIssues, lOut: TFpSonarIssueArray;
  lGlobs: TFpSonarSuppressionGlobArray;
  lBaseline: TFpSonarBaseline;

begin
  // No NOSONAR (nil map): config glob outranks the baseline.
  SetLength(lIssues, 1);
  lIssues[0] := MakeFpIssue('RuleX', 'a.pas', 10, 'fp-cb');
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('RuleX', '');
  lBaseline := BaselineWith('fp-cb');
  lOut := ClassifySuppressions(lIssues, nil, lGlobs, lBaseline);
  AssertEquals('config glob outranks baseline', Ord(ssConfig),
    Ord(lOut[0].SuppressionSource));
end;


procedure TGovernanceTest.BaselineOnlyClassifiesBaseline;

var
  lIssues, lOut: TFpSonarIssueArray;
  lBaseline: TFpSonarBaseline;

begin
  // No NOSONAR, no glob: a baselined fingerprint classifies ssBaseline.
  SetLength(lIssues, 1);
  lIssues[0] := MakeFpIssue('RuleX', 'a.pas', 10, 'fp-b');
  lBaseline := BaselineWith('fp-b');
  lOut := ClassifySuppressions(lIssues, nil, nil, lBaseline);
  AssertEquals('baseline-only match classifies baseline', Ord(ssBaseline),
    Ord(lOut[0].SuppressionSource));
end;


procedure TGovernanceTest.NoMatchIsActive;

var
  lIssues, lOut: TFpSonarIssueArray;
  lBaseline: TFpSonarBaseline;

begin
  SetLength(lIssues, 1);
  lIssues[0] := MakeFpIssue('RuleX', 'a.pas', 10, 'fp-none');
  // A baseline that does NOT contain the issue's fingerprint.
  lBaseline := BaselineWith('some-other-fp');
  lOut := ClassifySuppressions(lIssues, nil, nil, lBaseline);
  AssertEquals('no source matches => active', Ord(ssActive),
    Ord(lOut[0].SuppressionSource));
end;


procedure TGovernanceTest.TrackNoSonarExemptFromNoSonar;

var
  lIssues, lOut: TFpSonarIssueArray;
  lMap: TFpSonarSuppressionMap;
  lBaseline: TFpSonarBaseline;

begin
  // The tracker on a NOSONAR line is exempt from the NOSONAR branch; with no
  // glob/baseline it stays ssActive (the exemption survives the refactor).
  SetLength(lIssues, 1);
  lIssues[0] := MakeFpIssue('TrackNoSonar', 'a.pas', 10, 'fp-track');
  lBaseline := BaselineWith('unrelated');
  lMap := NoSonarMapOn('a.pas', 10);
  try
    lOut := ClassifySuppressions(lIssues, lMap, nil, lBaseline);
    AssertEquals('TrackNoSonar on a NOSONAR line stays active', Ord(ssActive),
      Ord(lOut[0].SuppressionSource));
  finally
    lMap.Free;
  end;
end;


procedure TGovernanceTest.ClassifyPreservesOrderAndFingerprint;

var
  lIssues, lOut: TFpSonarIssueArray;
  lGlobs: TFpSonarSuppressionGlobArray;
  lBaseline: TFpSonarBaseline;
  i: Integer;

begin
  SetLength(lIssues, 3);
  lIssues[0] := MakeFpIssue('RuleA', 'a.pas', 1, 'fp-a');
  lIssues[1] := MakeFpIssue('RuleB', 'b.pas', 2, 'fp-b');
  lIssues[2] := MakeFpIssue('RuleC', 'c.pas', 3, 'fp-c');
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('RuleB', '');           // mark the middle one
  lBaseline := BaselineWith('fp-c');            // and baseline the last one

  lOut := ClassifySuppressions(lIssues, nil, lGlobs, lBaseline);
  AssertEquals('length preserved', 3, Length(lOut));
  for i := 0 to High(lIssues) do
    AssertEquals('fingerprint untouched at ' + IntToStr(i),
      lIssues[i].Fingerprint, lOut[i].Fingerprint);
  // Order preserved with the expected per-issue sources.
  AssertEquals('first active', Ord(ssActive), Ord(lOut[0].SuppressionSource));
  AssertEquals('second config', Ord(ssConfig), Ord(lOut[1].SuppressionSource));
  AssertEquals('third baseline', Ord(ssBaseline), Ord(lOut[2].SuppressionSource));
end;


procedure TGovernanceTest.EmptySourcesAllActiveIsIdentity;

var
  lIssues, lOut: TFpSonarIssueArray;
  lEmpty: TFpSonarBaseline;
  i: Integer;

begin
  SetLength(lIssues, 2);
  lIssues[0] := MakeFpIssue('RuleA', 'a.pas', 1, 'fp-a');
  lIssues[1] := MakeFpIssue('RuleB', 'b.pas', 2, 'fp-b');
  lEmpty := TFpSonarBaseline.FromIssues(nil);

  lOut := ClassifySuppressions(lIssues, nil, nil, lEmpty);
  AssertEquals('length preserved', 2, Length(lOut));
  for i := 0 to High(lOut) do
    AssertEquals('every issue active (identity) at ' + IntToStr(i),
      Ord(ssActive), Ord(lOut[i].SuppressionSource));
end;


procedure TGovernanceTest.ActiveIssuesReturnsActiveSubsetInOrder;

var
  lIssues, lActive: TFpSonarIssueArray;
  lGlobs: TFpSonarSuppressionGlobArray;
  lEmpty: TFpSonarBaseline;

begin
  SetLength(lIssues, 3);
  lIssues[0] := MakeFpIssue('RuleA', 'a.pas', 1, 'fp-a');
  lIssues[1] := MakeFpIssue('RuleB', 'b.pas', 2, 'fp-b');  // suppressed by glob
  lIssues[2] := MakeFpIssue('RuleC', 'c.pas', 3, 'fp-c');
  SetLength(lGlobs, 1);
  lGlobs[0] := MakeGlob('RuleB', '');
  lEmpty := TFpSonarBaseline.FromIssues(nil);

  lActive := ActiveIssues(ClassifySuppressions(lIssues, nil, lGlobs, lEmpty));
  AssertEquals('only the two active issues survive', 2, Length(lActive));
  AssertEquals('order kept: A first', 'fp-a', lActive[0].Fingerprint);
  AssertEquals('order kept: C second', 'fp-c', lActive[1].Fingerprint);
end;



procedure TGovernanceTest.SourceNamesAreLowercase;

begin
  AssertEquals('active', SuppressionSourceName(ssActive));
  AssertEquals('nosonar', SuppressionSourceName(ssNoSonar));
  AssertEquals('config', SuppressionSourceName(ssConfig));
  AssertEquals('baseline', SuppressionSourceName(ssBaseline));
end;


initialization
  RegisterTest(TGovernanceTest);

end.
