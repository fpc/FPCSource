{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the engine's cooperative progress/abort seam

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstProgressAbort;

{ TFpSonarEngine.OnProgress tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework, FpSonar.Engine,
  UtstFixtures, UtstCoreFixtures;

type
  { A synthetic LEX rule emitting one sentinel issue per file, so a test can
    count how many files were actually analyzed. }
  TSynthProgressRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Progress/abort seam tests. }
  TProgressAbortTest = class(TTestCase)
  private
    FSeenFiles: TFpSonarStringArray;
    FSeenIndex: array of Integer;
    FSeenTotal: array of Integer;
    FCancelAfter: Integer; // 0 = never cancel; else cancel before file > k
    // The OnProgress handler under test: records each call and, when
    // FCancelAfter > 0, requests a clean cancel once index exceeds it.
    procedure RecordProgress(const aFile: string; aIndex, aTotal: Integer;
      out aContinue: Boolean);
    function MakeConfig(const aFiles: array of string): TFpSonarAnalysisConfig;
    function RegisterSynth(aReg: TRuleRegistry): TSynthProgressRule;
    function CountSynth(const aCollector: TFpSonarIssueCollector): Integer;
    function AddThree(aFix: TTempFixtures; out aA, aB, aC: string): TFpSonarAnalysisConfig;
  published
    procedure ProgressFiresPerFileInSortedOrder;
    procedure AbortAfterKLeavesKFilesIssues;
    procedure AlwaysContinueMatchesNoCallback;
  end;


implementation

const
  cMode = 'OBJFPC';
  cSynthId = 'SynthProgress';

procedure TSynthProgressRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, 1, 1, 1, 1,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [], 'synth');
end;


procedure TProgressAbortTest.RecordProgress(const aFile: string;
  aIndex, aTotal: Integer; out aContinue: Boolean);

begin
  SetLength(FSeenFiles, Length(FSeenFiles) + 1);
  FSeenFiles[High(FSeenFiles)] := aFile;
  SetLength(FSeenIndex, Length(FSeenIndex) + 1);
  FSeenIndex[High(FSeenIndex)] := aIndex;
  SetLength(FSeenTotal, Length(FSeenTotal) + 1);
  FSeenTotal[High(FSeenTotal)] := aTotal;
  aContinue := (FCancelAfter = 0) or (aIndex <= FCancelAfter);
end;


function TProgressAbortTest.MakeConfig(const aFiles: array of string): TFpSonarAnalysisConfig;

var
  i: Integer;

begin
  Result.Mode := cMode;
  Result.TargetCPU := 'x86_64';
  Result.TargetOS := 'linux';
  SetLength(Result.TargetFiles, Length(aFiles));
  for i := 0 to High(aFiles) do
    Result.TargetFiles[i] := aFiles[i];
end;


function TProgressAbortTest.RegisterSynth(aReg: TRuleRegistry): TSynthProgressRule;

begin
  Result := TSynthProgressRule.Create(TRuleMetadata.Make(cSynthId, rtLex,
    rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, ''));
  aReg.Register(Result);
end;


function TProgressAbortTest.CountSynth(const aCollector: TFpSonarIssueCollector): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = cSynthId then
      Inc(Result);
end;


// Adds three clean fixtures named so their SORTED order is a < b < c 
function TProgressAbortTest.AddThree(aFix: TTempFixtures;
  out aA, aB, aC: string): TFpSonarAnalysisConfig;

begin
  aA := aFix.Add('a_progress.pas', cSmokeFixture);
  aB := aFix.Add('b_progress.pas', cSmokeFixture);
  aC := aFix.Add('c_progress.pas', cSmokeFixture);
  // Hand them in NON-sorted order; the engine sorts before dispatch.
  Result := MakeConfig([aC, aA, aB]);
end;


procedure TProgressAbortTest.ProgressFiresPerFileInSortedOrder;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarEngine;
  lCollector: TFpSonarIssueCollector;
  lA, lB, lC: string;
  lConfig: TFpSonarAnalysisConfig;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    RegisterSynth(lReg);
    SetLength(FSeenFiles, 0);
    SetLength(FSeenIndex, 0);
    SetLength(FSeenTotal, 0);
    FCancelAfter := 0;

    lConfig := AddThree(lFix, lA, lB, lC);
    lEngine.OnProgress := @Self.RecordProgress;
    lEngine.Run(lConfig, lCollector);

    AssertEquals('progress fired once per file', 3, Length(FSeenFiles));
    // Sorted order a < b < c, 1-based index, constant total.
    AssertEquals('1st file is sorted-first', lA, FSeenFiles[0]);
    AssertEquals('2nd file', lB, FSeenFiles[1]);
    AssertEquals('3rd file', lC, FSeenFiles[2]);
    AssertEquals('1-based index #1', 1, FSeenIndex[0]);
    AssertEquals('1-based index #2', 2, FSeenIndex[1]);
    AssertEquals('1-based index #3', 3, FSeenIndex[2]);
    AssertEquals('total on call #1', 3, FSeenTotal[0]);
    AssertEquals('total on call #2', 3, FSeenTotal[1]);
    AssertEquals('total on call #3', 3, FSeenTotal[2]);
    // All three files were actually analyzed (one sentinel each).
    AssertEquals('sentinel per analyzed file', 3, CountSynth(lCollector));
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TProgressAbortTest.AbortAfterKLeavesKFilesIssues;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarEngine;
  lCollector: TFpSonarIssueCollector;
  lA, lB, lC: string;
  lConfig: TFpSonarAnalysisConfig;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    RegisterSynth(lReg);
    SetLength(FSeenFiles, 0);
    SetLength(FSeenIndex, 0);
    SetLength(FSeenTotal, 0);
    { Cancel BEFORE the 3rd file: files 1 and 2 get analyzed, then the boundary
      check on index 3 requests a clean cancel. }
    FCancelAfter := 2;

    lConfig := AddThree(lFix, lA, lB, lC);
    lEngine.OnProgress := @Self.RecordProgress;
    lEngine.Run(lConfig, lCollector);

    { The onprogress was consulted at file 3's boundary (so 3 calls), but only the
      first two files were analyzed before the cancel took effect.}
    AssertEquals('consulted at every boundary up to the cancel', 3,
      Length(FSeenFiles));
    AssertEquals('exactly k files analyzed', 2, CountSynth(lCollector));
    // The retained issues are the first two sorted files, not the cancelled one.
    AssertEquals('kept file #1', lA, lCollector.Issues[0].FileName);
    AssertEquals('kept file #2', lB, lCollector.Issues[1].FileName);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TProgressAbortTest.AlwaysContinueMatchesNoCallback;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarEngine;
  lBaseline, lWithCb: TFpSonarIssueCollector;
  lA, lB, lC: string;
  lConfig: TFpSonarAnalysisConfig;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarEngine.CreateWith(lReg);
  lBaseline := TFpSonarIssueCollector.Create;
  lWithCb := TFpSonarIssueCollector.Create;
  try
    RegisterSynth(lReg);
    lConfig := AddThree(lFix, lA, lB, lC);

    // Baseline: no callback assigned (the default, today's behaviour).
    lEngine.OnProgress := nil;
    lEngine.Run(lConfig, lBaseline);

    // Same run with an always-continue callback: results must be identical.
    SetLength(FSeenFiles, 0);
    SetLength(FSeenIndex, 0);
    SetLength(FSeenTotal, 0);
    FCancelAfter := 0;
    lEngine.OnProgress := @Self.RecordProgress;
    lEngine.Run(lConfig, lWithCb);

    AssertTrue('baseline emitted issues', lBaseline.Count > 0);
    AssertEquals('identical issue count', lBaseline.Count, lWithCb.Count);
    for i := 0 to lBaseline.Count - 1 do
      begin
        AssertEquals('same RuleId at ' + IntToStr(i),
          lBaseline.Issues[i].RuleId, lWithCb.Issues[i].RuleId);
        AssertEquals('same FileName at ' + IntToStr(i),
          lBaseline.Issues[i].FileName, lWithCb.Issues[i].FileName);
        AssertEquals('same fingerprint at ' + IntToStr(i),
          lBaseline.Issues[i].Fingerprint, lWithCb.Issues[i].Fingerprint);
      end;
  finally
    lBaseline.Free;
    lWithCb.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TProgressAbortTest);

end.
