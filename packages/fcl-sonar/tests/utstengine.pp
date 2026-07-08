{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the multi-file analysis engine (TFpSonarEngine)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstEngine;

{ TFpSonarEngine tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework, FpSonar.Engine,
  UtstFixtures, UtstCoreFixtures;

type
  { A synthetic LEX rule emitting one sentinel issue per file (carrying the
    file it ran on), so a test can observe per-file processing order. }
  TSynthFileRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Engine orchestration tests. }
  TEngineTest = class(TTestCase)
  private
    function MakeConfig(const aFiles: array of string): TFpSonarAnalysisConfig;
    function RegisterSynth(aReg: TRuleRegistry): TSynthFileRule;
    function CountRule(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
  published
    procedure RunsFilesInSortedOrder;
    procedure IsolatesUnparseableFile;
    procedure ZeroRulesCleanRunYieldsNoIssues;
    procedure RunIsDeterministic;
  end;


implementation

const
  cMode = 'OBJFPC';
  cSynthId = 'SynthFile';

procedure TSynthFileRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, 1, 1, 1, 1,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [], 'synth');
end;


function TEngineTest.MakeConfig(const aFiles: array of string): TFpSonarAnalysisConfig;

var
  i: Integer;

begin
  // x86_64/linux so EffectiveDefines = ['FPC','CPUX86_64','UNIX','LINUX'].
  Result.Mode := cMode;
  Result.TargetCPU := 'x86_64';
  Result.TargetOS := 'linux';
  SetLength(Result.TargetFiles, Length(aFiles));
  for i := 0 to High(aFiles) do
    Result.TargetFiles[i] := aFiles[i];
end;


function TEngineTest.RegisterSynth(aReg: TRuleRegistry): TSynthFileRule;

begin
  Result := TSynthFileRule.Create(TRuleMetadata.Make(cSynthId, rtLex, rfTokenStream,
    sevMinor, itCodeSmell, cfMedium, True, ''));
  aReg.Register(Result);
end;


function TEngineTest.CountRule(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


procedure TEngineTest.RunsFilesInSortedOrder;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarEngine;
  lCollector: TFpSonarIssueCollector;
  lSmoke, lScanner, lFirstExpected: string;
  lSeen: array of string;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    RegisterSynth(lReg);
    lSmoke := lFix.Add('smokefixture.pas', cSmokeFixture);
    lScanner := lFix.Add('scannerfixture.pas', cScannerFixture);

    // TargetFiles given in REVERSE-sorted order; the engine must sort them.
    lEngine.Run(MakeConfig([lSmoke, lScanner]), lCollector);

    // One sentinel per file, in sorted-path order (not input order).
    SetLength(lSeen, 0);
    for i := 0 to lCollector.Count - 1 do
      if lCollector.Issues[i].RuleId = cSynthId then
        begin
          SetLength(lSeen, Length(lSeen) + 1);
          lSeen[High(lSeen)] := lCollector.Issues[i].FileName;
        end;

    AssertEquals('both files analyzed', 2, Length(lSeen));
    if CompareStr(lSmoke, lScanner) < 0 then
      lFirstExpected := lSmoke
    else
      lFirstExpected := lScanner;
    AssertEquals('first analyzed file is sorted-first', lFirstExpected,
      lSeen[0]);
    AssertTrue('second analyzed file is the other one',
      (lSeen[1] = lSmoke) or (lSeen[1] = lScanner));
    AssertTrue('the two files differ', lSeen[0] <> lSeen[1]);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TEngineTest.IsolatesUnparseableFile;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarEngine;
  lCollector: TFpSonarIssueCollector;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    RegisterSynth(lReg);

    // faultbad.pas fails to parse; smokefixture/scannerfixture are clean.
    lEngine.Run(MakeConfig([lFix.Add('faultbad.pas', cFaultBad),
      lFix.Add('smokefixture.pas', cSmokeFixture), lFix.Add('scannerfixture.pas', cScannerFixture)]),
      lCollector);

    // The bad file surfaces a ParseError; the run continued for the others.
    AssertTrue('ParseError folded for the bad file',
      CountRule(lCollector, 'ParseError') >= 1);
    // LEX rule fires on every file's token stream (tokens survive a failed
    // parse) => one sentinel per target, including the bad one.
    AssertEquals('sentinel per file (bad file did not void the run)', 3,
      CountRule(lCollector, cSynthId));
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TEngineTest.ZeroRulesCleanRunYieldsNoIssues;

var
  lFix: TTempFixtures;
  lEngine: TFpSonarEngine;
  lCollector: TFpSonarIssueCollector;

begin
  // The real LEX layout rules AND the TOK
  // declaration/keyword rules are registered into the global registry, so this run now actually
  // exercises all of them. smokefixture.pas is both LEX-clean (no tabs, no
  // trailing whitespace, longest line < 120, no >=5-digit unseparated literal)
  // and TOK-clean (all keywords lowercase; a single type and a single var
  // section, no consecutive same-kind sections; every declaration is single-name;
  // SumTriple has no empty parentheses), so it legitimately produces zero
  // issues — the assertion stays at 0.
  lFix := TTempFixtures.Create;
  lEngine := TFpSonarEngine.Create;
  lCollector := TFpSonarIssueCollector.Create;
  try
    lEngine.Run(MakeConfig([lFix.Add('smokefixture.pas', cSmokeFixture)]), lCollector);
    AssertEquals('zero issues on a clean file with no rules', 0,
      lCollector.Count);
  finally
    lCollector.Free;
    lEngine.Free;
    lFix.Free;
  end;
end;


procedure TEngineTest.RunIsDeterministic;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarEngine;
  lFirst, lSecond: TFpSonarIssueCollector;
  lConfig: TFpSonarAnalysisConfig;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarEngine.CreateWith(lReg);
  lFirst := TFpSonarIssueCollector.Create;
  lSecond := TFpSonarIssueCollector.Create;
  try
    RegisterSynth(lReg);
    lConfig := MakeConfig([lFix.Add('smokefixture.pas', cSmokeFixture),
      lFix.Add('scannerfixture.pas', cScannerFixture)]);

    lEngine.Run(lConfig, lFirst);
    lEngine.Run(lConfig, lSecond);

    AssertTrue('at least one issue emitted', lFirst.Count > 0);
    AssertEquals('identical issue count', lFirst.Count, lSecond.Count);
    for i := 0 to lFirst.Count - 1 do
      begin
        AssertEquals('same RuleId at ' + IntToStr(i),
          lFirst.Issues[i].RuleId, lSecond.Issues[i].RuleId);
        AssertEquals('same FileName at ' + IntToStr(i),
          lFirst.Issues[i].FileName, lSecond.Issues[i].FileName);
        AssertEquals('same fingerprint at ' + IntToStr(i),
          lFirst.Issues[i].Fingerprint, lSecond.Issues[i].Fingerprint);
      end;
  finally
    lFirst.Free;
    lSecond.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TEngineTest);

end.
