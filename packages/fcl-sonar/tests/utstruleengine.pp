{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the single-file rule dispatch engine

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRuleEngine;

{ Engine dispatch tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework, FpSonar.Config,
  UtstFixtures, UtstCoreFixtures;

type
  { A synthetic rule that emits one sentinel issue carrying its own RuleId. }
  TSynthEmitRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { A synthetic rule whose Apply raises — exercises the fault boundary. }
  TSynthRaisingRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { A synthetic rule that records the CompilerMode it received from the context
    into a unit-global, so a test can assert the engine threads it through. }
  TSynthModeRule = class(TRuleBase)
  public
    procedure Apply(const aContext: TRuleContext;
      const aCollector: TFpSonarIssueCollector); override;
  end;

  { Engine dispatch tests. }
  TRuleEngineTest = class(TTestCase)
  private
    function MakeSynth(aReg: TRuleRegistry; const aId: string; aTier: TRuleTier;
      aFeed: TRuleFeed; aEnabled: Boolean): TSynthEmitRule;
    function IndexOfRule(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
  published
    procedure DispatchesInFixedTierOrder;
    procedure GracefullyDegradesWhenParseFails;
    procedure DefaultDisabledRulesSkip;
    procedure RuleFaultBoundaryIsolatesRaisingRule;
    procedure SingleEmissionPathAndDeterminism;
    procedure ContextCarriesCompilerMode;
    procedure ConfigOverridesRuleEnable;
  end;


implementation

const
  cMode = 'OBJFPC';

var
  // Captures the CompilerMode TSynthModeRule saw, for ContextCarriesCompilerMode.
  GCapturedMode: string;

procedure TSynthEmitRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  aCollector.AddIssue(FMetadata.RuleId, aContext.FileName, 1, 1, 1, 1,
    FMetadata.Severity, FMetadata.Category, FMetadata.DefaultConfidence,
    FMetadata.MessageKey, [], 'synth');
end;


procedure TSynthRaisingRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  raise Exception.Create('synthetic boom');
end;


procedure TSynthModeRule.Apply(const aContext: TRuleContext;
  const aCollector: TFpSonarIssueCollector);

begin
  GCapturedMode := aContext.CompilerMode;
end;


function TRuleEngineTest.MakeSynth(aReg: TRuleRegistry; const aId: string;
  aTier: TRuleTier; aFeed: TRuleFeed; aEnabled: Boolean): TSynthEmitRule;

begin
  Result := TSynthEmitRule.Create(TRuleMetadata.Make(aId, aTier, aFeed, sevMinor,
    itCodeSmell, cfMedium, aEnabled, ''));
  aReg.Register(Result);
end;


function TRuleEngineTest.IndexOfRule(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      begin
        Result := i;
        Exit;
      end;
end;


procedure TRuleEngineTest.DispatchesInFixedTierOrder;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;
  lLex, lTok, lAst: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    // Deliberately SCRAMBLED registration order (Ast, Lex, Tok) to prove the
    // engine orders by TIER, not by registration order.
    MakeSynth(lReg, 'SynthAst', rtAst, rfAst, True);
    MakeSynth(lReg, 'SynthLex', rtLex, rfTokenStream, True);
    MakeSynth(lReg, 'SynthTok', rtTok, rfTokenStream, True);

    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    lLex := IndexOfRule(lCollector, 'SynthLex');
    lTok := IndexOfRule(lCollector, 'SynthTok');
    lAst := IndexOfRule(lCollector, 'SynthAst');
    AssertTrue('LEX rule fired', lLex >= 0);
    AssertTrue('TOK rule fired', lTok >= 0);
    AssertTrue('AST rule fired', lAst >= 0);

    // Fixed tier order LEX -> TOK -> AST in emission order.
    AssertTrue('LEX before TOK', lLex < lTok);
    AssertTrue('TOK before AST', lTok < lAst);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRuleEngineTest.GracefullyDegradesWhenParseFails;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;
  lParse, lLex: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    MakeSynth(lReg, 'SynthLex', rtLex, rfTokenStream, True);
    MakeSynth(lReg, 'SynthTok', rtTok, rfTokenStream, True);
    MakeSynth(lReg, 'SynthAst', rtAst, rfAst, True);
    // A SEM rule on the resolver feed: rfResolver is wired to the
    // per-unit resolver, which is built ONLY after a successful parse. Here the
    // parse FAILS, so no resolver is built -> the feed is absent -> the SEM rule
    // is skipped (graceful degradation). The positive case — rfResolver runs
    // when the feed IS present — is covered in utstResolver.
    MakeSynth(lReg, 'SynthSem', rtSem, rfResolver, True);

    // faultbad.pas: parse fails (Module nil) but the token stream survives.
    lEngine.Analyze(lFix.Add('faultbad.pas', cFaultBad), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    // LEX/TOK still run on the surviving tokens; AST/SEM degrade to skip.
    AssertTrue('LEX rule fired on token stream', IndexOfRule(lCollector,
      'SynthLex') >= 0);
    AssertTrue('TOK rule fired on token stream', IndexOfRule(lCollector,
      'SynthTok') >= 0);
    AssertEquals('AST rule skipped (no Module)', -1,
      IndexOfRule(lCollector, 'SynthAst'));
    AssertEquals('SEM rule skipped (no resolver feed)', -1,
      IndexOfRule(lCollector, 'SynthSem'));

    // The folded ParseError diagnostic is present and precedes rule issues.
    lParse := IndexOfRule(lCollector, 'ParseError');
    lLex := IndexOfRule(lCollector, 'SynthLex');
    AssertTrue('ParseError issue folded in', lParse >= 0);
    AssertTrue('diagnostic fold precedes rule issues', lParse < lLex);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRuleEngineTest.DefaultDisabledRulesSkip;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    MakeSynth(lReg, 'SynthOn', rtLex, rfTokenStream, True);
    MakeSynth(lReg, 'SynthOff', rtTok, rfTokenStream, False);

    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    AssertTrue('enabled rule fired', IndexOfRule(lCollector, 'SynthOn') >= 0);
    AssertEquals('default-disabled rule skipped', -1,
      IndexOfRule(lCollector, 'SynthOff'));
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRuleEngineTest.RuleFaultBoundaryIsolatesRaisingRule;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;
  lErr: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    // A raising rule and a sibling enabled rule on the same tier.
    lReg.Register(TSynthRaisingRule.Create(TRuleMetadata.Make('SynthBoom', rtTok,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));
    MakeSynth(lReg, 'SynthSibling', rtTok, rfTokenStream, True);

    // Must not raise out of the engine (one bad rule never voids the run).
    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    lErr := IndexOfRule(lCollector, 'RuleError');
    AssertTrue('RuleError issue emitted for the raising rule', lErr >= 0);
    // The RuleError message resolves through the catalogued template.
    AssertEquals('RuleError message resolves via the catalog',
      'Rule "SynthBoom" failed: synthetic boom',
      FormatMessage(lCollector.Issues[lErr].MessageKey,
        lCollector.Issues[lErr].MessageArgs));
    AssertTrue('sibling rule still fired',
      IndexOfRule(lCollector, 'SynthSibling') >= 0);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRuleEngineTest.SingleEmissionPathAndDeterminism;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lFirst, lSecond: TFpSonarIssueCollector;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lFirst := TFpSonarIssueCollector.Create;
  lSecond := TFpSonarIssueCollector.Create;
  try
    MakeSynth(lReg, 'SynthLex', rtLex, rfTokenStream, True);
    MakeSynth(lReg, 'SynthTok', rtTok, rfTokenStream, True);
    MakeSynth(lReg, 'SynthAst', rtAst, rfAst, True);

    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lFirst);
    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lSecond);

    AssertTrue('at least one issue emitted', lFirst.Count > 0);
    // Single emission path: everything went through AddIssue, so every
    // issue carries a non-empty fingerprint.
    for i := 0 to lFirst.Count - 1 do
      AssertTrue('every issue is fingerprinted',
        lFirst.Issues[i].Fingerprint <> '');

    // Determinism: identical input => identical output, in order.
    AssertEquals('identical issue count across runs', lFirst.Count,
      lSecond.Count);
    for i := 0 to lFirst.Count - 1 do
      begin
        AssertEquals('same RuleId at index ' + IntToStr(i),
          lFirst.Issues[i].RuleId, lSecond.Issues[i].RuleId);
        AssertEquals('same fingerprint at index ' + IntToStr(i),
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


procedure TRuleEngineTest.ContextCarriesCompilerMode;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;

begin
  // The engine must thread the analyzed compiler mode into TRuleContext, not
  // drop it — a mode-sensitive rule reads aContext.CompilerMode (M1 regression).
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    GCapturedMode := '<unset>';
    lReg.Register(TSynthModeRule.Create(TRuleMetadata.Make('SynthMode', rtLex,
      rfTokenStream, sevMinor, itCodeSmell, cfMedium, True, '')));

    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    AssertEquals('rule received the analyzed compiler mode', cMode,
      GCapturedMode);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


procedure TRuleEngineTest.ConfigOverridesRuleEnable;

var
  lFix: TTempFixtures;
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lCollector: TFpSonarIssueCollector;
  lConfig: TFpSonarConfig;
  lErr: string;

begin
  // Config beats the metadata default AT DISPATCH. Register a
  // default-ENABLED rule and a default-DISABLED one, then flip both via config.
  lFix := TTempFixtures.Create;
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  lCollector := TFpSonarIssueCollector.Create;
  try
    MakeSynth(lReg, 'CfgLexOn', rtLex, rfTokenStream, True);   // default-enabled
    MakeSynth(lReg, 'CfgTokOff', rtTok, rfTokenStream, False);  // default-disabled

    AssertTrue('config loads',
      lConfig.LoadFromJSON('{"rules":{"CfgLexOn":{"enabled":false},' +
        '"CfgTokOff":{"enabled":true}}}', lErr));
    lEngine.Config := lConfig;

    lEngine.Analyze(lFix.Add('smokefixture.pas', cSmokeFixture), cMode,
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], lCollector);

    // The configured-OFF rule does not fire despite its enabled metadata default.
    AssertEquals('configured-off rule skipped', -1,
      IndexOfRule(lCollector, 'CfgLexOn'));
    // The configured-ON rule fires despite its disabled metadata default.
    AssertTrue('configured-on rule fired',
      IndexOfRule(lCollector, 'CfgTokOff') >= 0);
  finally
    lCollector.Free;
    lEngine.Free;
    lReg.Free;
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TRuleEngineTest);

end.
