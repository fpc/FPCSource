{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    FPCUnit console test runner for the Fp-Sonar test suite

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fpsonar_tests;

{ FPCUnit consoletestrunner test program (LCL-free).
  Run with `--format=plain` so the runner sets a non-zero exit code on
  failures/errors (required for CI to fail the job). }

{$mode objfpc}{$H+}

uses
  consoletestrunner,
  utstSmokeParse,
  utstScanner,
  utstParser,
  utstFaultIsolation,
  utstFpcSource,
  utstPositionFidelity,
  utstFingerprint,
  utstIssueModel,
  utstRuleRegistry,
  utstRuleEngine,
  utstAnalysisConfig,
  utstProjectModel,
  utstEngine,
  utstOutputText,
  utstOutputSarif,
  utstOutputSonarJson,
  utstConfig,
  utstQualityGate,
  utstSuppression,
  utstBaseline,
  utstGovernance,
  utstRulesLayout,
  utstRulesTokens,
  utstAstVisitor,
  utstRulesStructure,
  utstRulesNaming,
  utstRulesClasses,
  utstRulesExceptions,
  utstResolver,
  utstRulesCasts,
  utstRulesCalls,
  utstRulesControl,
  utstRulesImports,
  utstRulesRefs,
  utstRulesSemNaming,
  utstRulesUnused,
  utstRulesTrackers,
  utstRulesParens,
  utstRulesForms;

var
  App: TTestRunner;

begin
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Fp-Sonar test suite';
    App.Run;
  finally
    App.Free;
  end;
end.
