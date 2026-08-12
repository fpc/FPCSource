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
program test_fpsonar;

{ FPCUnit consoletestrunner test program }

{$mode objfpc}{$H+}

uses
  consoletestrunner,
  utstSmokeParse,
  utstScanner,
  utstParser,
  utstPas2jsDialect,
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
  utstProgressAbort,
  utstOutputText,
  utstOutputSarif,
  utstOutputSonarJson,
  utstConfig,
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
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  App := TTestRunner.Create(nil);
  try
    App.Initialize;
    App.Title := 'Fp-Sonar test suite';
    App.Run;
  finally
    App.Free;
  end;
end.
