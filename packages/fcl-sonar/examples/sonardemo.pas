{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 Free Pascal Team

    Simple demo of fcl-sonar package.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program sonardemo;

{$mode objfpc}{$H+}

uses
  FpSonar.Types,        // TFpSonarAnalysisConfig, TFpSonarGateOutcome, SplitSearchPath,
                        // FormatMessage, FpSonarBanner
  FpSonar.Config,       // TFpSonarConfig, SeverityName
  FpSonar.Issues,       // TFpSonarIssueCollector, TFpSonarSuppressionMap
  FpSonar.Engine,       // TFpSonarEngine
  FpSonar.Output.Text,  // FormatText
  // Rule units -- referenced so their initialization sections register into
  // the global registry.
  //FpSonar.Rules.Calls,
  //FpSonar.Rules.Casts,
  //FpSonar.Rules.Classes,
  //FpSonar.Rules.Concurrency,
  //FpSonar.Rules.CondComp,
  //FpSonar.Rules.Consts,
  //FpSonar.Rules.Control,
  //FpSonar.Rules.Dataflow,
  //FpSonar.Rules.Eval,
  //FpSonar.Rules.Exceptions,
  //FpSonar.Rules.Forms,
  //FpSonar.Rules.FpcStyle,
  //FpSonar.Rules.Generics,
  //FpSonar.Rules.Imports,
  //FpSonar.Rules.Layout,
  //FpSonar.Rules.Lifetime,
  //FpSonar.Rules.Naming,
  FpSonar.Rules.Parens,
  //FpSonar.Rules.Refs,
  //FpSonar.Rules.Semnaming,
  //FpSonar.Rules.Strings,
  FpSonar.Rules.Structure,
  //FpSonar.Rules.Tokens,
  //FpSonar.Rules.Trackers,
  FpSonar.Rules.Unused,

  SysUtils;

var
  Analysis: TFpSonarAnalysisConfig;
  Cfg: TFpSonarConfig;
  Engine: TFpSonarEngine;
  Collector: TFpSonarIssueCollector;
  Issues: TFpSonarIssueArray;
  Gate: TFpSonarGateOutcome;
  I: Integer;
begin
  WriteLn(FpSonarBanner);

  // -- 1. What and how to analyze ------------------------------------------
  Analysis := TFpSonarAnalysisConfig.Default;   // Mode='OBJFPC', host CPU/OS
  SetLength(Analysis.UnitSearchPaths, 1);
  Analysis.UnitSearchPaths[0] := '';
  SetLength(Analysis.IncludePaths, 1);
  Analysis.IncludePaths[0] := './';
  SetLength(Analysis.TargetFiles, 1);
  if ParamCount=1 then
    Analysis.TargetFiles[0] := ParamStr(1)
  else
    Analysis.TargetFiles[0] := 'sonardemo.pas';

  // -- 2. Ruleset and gate policy --------------------------------------------
  Cfg := TFpSonarConfig.Default;
  SetLength(Cfg.Rules, 1);                      // one per-rule override:
  Cfg.Rules[0].RuleId := '<id-from-fpsonar.rules.consts>';  // note 2
  Cfg.Rules[0].HasEnabled := True;
  Cfg.Rules[0].Enabled := False;
  Cfg.Gate.MaxTotal := 50;                      // quality gate: >50 issues fails
  Cfg.Gate.MaxPerSeverity[sevBlocker] := 0;

  // -- 3. Run ------------------------------------------------------------------
  Engine := TFpSonarEngine.Create;              // global-registry wiring
  Collector := TFpSonarIssueCollector.Create;
  try
    Engine.Config := Cfg;
    Engine.Run(Analysis, Collector);            // accumulates across all files

    Issues := Collector.Issues;

    // -- 4. Report ---------------------------------------------------------
    //Writeln(FormatText(Issues));                // output built-in human-readable report

    for I := 0 to High(Issues) do               // ...or render it yourself:
      WriteLn(Format('%s:%d:%d: %s [%s] %s', [Issues[I].FileName,
        Issues[I].StartLine, Issues[I].StartCol,
        SeverityName(Issues[I].Severity), Issues[I].RuleId,
        FormatMessage(Issues[I].MessageKey, Issues[I].MessageArgs)]));
    Writeln(High(Issues)+1,' issue(s).');

    // -- 5. Quality gate -> process exit code --------------------------------
    Gate := Cfg.Gate.Evaluate(Issues);
    if Gate.Failed then
      WriteLn('gate FAILED: ', Gate.Reason);
    ExitCode := Gate.ExitCode;                  // 0 = pass, 1 = fail
  finally
    Collector.Free;
    Engine.Free;
  end;
end.
