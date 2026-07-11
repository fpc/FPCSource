{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Fp-Sonar command-line utility

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fpsonar;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  CustApp,
  FpSonar.Types,
  FpSonar.CLI.Options,
  FpSonar.Engine,
  FpSonar.Issues,
  FpSonar.Config,
  FpSonar.RuleFramework,
  FpSonar.Baseline,
  FpSonar.Output.Text,
  FpSonar.Output.Sarif,
  FpSonar.Output.SonarJson,
  FpSonar.Rules.Layout,
  FpSonar.Rules.Tokens,
  FpSonar.Rules.Structure,
  FpSonar.Rules.Naming,
  FpSonar.Rules.Classes,
  FpSonar.Rules.Exceptions,
  FpSonar.Rules.Casts,
  FpSonar.Rules.Calls,
  FpSonar.Rules.Control,
  FpSonar.Rules.Imports,
  FpSonar.Rules.Refs,
  FpSonar.Rules.SemNaming,
  FpSonar.Rules.Unused,
  FpSonar.Rules.Trackers,
  FpSonar.Rules.Parens,
  FpSonar.Rules.Forms,
  FpSonar.Consts;

type

  { TFPSonarApplication }

  TFPSonarApplication = class(TCustomApplication)
  private
    FOpts: TFpSonarCliOptions;
    function Analyze: integer;
    function EmitConfig: integer;
    function GetOptions(var aOpts: TFpSonarCliOptions): boolean;
  public
    function CollectArgs: TStringArray;
    function RenderReport(aFormat: TFpSonarOutputFormat;
      const aIssues: TFpSonarIssueArray): string;
    function WriteReportFile(const aPath, aReport: string): boolean;
    procedure DoRun; override;
  end;

  function TFPSonarApplication.CollectArgs: TStringArray;
  var
    i: integer;
  begin
    SetLength(Result, ParamCount);
    for i := 1 to ParamCount do
      Result[i - 1] := ParamStr(i);
  end;

  // Renders the collected issues via the adapter selected by aFormat.
  function TFPSonarApplication.RenderReport(aFormat: TFpSonarOutputFormat;
  const aIssues: TFpSonarIssueArray): string;
  begin
    case aFormat of
      ofSarif: Result := FormatSarif(aIssues);
      ofSonarJson: Result := FormatSonarJson(aIssues);
      else
        Result := FormatText(aIssues);
    end;
  end;


  // Writes aReport verbatim to aPath. Returns False and reports to stderr on an IO failure.
  function TFPSonarApplication.WriteReportFile(const aPath, aReport: string): boolean;
  var
    lStream: TFileStream;
  begin
    lStream := nil;
    Result := True;
    try
      lStream := TFileStream.Create(aPath, fmCreate);
      if aReport <> '' then
        lStream.WriteBuffer(aReport[1], Length(aReport));
    except
      on E: Exception do
      begin
        Writeln(StdErr, Format(SCannotWriteOutput, [E.Message]));
        Result := False;
      end;
    end;
    lStream.Free;
  end;

  function TFPSonarApplication.GetOptions(var aOpts: TFpSonarCliOptions): boolean;
  var
    LOpts: TFpSonarCliOptions;
    LParamError: string;
  begin
    Result := False;
    LOpts := TFpSonarCliOptions.Parse(CollectArgs);
    // Usage error: report to stderr and exit 2 (NOT the quality-gate code).
    if LOpts.HadError then
    begin
      Writeln(StdErr, Format(SCliError, [LOpts.ErrorMessage]));
      Writeln(StdErr, STryHelp);
      ExitCode := 2;
      Exit;
    end;

    // Help / no command: banner + usage, exit 0.
    if LOpts.ShowHelp then
    begin
      Writeln(FpSonarBanner);
      Writeln(SHelpText);
      ExitCode := 0;
      Exit;
    end;

    // Fail fast on a bad per-rule param
    if not RuleRegistry.ValidateConfig(LOpts.RuleConfig, LParamError) then
    begin
      Writeln(StdErr, Format(SCliError, [LParamError]));
      ExitCode := 2;
      Exit;
    end;
    aOpts := LOpts;
    Result := True;
  end;

  function TFPSonarApplication.Analyze: integer;
  var
    LEngine: TFpSonarEngine;
    LCollector: TFpSonarIssueCollector;
    LSuppression: TFpSonarSuppressionMap;
    LReport: string;
    LEffective: TFpSonarIssueArray;
    LGate: TFpSonarGateOutcome;
    LBaseline: TFpSonarBaseline;
    LBaselineErr: string;
  begin
    // analyze: build the collector + engine, run the config, render + emit report.
    LEngine := nil;
    LSuppression := nil;
    LCollector := TFpSonarIssueCollector.Create;
    try
      LEngine := TFpSonarEngine.Create;
      LSuppression := TFpSonarSuppressionMap.Create;
      LEngine.Config := FOpts.RuleConfig;
      LEngine.SuppressionMap := LSuppression;
      LEngine.RealRtlPreferred := FOpts.RealRtlPreferred;
      LEngine.PpuAutoDetect := not FOpts.SyntheticOnly;
      LEngine.PpuCacheDir := FOpts.PpuCacheDir;
      LEngine.Run(FOpts.Config, LCollector);

      if FOpts.HasProjectDiag then
        Writeln(StdErr, Format(SProjectLoadNote, [FOpts.ProjectDiag.Message]));

      LEffective := FOpts.RuleConfig.ApplyTo(LCollector.Issues);

      if (FOpts.Command = 'analyze') and FOpts.MutedMode then
      begin
        if not LBaseline.LoadFromFile(FOpts.MutedBaselineFile, LBaselineErr) then
        begin
          Writeln(StdErr, Format(SCliError, [LBaselineErr]));
          ExitCode := 2;
        end
        else
          LEffective := ClassifySuppressions(LEffective, LSuppression,
            FOpts.RuleConfig.Suppressions, LBaseline);
      end
      else
        LEffective := ApplySuppressions(LEffective, LSuppression,
          FOpts.RuleConfig.Suppressions);

      if FOpts.Command = 'baseline' then
      begin
        // Snapshot generation:
        LReport := TFpSonarBaseline.FromIssues(LEffective).ToJSON;
        if FOpts.OutputFile = '' then
          Writeln(LReport)
        else if not WriteReportFile(FOpts.OutputFile, LReport + LineEnding) then
          ExitCode := 2;
      end
      else
      begin
        // Basic run
        if FOpts.NewCodeMode then
        begin
          if not LBaseline.LoadFromFile(FOpts.BaselineFile, LBaselineErr) then
          begin
            Writeln(StdErr, Format(SCliError, [LBaselineErr]));
            ExitCode := 2;
          end
          else
            LEffective := LBaseline.FilterNewCode(LEffective);
        end;

        if ExitCode <> 2 then
        begin
          LReport := RenderReport(FOpts.OutputFormat, LEffective);

          if FOpts.OutputFile = '' then
          begin
            Writeln(LReport);
            // Keep the summary only in text-to-stdout mode
            if FOpts.OutputFormat = ofText then
              Writeln(Format(SAnalyzedSummary,
                [Length(FOpts.Config.TargetFiles), Length(LEffective)]));
          end
          // Append the same trailing newline stdout's Writeln emits
          else if not WriteReportFile(FOpts.OutputFile, LReport + LineEnding) then
            ExitCode := 2;

          // Evaluate the quality gate.
          if FOpts.MutedMode then
            LGate := FOpts.RuleConfig.Gate.Evaluate(ActiveIssues(LEffective))
          else
            LGate := FOpts.RuleConfig.Gate.Evaluate(LEffective);
          if LGate.Failed then
            Writeln(StdErr, Format(SGateFailedNote, [LGate.Reason]));
        end;
      end;
    finally
      LEngine.Free;
      LCollector.Free;
      LSuppression.Free;
    end;
    Result := LGate.ExitCode;
  end;

  { init-config: emit a complete, editable JSON config listing every registered
    rule with its default enabled state + severity, plus the default gate policy.
    Honours --output/-o (default: stdout). }
  function TFPSonarApplication.EmitConfig: integer;
  var
    lRules: array of TRuleMetadata;
    lReport: string;
    i: integer;
  begin
    Result := 0;
    SetLength(lRules, RuleRegistry.Count);
    for i := 0 to RuleRegistry.Count - 1 do
      lRules[i] := RuleRegistry.Rule(i).Metadata;
    lReport := TFpSonarConfig.Default.TemplateToJSON(lRules);
    if FOpts.OutputFile = '' then
      Writeln(lReport)
    else if not WriteReportFile(FOpts.OutputFile, lReport + LineEnding) then
      Result := 2;
  end;

  procedure TFPSonarApplication.DoRun;
  var
    lCode: integer;
  begin
    Terminate;
    if not GetOptions(FOpts) then
      exit;
    if FOpts.Command = 'init-config' then
    begin
      ExitCode := EmitConfig;
      Exit;
    end;
    lCode := Analyze;
    if ExitCode <> 2 then
      ExitCode := lCode;
  end;

var
  Application: TFPSonarApplication;

begin
  Application := TFPSonarApplication.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
