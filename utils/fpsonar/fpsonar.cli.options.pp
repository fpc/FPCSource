{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Command-line argument parsing and AnalysisConfig assembly

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.CLI.Options;

{ argv -> AnalysisConfig translation for the analyze/baseline commands }

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, FpSonar.Types,
  FpSonar.Config;

type
  // Selected output/report format. ofText is the default.
  TFpSonarOutputFormat = (ofText, ofSarif, ofSonarJson);

  { The parsed CLI request: the command, help/error flags, the resolved
    AnalysisConfig, and any soft project-load diagnostic the CLI should surface.}
  TFpSonarCliOptions = record
    // 'analyze' or 'baseline'
    Command: string;
    // --help/-h, or no command given
    ShowHelp: Boolean;
    // a usage error (unknown flag, missing value)
    HadError: Boolean;
    // human-readable reason when HadError
    ErrorMessage: string;
    // first --project / positional .lpi-.lpk, if any
    ProjectFile: string;
    // a project file failed to load (soft)
    HasProjectDiag: Boolean;
    // the first such failure
    ProjectDiag: TFpSonarDiagnostic;
    // the assembled, precedence-resolved config
    Config: TFpSonarAnalysisConfig;
    // --format/-f (default ofText)
    OutputFormat: TFpSonarOutputFormat;
    // --output/-o ('' = stdout)
    OutputFile: string;
    // --config/-c (default TFpSonarConfig.Default)
    RuleConfig: TFpSonarConfig;
    // --new-code <file> given (default False)
    NewCodeMode: Boolean;
    // the --new-code argument: baseline path
    BaselineFile: string;
    // --baseline <file> given (default False)
    MutedMode: Boolean;
    // the --baseline argument: baseline path
    MutedBaselineFile: string;
    // --real-rtl given (default False)
    RealRtlPreferred: Boolean;
    // --synthetic-only given (default False => // auto-detect ppudump is default)
    SyntheticOnly: Boolean;
    // --ppu-cache <dir>: persistent ppudump-stub cache directory ('' = per-run temp only)
    PpuCacheDir: string;
    // Parses argv (excluding argv[0]) into a TFpSonarCliOptions.
    class function Parse(const aArgs: array of string): TFpSonarCliOptions; static;
  end;

implementation

uses
  FpSonar.Consts;

// Appends aValue to aList.
procedure AddStr(var aList: TFpSonarStringArray; const aValue: string);

begin
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aValue;
end;


// True iff aName has a source-target extension (.pas/.pp/.lpr). .inc is a
// fragment, not a target, and is deliberately excluded.
function IsSourceTarget(const aName: string): Boolean;

var
  lExt: string;

begin
  lExt := LowerCase(ExtractFileExt(aName));
  Result := (lExt = '.pas') or (lExt = '.pp') or (lExt = '.lpr');
end;


// Recursively collects *.pas/*.pp/*.lpr files under aDir into aFiles (absolute
// paths). Discovery order is irrelevant — the Engine sorts before dispatch.
procedure CollectSourceFiles(const aDir: string; var aFiles: TFpSonarStringArray);

var
  lInfo: TSearchRec;
  lBase, lFull: string;

begin
  lBase := IncludeTrailingPathDelimiter(aDir);
  if FindFirst(lBase + '*', faAnyFile, lInfo) = 0 then
    try
      repeat
        if (lInfo.Name = '.') or (lInfo.Name = '..') then
          Continue;
        lFull := lBase + lInfo.Name;
        if (lInfo.Attr and faDirectory) <> 0 then
          CollectSourceFiles(lFull, aFiles)
        else if IsSourceTarget(lInfo.Name) then
          AddStr(aFiles, ExpandFileName(lFull));
      until FindNext(lInfo) <> 0;
    finally
      FindClose(lInfo);
    end;
end;


class function TFpSonarCliOptions.Parse(const aArgs: array of string): TFpSonarCliOptions;

var
  lCli: TFpSonarAnalysisConfig;      // CLI-only overrides (managed fields auto-empty)
  lProjectFiles: TFpSonarStringArray;
  lDiscovered: TFpSonarStringArray;
  lModel: TFpSonarProjectModel;
  lPmConfig: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;
  i, j: Integer;
  lArg, lAbs, lExt, lFmt, lCfgPath, lCfgErr, lDialect: string;

  // Records a usage error (first one wins).
  procedure FlagError(const aWhat: string);

  begin
    if not Result.HadError then
      begin
        Result.HadError := True;
        Result.ErrorMessage := aWhat;
      end;
  end;

  // Returns the next arg as the value of a flag, flagging a usage error when
  // it is missing.
  function ConsumeValue(const aFlag: string): string;

  begin
    if i + 1 <= High(aArgs) then
      begin
        Inc(i);
        Result := aArgs[i];
      end
    else
      begin
        Result := '';
        FlagError(Format(SMissingValue, [aFlag]));
      end;
  end;

begin
  Result:=Default(TFpSonarCliOptions);
  Result.OutputFormat := ofText;
  Result.RuleConfig := TFpSonarConfig.Default;

  // --- 1. Parse argv into command / overrides / positionals. ---
  i := 0;
  while i <= High(aArgs) do
    begin
      lArg := aArgs[i];
      if (lArg <> '') and (lArg[1] = '-') then
        begin
          // Long and bare flags first, then the glued FPC-style families.
          if (lArg = '--help') or (lArg = '-h') then
            Result.ShowHelp := True
          else if lArg = '--define' then
            AddStr(lCli.Defines, ConsumeValue('--define'))
          else if lArg = '--mode' then
            lCli.Mode := ConsumeValue('--mode')
          else if lArg = '--cpu' then
            lCli.TargetCPU := ConsumeValue('--cpu')
          else if lArg = '--os' then
            lCli.TargetOS := ConsumeValue('--os')
          else if (lArg = '--project') or (lArg = '-p') then
            AddStr(lProjectFiles, ConsumeValue('--project'))
          else if (lArg = '--format') or (lArg = '-f') then
            begin
            lFmt := ConsumeValue('--format');
            if lFmt = 'text' then
              Result.OutputFormat := ofText
            else if lFmt = 'sarif' then
              Result.OutputFormat := ofSarif
            else if lFmt = 'sonar-json' then
              Result.OutputFormat := ofSonarJson
            else if lFmt <> '' then
              FlagError(Format(SUnknownFormat, [lFmt]));
            end
          else if (lArg = '--output') or (lArg = '-o') then
            Result.OutputFile := ConsumeValue('--output')
          else if lArg = '--new-code' then
            begin
            Result.BaselineFile := ConsumeValue('--new-code');
            Result.NewCodeMode := True;
            end
          else if lArg = '--baseline' then
            begin
            Result.MutedBaselineFile := ConsumeValue('--baseline');
            Result.MutedMode := True;
            end
          else if (lArg = '--config') or (lArg = '-c') then
            begin
            lCfgPath := ConsumeValue('--config');
            if (lCfgPath <> '') and
              not Result.RuleConfig.LoadFromFile(lCfgPath, lCfgErr) then
              FlagError(Format(SCannotLoadConfig, [lCfgPath, lCfgErr]));
            end
          else if lArg = '--real-rtl' then
            Result.RealRtlPreferred := True
          else if lArg = '--synthetic-only' then
            Result.SyntheticOnly := True
          else if lArg = '--pas2js' then
            lCli.Dialect := dlPas2js
          else if lArg = '--dialect' then
            begin
            lDialect := ConsumeValue('--dialect');
            if lDialect = 'pas2js' then
              lCli.Dialect := dlPas2js
            else if lDialect = 'default' then
              lCli.Dialect := dlDefault
            else if lDialect <> '' then
              FlagError(Format(SUnknownDialect, [lDialect]));
            end
          else if lArg = '--pas2js-src' then
            AddStr(lCli.UnitSearchPaths, ConsumeValue('--pas2js-src'))
          else if lArg = '--ppu-cache' then
            Result.PpuCacheDir := ConsumeValue('--ppu-cache')
          else if Copy(lArg, 12) = '--ppu-cache=' then
            Result.PpuCacheDir := Copy(lArg, 13, Length(lArg) - 12)
          else if lArg = '-d' then
            AddStr(lCli.Defines, ConsumeValue('-d'))
          else if lArg = '-M' then
            lCli.Mode := ConsumeValue('-M')
          else if lArg = '-Fu' then
            AddStr(lCli.UnitSearchPaths, ConsumeValue('-Fu'))
          else if lArg = '-Fi' then
            AddStr(lCli.IncludePaths, ConsumeValue('-Fi'))
          else if Copy(lArg, 1, 3) = '-Fu' then
            AddStr(lCli.UnitSearchPaths, Copy(lArg, 4, Length(lArg) - 3))
          else if Copy(lArg, 1, 3) = '-Fi' then
            AddStr(lCli.IncludePaths, Copy(lArg, 4, Length(lArg) - 3))
          else if Copy(lArg, 1, 2) = '-d' then
            AddStr(lCli.Defines, Copy(lArg, 3, Length(lArg) - 2))
          else if Copy(lArg, 1, 2) = '-M' then
            lCli.Mode := Copy(lArg, 3, Length(lArg) - 2)
          else
            FlagError(Format(SUnknownOption, [lArg]));
        end
      else if Result.Command = '' then
        Result.Command := lArg
      else
        begin
          lAbs := ExpandFileName(lArg);
          lExt := LowerCase(ExtractFileExt(lAbs));
          if DirectoryExists(lAbs) then
            CollectSourceFiles(lAbs, lDiscovered)
          else if (lExt = '.lpi') or (lExt = '.lpk') then
            AddStr(lProjectFiles, lAbs)
          else if lExt <> '.inc' then
            AddStr(lDiscovered, lAbs);
        end;
      Inc(i);
    end;

  if Result.NewCodeMode and Result.MutedMode then
    FlagError(SMutuallyExclusive);

  if (Result.Command <> 'analyze') and (Result.Command <> 'baseline')
    and (Result.Command <> 'init-config') then
    Result.ShowHelp := True;
  if Length(lProjectFiles) > 0 then
    Result.ProjectFile := lProjectFiles[0];

  Result.Config := TFpSonarAnalysisConfig.Default;

  lModel := TFpSonarProjectModel.Create;
  try
    for j := 0 to High(lProjectFiles) do
      if lModel.TryLoad(lProjectFiles[j], lPmConfig, lDiag) then
        Result.Config := Result.Config.Merge(lPmConfig)
      else if not Result.HasProjectDiag then
        begin
          Result.HasProjectDiag := True;
          Result.ProjectDiag := lDiag;
        end;
  finally
    lModel.Free;
  end;

  Result.Config := Result.Config.Merge(lCli);

  for j := 0 to High(lDiscovered) do
    AddStr(Result.Config.TargetFiles, lDiscovered[j]);
end;

end.
