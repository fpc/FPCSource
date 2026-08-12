{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Config-driven, multi-file analysis orchestration

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Engine;


{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  FpSonar.Types, FpSonar.RuleFramework, FpSonar.Issues, FpSonar.Config,
  FpSonar.SourceFile, FpSonar.Traversal,
  FpSonar.FpcSource;

type
  { Cooperative per-file progress/abort hook }
  TFpSonarProgressEvent = procedure(const aFile: string; aIndex, aTotal: integer;
    out aContinue: boolean) of object;

  { The config-driven, multi-file orchestrator above TFpSonarRuleEngine. }
  TFpSonarEngine = class
  private
    FRuleEngine: TFpSonarRuleEngine;
    FRealRtlPreferred: boolean;
    FPpuAutoDetect: boolean;
    FPpuCacheDir: string;
    FOnProgress: TFpSonarProgressEvent;
    function GetConfig: TFpSonarConfig;
    procedure SetConfig(const aConfig: TFpSonarConfig);
    function GetSuppressionMap: TFpSonarSuppressionMap;
    procedure SetSuppressionMap(const aMap: TFpSonarSuppressionMap);
  public
    // Drives the global RuleRegistry (production wiring); owns the inner engine.
    constructor Create;
    // Drives a specific registry. Owns the inner engine; does not own aRegistry.
    constructor CreateWith(aRegistry: TRuleRegistry);
    // Frees the owned inner TFpSonarRuleEngine.
    destructor Destroy; override;
    // Analyzes aConfig.TargetFiles in deterministic sorted order, accumulating
    // every file's issues into aCollector.
    procedure Run(const aConfig: TFpSonarAnalysisConfig;
      const aCollector: TFpSonarIssueCollector);
    // The ruleset/gate policy, forwarded to the inner dispatcher.
    property Config: TFpSonarConfig read GetConfig write SetConfig;
    // The optional NOSONAR suppression map, forwarded to the inner dispatcher.
    property SuppressionMap: TFpSonarSuppressionMap
      read GetSuppressionMap write SetSuppressionMap;
    // Opt-in real-RTL-preferred mode. When True, Run consumes FPC-source discovery. Default False.
    property RealRtlPreferred: boolean read FRealRtlPreferred write FRealRtlPreferred;
    // Auto-detect ppudump-from-live-.ppu resolution. Independent of the real-RTL preference.
    property PpuAutoDetect: boolean read FPpuAutoDetect write FPpuAutoDetect;
    // Persistent ppudump-stub cache directory (--ppu-cache)
    property PpuCacheDir: string read FPpuCacheDir write FPpuCacheDir;
    // Optional cooperative progress/abort hook, checked between files 
    property OnProgress: TFpSonarProgressEvent read FOnProgress write FOnProgress;
  end;

{ Parses every file in aFiles once and folds it into a fresh project-wide name-reference index
  (USE tier): the caller OWNS the returned index and must free it. }
function BuildProjectIndex(const aFiles: array of string; const aMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string)
  : TFpSonarProjectIndex; overload;

{ As above, additionally threading the real-RTL-preferred mode into
  each per-file parse so the project index folds the same names a real-RTL run resolves. }
function BuildProjectIndex(const aFiles: array of string; const aMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string;
  aRealRtl: boolean; aTargetPointerSize: integer;
  aDialect: TFpSonarDialect = dlDefault)
  : TFpSonarProjectIndex; overload;


implementation

// Sorts aFiles in place, case-sensitively (CompareStr)
procedure SortFiles(var aFiles: TFpSonarStringArray);
var
  i, j, lMin: integer;
  lTmp: string;
begin
  for i := 0 to High(aFiles) - 1 do
  begin
    lMin := i;
    for j := i + 1 to High(aFiles) do
      if CompareStr(aFiles[j], aFiles[lMin]) < 0 then
        lMin := j;
    if lMin <> i then
    begin
      lTmp := aFiles[i];
      aFiles[i] := aFiles[lMin];
      aFiles[lMin] := lTmp;
    end;
  end;
end;


// Maps a target CPU name to its pointer width in bytes.
function PointerSizeForCpu(const aCpu: string): integer;
var
  lCpu: string;
begin
  lCpu := LowerCase(aCpu);
  if lCpu = '' then
    Result := SizeOf(Pointer)
  else if (lCpu = 'x86_64') or (lCpu = 'aarch64') or (lCpu = 'ppc64')
    or (lCpu = 'ppc64le') or (lCpu = 'riscv64') or (lCpu = 'loongarch64') then
    Result := 8
  else
    Result := 4;
end;


// Returns aDefines with the objpas uses-chain marker removed
function StripObjPasMarker(const aDefines: TFpSonarStringArray)
: TFpSonarStringArray;
var
  i: integer;
begin
  SetLength(Result, 0);
  for i := 0 to High(aDefines) do
    if not SameText(aDefines[i], cImplicitObjPasMarker) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := aDefines[i];
    end;
end;


function BuildProjectIndex(const aFiles: array of string; const aMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string)
: TFpSonarProjectIndex;
begin
  Result := BuildProjectIndex(aFiles, aMode, aDefines, aUnitPaths, aIncludePaths,
    False, 0);
end;


function BuildProjectIndex(const aFiles: array of string; const aMode: string;
  const aDefines, aUnitPaths, aIncludePaths: array of string;
  aRealRtl: boolean; aTargetPointerSize: integer;
  aDialect: TFpSonarDialect = dlDefault)
: TFpSonarProjectIndex;
var
  lSourceFile: TFpSonarSourceFile;
  i: integer;
begin
  Result := TFpSonarProjectIndex.Create;
  // A fresh SourceFile per file
  for i := 0 to High(aFiles) do
  begin
    lSourceFile := TFpSonarSourceFile.Create;
    try
      lSourceFile.Analyze(aFiles[i], aMode, aDefines, aUnitPaths,
        aIncludePaths, aRealRtl, aTargetPointerSize, aDialect);
      if lSourceFile.Module <> nil then
        Result.AddModule(lSourceFile.Module)
      else
        Result.MarkUnparseable;
    finally
      lSourceFile.Free;
    end;
  end;
end;


constructor TFpSonarEngine.Create;
begin
  inherited Create;
  FRuleEngine := TFpSonarRuleEngine.Create;
end;


constructor TFpSonarEngine.CreateWith(aRegistry: TRuleRegistry);
begin
  inherited Create;
  FRuleEngine := TFpSonarRuleEngine.CreateWith(aRegistry);
end;


destructor TFpSonarEngine.Destroy;
begin
  FRuleEngine.Free;
  inherited Destroy;
end;


function TFpSonarEngine.GetConfig: TFpSonarConfig;
begin
  Result := FRuleEngine.Config;
end;


procedure TFpSonarEngine.SetConfig(const aConfig: TFpSonarConfig);
begin
  FRuleEngine.Config := aConfig;
end;


function TFpSonarEngine.GetSuppressionMap: TFpSonarSuppressionMap;
begin
  Result := FRuleEngine.SuppressionMap;
end;


procedure TFpSonarEngine.SetSuppressionMap(const aMap: TFpSonarSuppressionMap);
begin
  FRuleEngine.SuppressionMap := aMap;
end;


procedure TFpSonarEngine.Run(const aConfig: TFpSonarAnalysisConfig;
  const aCollector: TFpSonarIssueCollector);
var
  lFiles: TFpSonarStringArray;
  lDefines: TFpSonarStringArray;
  lUnitPaths, lIncludePaths: TFpSonarStringArray;
  lMode: string;
  lProjectIndex: TFpSonarProjectIndex;
  lFpcCfg: TFpSonarFpcSourceConfig;
  lRtl, lMerged: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;
  lPointerSize: integer;
  lContinue: boolean;
  i: integer;
begin
  // Copy then sort: never mutate the caller's config; analyze deterministically.
  lFiles := Copy(aConfig.TargetFiles, 0, Length(aConfig.TargetFiles));
  SortFiles(lFiles);

  lMode := aConfig.Mode;
  lPointerSize := 0;

  if FRealRtlPreferred then
  begin
      { Consume the FPC-source discovery:
        merge its real-RTL-Fu/-Fi/defines into the run's effective config so they ride the existing
        resolver path args.
        Honour any CLI --cpu/--os override; an empty value keeps the host default.
        Defines are produced even when the source tree is not located
        => a missing tree simply degrades to the synthetic fallback per RTL chain name, inside the resolver.
        }
    lFpcCfg := TFpSonarFpcSourceConfig.Default;
    if aConfig.TargetCPU <> '' then
      lFpcCfg.TargetCPU := aConfig.TargetCPU;
    if aConfig.TargetOS <> '' then
      lFpcCfg.TargetOS := aConfig.TargetOS;
    lFpcCfg.BuildRtlSourceConfig(lRtl, lDiag);
    // Merge the paths + defines only
    lRtl.Mode := '';
    lRtl.TargetCPU := '';
    lRtl.TargetOS := '';
    lMerged := aConfig.Merge(lRtl);
    lUnitPaths := lMerged.UnitSearchPaths;
    lIncludePaths := lMerged.IncludePaths;
    // objpas needs the implicit uses-chain, not -d: strip the marker
    // out of the -d defines that reach the scanner.
    lDefines := StripObjPasMarker(lMerged.EffectiveDefines);
    lPointerSize := PointerSizeForCpu(lMerged.TargetCPU);
  end
  else
  begin
    // Synthetic-preferred default (byte-identical to the committed path).
    lDefines := aConfig.EffectiveDefines;
    lUnitPaths := Copy(aConfig.UnitSearchPaths, 0,
      Length(aConfig.UnitSearchPaths));
    lIncludePaths := Copy(aConfig.IncludePaths, 0,
      Length(aConfig.IncludePaths));
  end;

  { Build the project-wide name-reference index in a parse-pass over
    the whole target set before per-file dispatch, so a project-scope USE rule
    sees every unit's references when it runs on any one file.
    Owned + freed here }
  lProjectIndex := BuildProjectIndex(lFiles, lMode, lDefines,
    lUnitPaths, lIncludePaths, FRealRtlPreferred, lPointerSize,
    aConfig.Dialect);
  FRuleEngine.ProjectIndex := lProjectIndex;
  // Forward auto-detect ppudump resolution to the dispatcher (per-run).
  FRuleEngine.PpuAutoDetect := FPpuAutoDetect;
  FRuleEngine.PpuCacheDir := FPpuCacheDir;
  try
    for i := 0 to High(lFiles) do
    begin
      // Cooperative seam: report progress + honour a caller cancel between files. 
      if Assigned(FOnProgress) then
      begin
        lContinue := True;
        FOnProgress(lFiles[i], i + 1, Length(lFiles), lContinue);
        if not lContinue then
          Break;
      end;
      FRuleEngine.Analyze(lFiles[i], lMode, lDefines,
        lUnitPaths, lIncludePaths, FRealRtlPreferred, lPointerSize, aCollector,
        aConfig.Dialect);
    end;
  finally
    FRuleEngine.ProjectIndex := nil;
    lProjectIndex.Free;
  end;
end;


end.
