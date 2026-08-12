{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    FPC source-tree configuration and path discovery

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.FpcSource;

{ FPC source-tree config & path discovery. }

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  FpSonar.Types;

type
  // Constant-as-default config for FPC-source discovery.
  TFpSonarFpcSourceConfig = record
    // explicit FPC source root ('' = discover)
    SourceDir: string;
    // e.g. 'x86_64' (feeds the CPU define + per-CPU dir)
    TargetCPU: string;
    // e.g. 'linux' (feeds the OS define(s) + OS dirs)
    TargetOS: string;
    // -Fu dir templates ('<os>'/'<cpu>' placeholders)
    UnitDirTemplates: TFpSonarStringArray;
    // -Fi dir templates ('<os>'/'<cpu>' placeholders)
    IncludeDirTemplates: TFpSonarStringArray;
    // the FPC_HAS_FEATURE_* family to emit
    FeatureDefines: TFpSonarStringArray;
    // Built-in default: SourceDir empty (discover); host TargetCPU/OS via the
    // compile-time macros; layout templates + feature defines = the named
    // constants below.
    class function Default: TFpSonarFpcSourceConfig; static;
    // The full RTL define set: EffectiveDefines (OS/CPU/FPC) + the
    // FPC_HAS_FEATURE_* family + the implicit objpas marker, first-occurrence
    // de-duplicated.
    function RtlSourceDefines: TFpSonarStringArray;
    // Assembles Self's paths + defines into aAnalysis. Returns True iff the
    // source tree was located (paths populated); on failure returns False with
    // empty paths + aDiag filled. Defines are still produced.
    function BuildRtlSourceConfig(out aAnalysis: TFpSonarAnalysisConfig;
      out aDiag: TFpSonarDiagnostic): boolean;
  end;

const
  // The CodeTools source-layout heuristic, generalized by <os>/<cpu>.
  // -Fu (units) and -Fi (includes) are kept distinct per the verified split.
  cFpcUnitDirTemplates: array[0..3] of string = (
    'rtl/<os>',
    'rtl/objpas',
    'rtl/objpas/sysutils',
    'rtl/objpas/classes'
    );
  cFpcIncludeDirTemplates: array[0..5] of string = (
    'rtl/inc',
    'rtl/<cpu>',
    'rtl/unix',
    'rtl/<os>',
    'rtl/objpas',
    'rtl/<os>/<cpu>'   // the per-CPU include dir (e.g. rtl/linux/x86_64)
    );

  // The implicit objpas unit is on the RTL uses-chain but is not a -d define;
  cImplicitObjPasMarker = 'objpas';

  // The full FPC feature-define family (tfeature set, FPC 3.x).
  cFpcFeatureDefines: array[0..23] of string = (
    'FPC_HAS_FEATURE_HEAP',
    'FPC_HAS_FEATURE_INITFINAL',
    'FPC_HAS_FEATURE_RTTI',
    'FPC_HAS_FEATURE_CLASSES',
    'FPC_HAS_FEATURE_EXCEPTIONS',
    'FPC_HAS_FEATURE_EXITCODE',
    'FPC_HAS_FEATURE_ANSISTRINGS',
    'FPC_HAS_FEATURE_WIDESTRINGS',
    'FPC_HAS_FEATURE_TEXTIO',
    'FPC_HAS_FEATURE_CONSOLEIO',
    'FPC_HAS_FEATURE_FILEIO',
    'FPC_HAS_FEATURE_RANDOM',
    'FPC_HAS_FEATURE_VARIANTS',
    'FPC_HAS_FEATURE_OBJECTS',
    'FPC_HAS_FEATURE_DYNARRAYS',
    'FPC_HAS_FEATURE_THREADING',
    'FPC_HAS_FEATURE_COMMANDARGS',
    'FPC_HAS_FEATURE_PROCESSES',
    'FPC_HAS_FEATURE_STACKCHECK',
    'FPC_HAS_FEATURE_DYNLIBS',
    'FPC_HAS_FEATURE_SOFTFPU',
    'FPC_HAS_FEATURE_OBJECTIVEC1',
    'FPC_HAS_FEATURE_RESOURCES',
    'FPC_HAS_FEATURE_UNICODESTRINGS'
    );

  // Documented well-known FPC SOURCE locations.
  cWellKnownFpcSourceDirs: array[0..3] of string = (
    '/usr/share/fpcsrc',
    '/usr/local/share/fpcsrc',
    '/usr/lib/fpc/src',
    '/usr/local/lib/fpc/src'
    );

// True iff aRoot looks like an FPC source tree (rtl/inc/system.inc present).
function IsFpcSourceTree(const aRoot: string): boolean;

{ Locates + validates the FPC SOURCE root. Precedence:
  explicit aOverride -> $FPCDIR / $fpcsrcdir env -> documented well-known locations.
  An explicit but invalid aOverride does NOT fall through (deterministic). }
function LocateFpcSourceDir(const aOverride: string;
  out aDiag: TFpSonarDiagnostic): string;

{ Expands the source layout into the -Fu (units) / -Fi (includes) split:
  applies each template under aRoot, substituting <os>/<cpu>;
  emits only existing dirs, absolute + normalized }
procedure ExpandFpcSourceLayout(const aRoot, aOS, aCPU: string;
  const aUnitTemplates, aIncludeTemplates: TFpSonarStringArray;
  out aUnitPaths, aIncludePaths: TFpSonarStringArray);


implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

class function TFpSonarFpcSourceConfig.Default: TFpSonarFpcSourceConfig;
var
  i: integer;
begin
  Result.SourceDir := '';
  // The build host's CPU/OS, expanded at compile time.
  Result.TargetCPU := {$I %FPCTARGETCPU%};
  Result.TargetOS := {$I %FPCTARGETOS%};
  SetLength(Result.UnitDirTemplates, Length(cFpcUnitDirTemplates));
  for i := Low(cFpcUnitDirTemplates) to High(cFpcUnitDirTemplates) do
    Result.UnitDirTemplates[i] := cFpcUnitDirTemplates[i];
  SetLength(Result.IncludeDirTemplates, Length(cFpcIncludeDirTemplates));
  for i := Low(cFpcIncludeDirTemplates) to High(cFpcIncludeDirTemplates) do
    Result.IncludeDirTemplates[i] := cFpcIncludeDirTemplates[i];
  SetLength(Result.FeatureDefines, Length(cFpcFeatureDefines));
  for i := Low(cFpcFeatureDefines) to High(cFpcFeatureDefines) do
    Result.FeatureDefines[i] := cFpcFeatureDefines[i];
end;


// Appends aValue to aList only if no earlier entry equals it, preserving first-occurrence order.
procedure AddUnique(var aList: TFpSonarStringArray; const aValue: string);
var
  i: integer;
begin
  if aValue = '' then
    Exit;
  for i := 0 to High(aList) do
    if aList[i] = aValue then
      Exit;
  SetLength(aList, Length(aList) + 1);
  aList[High(aList)] := aValue;
end;


// Returns aValue when non-empty, else aFallback.
function NonEmptyStr(const aValue, aFallback: string): string;
begin
  if aValue <> '' then
    Result := aValue
  else
    Result := aFallback;
end;


// Returns aArr when it has entries, else a copy of aDefault.
function NonEmptyOr(const aArr: TFpSonarStringArray;
  const aDefault: array of string): TFpSonarStringArray;
var
  i: integer;
begin
  if Length(aArr) > 0 then
  begin
    Result := aArr;
    Exit;
  end;
  SetLength(Result, Length(aDefault));
  for i := 0 to High(aDefault) do
    Result[i] := aDefault[i];
end;


// Joins a template path under aRoot and normalizes it to an absolute path using the host path delimiter.
function JoinUnderRoot(const aRoot, aRelative: string): string;
var
  lRel: string;
begin
  lRel := StringReplace(aRelative, '/', PathDelim, [rfReplaceAll]);
  lRel := StringReplace(lRel, '\', PathDelim, [rfReplaceAll]);
  Result := ExpandFileName(IncludeTrailingPathDelimiter(aRoot) + lRel);
end;


// Substitutes the <os>/<cpu> placeholders in a layout template. FPC source
// dirs are lower-case, so the values are lowered.
function ExpandTemplate(const aTemplate, aOS, aCPU: string): string;
begin
  Result := StringReplace(aTemplate, '<os>', LowerCase(aOS), [rfReplaceAll]);
  Result := StringReplace(Result, '<cpu>', LowerCase(aCPU), [rfReplaceAll]);
end;


function IsFpcSourceTree(const aRoot: string): boolean;
begin
  Result := (aRoot <> '') and
    FileExists(JoinUnderRoot(aRoot, 'rtl/inc/system.inc'));
end;


// Fills aDiag as a path-discovery degrade diagnostic (dkParseError)
procedure SetDiscoveryDiag(out aDiag: TFpSonarDiagnostic;
  const aFileName, aMessage: string);
begin
  aDiag.FileName := aFileName;
  aDiag.Row := 0;
  aDiag.Col := 0;
  aDiag.Kind := dkParseError;
  aDiag.Message := aMessage;
end;


function LocateFpcSourceDir(const aOverride: string;
  out aDiag: TFpSonarDiagnostic): string;
var
  lEnv: string;
  i: integer;
begin
  Result := '';
  SetDiscoveryDiag(aDiag, '', '');
  { 1. Explicit override wins and does not fall through (deterministic): if it is
       not a valid source tree the run degrades rather than searching the host. }
  if aOverride <> '' then
  begin
    if IsFpcSourceTree(aOverride) then
      Result := ExpandFileName(aOverride)
    else
      SetDiscoveryDiag(aDiag, aOverride,
        'configured FPC source dir is not a valid FPC source tree: ' + aOverride);
    Exit;
  end;
  // 2. Environment: $FPCDIR then $fpcsrcdir.
  lEnv := GetEnvironmentVariable('FPCDIR');
  if (lEnv <> '') and IsFpcSourceTree(lEnv) then
  begin
    Result := ExpandFileName(lEnv);
    Exit;
  end;
  lEnv := GetEnvironmentVariable('fpcsrcdir');
  if (lEnv <> '') and IsFpcSourceTree(lEnv) then
  begin
    Result := ExpandFileName(lEnv);
    Exit;
  end;
  // 3. Documented well-known locations.
  for i := Low(cWellKnownFpcSourceDirs) to High(cWellKnownFpcSourceDirs) do
    if IsFpcSourceTree(cWellKnownFpcSourceDirs[i]) then
    begin
      Result := ExpandFileName(cWellKnownFpcSourceDirs[i]);
      Exit;
    end;
  SetDiscoveryDiag(aDiag, '',
    'could not locate an FPC source tree (no override, $FPCDIR/$fpcsrcdir, or well-known location matched)');
end;


procedure ExpandFpcSourceLayout(const aRoot, aOS, aCPU: string;
  const aUnitTemplates, aIncludeTemplates: TFpSonarStringArray;
  out aUnitPaths, aIncludePaths: TFpSonarStringArray);

  procedure ExpandInto(const aTemplates: TFpSonarStringArray;
  var aResult: TFpSonarStringArray);
  var
    i: integer;
    lDir: string;
  begin
    SetLength(aResult, 0);
    if aRoot = '' then
      Exit;
    for i := 0 to High(aTemplates) do
    begin
      lDir := JoinUnderRoot(aRoot, ExpandTemplate(aTemplates[i], aOS, aCPU));
      if DirectoryExists(lDir) then
        AddUnique(aResult, lDir);
    end;
  end;

begin
  ExpandInto(aUnitTemplates, aUnitPaths);
  ExpandInto(aIncludeTemplates, aIncludePaths);
end;


function TFpSonarFpcSourceConfig.RtlSourceDefines: TFpSonarStringArray;
var
  lAnalysis: TFpSonarAnalysisConfig;
  lBase, lFeatures: TFpSonarStringArray;
  i: integer;
begin
  SetLength(Result, 0);
  // 1. OS/CPU/FPC + user defines — reuse EffectiveDefines.
  // An unset TargetCPU/OS keeps the host default rather than clobbering it.
  lAnalysis := TFpSonarAnalysisConfig.Default;
  lAnalysis.TargetCPU := NonEmptyStr(TargetCPU, lAnalysis.TargetCPU);
  lAnalysis.TargetOS := NonEmptyStr(TargetOS, lAnalysis.TargetOS);
  lBase := lAnalysis.EffectiveDefines;
  for i := 0 to High(lBase) do
    AddUnique(Result, lBase[i]);
  // 2. The FPC_HAS_FEATURE_* family .
  lFeatures := NonEmptyOr(FeatureDefines, cFpcFeatureDefines);
  for i := 0 to High(lFeatures) do
    AddUnique(Result, lFeatures[i]);
  // 3. The implicit objpas unit marker.
  AddUnique(Result, cImplicitObjPasMarker);
end;


function TFpSonarFpcSourceConfig.BuildRtlSourceConfig(
  out aAnalysis: TFpSonarAnalysisConfig; out aDiag: TFpSonarDiagnostic): boolean;
var
  lRoot: string;
begin
  aAnalysis := TFpSonarAnalysisConfig.Default;
  // Non-empty override wins; an unset target keeps the host default.
  aAnalysis.TargetCPU := NonEmptyStr(TargetCPU, aAnalysis.TargetCPU);
  aAnalysis.TargetOS := NonEmptyStr(TargetOS, aAnalysis.TargetOS);
  // Defines are produced regardless of whether the source tree is located.
  aAnalysis.Defines := RtlSourceDefines;
  SetLength(aAnalysis.UnitSearchPaths, 0);
  SetLength(aAnalysis.IncludePaths, 0);
  lRoot := LocateFpcSourceDir(SourceDir, aDiag);
  Result := lRoot <> '';
  if not Result then
    Exit;
  // Empty template overrides fall back to the built-in constant defaults;
  ExpandFpcSourceLayout(lRoot, aAnalysis.TargetOS, aAnalysis.TargetCPU,
    NonEmptyOr(UnitDirTemplates, cFpcUnitDirTemplates),
    NonEmptyOr(IncludeDirTemplates, cFpcIncludeDirTemplates),
    aAnalysis.UnitSearchPaths, aAnalysis.IncludePaths);
end;


end.
