{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for FPC source-tree config and path discovery

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstFpcSource;

{ FPC source-tree config & path discovery tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types,
  FpSonar.FpcSource;

type
  { FPC source discovery tests. }
  TFpcSourceTest = class(TTestCase)
  private
    FRoot: string;  // the synthetic fake FPC source tree
    // Asserts aActual equals aExpected element-for-element (and same length).
    procedure AssertArray(const aMsg: string; const aExpected: array of string;
      const aActual: TFpSonarStringArray);
    // Asserts aActual contains aValue (membership, order-independent).
    procedure AssertContains(const aMsg: string; const aValue: string;
      const aActual: TFpSonarStringArray);
    // Asserts aActual does NOT contain aValue.
    procedure AssertNotContains(const aMsg: string; const aValue: string;
      const aActual: TFpSonarStringArray);
    // The absolute, normalized form of a '/'-relative dir under the fake root.
    function AbsDir(const aRelative: string): string;
    // Recursively removes a directory tree (LCL-free).
    procedure RemoveTree(const aDir: string);
    // Creates an empty stub file.
    procedure WriteStub(const aPath: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LayoutSplitMatchesLinuxX8664Golden;
    procedure LayoutEmitsOnlyExistingDirs;
    procedure LayoutEmptyRootYieldsEmpty;
    procedure DefinesContainOsCpuFeaturesAndObjpas;
    procedure DefaultConfigDefinesAreBuiltInConstant;
    procedure FeatureDefineOverrideWins;
    procedure EmptyFeatureOverrideFallsBackToBuiltIn;
    procedure EmptyTargetFallsBackToHost;
    procedure EmptyTemplatesFallBackToBuiltIn;
    procedure TemplateOverrideWins;
    procedure InvalidRootDegradesToDiagnostic;
    procedure BuildRtlSourceConfigOnValidTree;
    procedure BuildRtlSourceConfigDegradesOnMissingTree;
    procedure DiscoveryIsDeterministic;
  end;


implementation

procedure TFpcSourceTest.AssertArray(const aMsg: string;
  const aExpected: array of string; const aActual: TFpSonarStringArray);

var
  i: Integer;

begin
  AssertEquals(aMsg + ' (length)', Length(aExpected), Length(aActual));
  for i := 0 to High(aExpected) do
    AssertEquals(aMsg + ' [' + IntToStr(i) + ']', aExpected[i], aActual[i]);
end;


procedure TFpcSourceTest.AssertContains(const aMsg: string;
  const aValue: string; const aActual: TFpSonarStringArray);

var
  i: Integer;

begin
  for i := 0 to High(aActual) do
    if aActual[i] = aValue then
      Exit;
  Fail(aMsg + ': expected to contain "' + aValue + '"');
end;


procedure TFpcSourceTest.AssertNotContains(const aMsg: string;
  const aValue: string; const aActual: TFpSonarStringArray);

var
  i: Integer;

begin
  for i := 0 to High(aActual) do
    if aActual[i] = aValue then
      Fail(aMsg + ': expected NOT to contain "' + aValue + '"');
end;


function TFpcSourceTest.AbsDir(const aRelative: string): string;

var
  lRel: string;

begin
  lRel := StringReplace(aRelative, '/', PathDelim, [rfReplaceAll]);
  Result := ExpandFileName(IncludeTrailingPathDelimiter(FRoot) + lRel);
end;


procedure TFpcSourceTest.RemoveTree(const aDir: string);

var
  lRec: TSearchRec;
  lFull: string;

begin
  if not DirectoryExists(aDir) then
    Exit;
  if FindFirst(IncludeTrailingPathDelimiter(aDir) + '*', faAnyFile, lRec) = 0 then
    try
      repeat
        if (lRec.Name = '.') or (lRec.Name = '..') then
          Continue;
        lFull := IncludeTrailingPathDelimiter(aDir) + lRec.Name;
        if (lRec.Attr and faDirectory) <> 0 then
          RemoveTree(lFull)
        else
          DeleteFile(lFull);
      until FindNext(lRec) <> 0;
    finally
      FindClose(lRec);
    end;
  RemoveDir(aDir);
end;


procedure TFpcSourceTest.WriteStub(const aPath: string);

var
  lh: THandle;

begin
  lh := FileCreate(aPath);
  if lh <> THandle(-1) then
    FileClose(lh);
end;


procedure TFpcSourceTest.SetUp;

begin
  FRoot := IncludeTrailingPathDelimiter(GetTempDir) + 'fpsonar_fpcsrc_test';
  RemoveTree(FRoot);  // clear any leftover from a crashed run
  // The synthetic linux/x86_64 source layout (the golden dirs).
  ForceDirectories(AbsDir('rtl/inc'));
  ForceDirectories(AbsDir('rtl/objpas'));
  ForceDirectories(AbsDir('rtl/objpas/sysutils'));
  ForceDirectories(AbsDir('rtl/objpas/classes'));
  ForceDirectories(AbsDir('rtl/linux'));
  ForceDirectories(AbsDir('rtl/linux/x86_64'));
  ForceDirectories(AbsDir('rtl/unix'));
  ForceDirectories(AbsDir('rtl/x86_64'));
  WriteStub(AbsDir('rtl/inc/system.inc'));  // the IsFpcSourceTree marker
end;


procedure TFpcSourceTest.TearDown;

begin
  RemoveTree(FRoot);
end;


procedure TFpcSourceTest.LayoutSplitMatchesLinuxX8664Golden;

var
  lCfg: TFpSonarFpcSourceConfig;
  lUnits, lIncludes: TFpSonarStringArray;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  ExpandFpcSourceLayout(FRoot, 'linux', 'x86_64',
    lCfg.UnitDirTemplates, lCfg.IncludeDirTemplates, lUnits, lIncludes);

  // -Fu (units) — kept distinct from includes; template order, only-existing.
  AssertArray('unit dirs (-Fu)',
    [AbsDir('rtl/linux'), AbsDir('rtl/objpas'), AbsDir('rtl/objpas/sysutils'),
     AbsDir('rtl/objpas/classes')], lUnits);

  // -Fi (includes) — incl. the per-CPU include dir rtl/linux/x86_64 (wall 14).
  AssertArray('include dirs (-Fi)',
    [AbsDir('rtl/inc'), AbsDir('rtl/x86_64'), AbsDir('rtl/unix'), AbsDir('rtl/linux'),
     AbsDir('rtl/objpas'), AbsDir('rtl/linux/x86_64')], lIncludes);

  // The per-CPU include dir must be present and absolute.
  AssertContains('per-CPU include dir present', AbsDir('rtl/linux/x86_64'), lIncludes);
  AssertTrue('emitted paths are absolute',
    (Length(lUnits) > 0) and (lUnits[0] = ExpandFileName(lUnits[0])));
end;


procedure TFpcSourceTest.LayoutEmitsOnlyExistingDirs;

var
  lCfg: TFpSonarFpcSourceConfig;
  lUnits, lIncludes: TFpSonarStringArray;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  // Target arm: rtl/arm and rtl/linux/arm do NOT exist in the synthetic tree, so
  // the per-CPU dirs are omitted (NEVER fabricated).
  ExpandFpcSourceLayout(FRoot, 'linux', 'arm',
    lCfg.UnitDirTemplates, lCfg.IncludeDirTemplates, lUnits, lIncludes);
  AssertNotContains('absent per-CPU unit dir omitted', AbsDir('rtl/arm'), lIncludes);
  AssertNotContains('absent per-CPU include dir omitted',
    AbsDir('rtl/linux/arm'), lIncludes);
  // The OS-keyed dirs that DO exist are still emitted.
  AssertContains('existing OS include dir emitted', AbsDir('rtl/linux'), lIncludes);
end;


procedure TFpcSourceTest.LayoutEmptyRootYieldsEmpty;

var
  lCfg: TFpSonarFpcSourceConfig;
  lUnits, lIncludes: TFpSonarStringArray;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  ExpandFpcSourceLayout('', 'linux', 'x86_64',
    lCfg.UnitDirTemplates, lCfg.IncludeDirTemplates, lUnits, lIncludes);
  AssertEquals('empty root => no unit dirs', 0, Length(lUnits));
  AssertEquals('empty root => no include dirs', 0, Length(lIncludes));
end;


procedure TFpcSourceTest.DefinesContainOsCpuFeaturesAndObjpas;

var
  lCfg: TFpSonarFpcSourceConfig;
  lDefs: TFpSonarStringArray;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  lDefs := lCfg.RtlSourceDefines;
  // From EffectiveDefines (reused, not re-derived):
  AssertContains('FPC define', 'FPC', lDefs);
  AssertContains('CPU define', 'CPUX86_64', lDefs);
  AssertContains('UNIX define', 'UNIX', lDefs);
  AssertContains('LINUX define', 'LINUX', lDefs);
  // The FPC_HAS_FEATURE_* family (sample) + the implicit objpas marker:
  AssertContains('feature define', 'FPC_HAS_FEATURE_HEAP', lDefs);
  AssertContains('feature define', 'FPC_HAS_FEATURE_UNICODESTRINGS', lDefs);
  AssertContains('implicit objpas marker', 'objpas', lDefs);
end;


procedure TFpcSourceTest.DefaultConfigDefinesAreBuiltInConstant;

var
  lCfg: TFpSonarFpcSourceConfig;
  lDefs: TFpSonarStringArray;
  i: Integer;

begin
  // An unconfigured run is byte-identical to the built-in constant default.
  lCfg := TFpSonarFpcSourceConfig.Default;
  lDefs := lCfg.RtlSourceDefines;
  for i := Low(cFpcFeatureDefines) to High(cFpcFeatureDefines) do
    AssertContains('default carries built-in feature constant',
      cFpcFeatureDefines[i], lDefs);
  AssertEquals('default feature set length matches the constant',
    Length(cFpcFeatureDefines), Length(lCfg.FeatureDefines));
end;


procedure TFpcSourceTest.FeatureDefineOverrideWins;

var
  lCfg: TFpSonarFpcSourceConfig;
  lDefs: TFpSonarStringArray;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  SetLength(lCfg.FeatureDefines, 1);
  lCfg.FeatureDefines[0] := 'FPC_HAS_FEATURE_HEAP';
  lDefs := lCfg.RtlSourceDefines;
  // Override replaces the family: only the one feature survives.
  AssertContains('override feature present', 'FPC_HAS_FEATURE_HEAP', lDefs);
  AssertNotContains('non-overridden feature absent',
    'FPC_HAS_FEATURE_CLASSES', lDefs);
  // OS/CPU defines + objpas are still produced.
  AssertContains('OS/CPU still derived', 'LINUX', lDefs);
  AssertContains('objpas still appended', 'objpas', lDefs);
end;


procedure TFpcSourceTest.EmptyFeatureOverrideFallsBackToBuiltIn;

var
  lCfg: TFpSonarFpcSourceConfig;
  lDefs: TFpSonarStringArray;
  i: Integer;

begin
  // An EMPTY feature-override array means "use the built-in constant
  // default", NOT "emit no feature defines" (the record's documented seam).
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  SetLength(lCfg.FeatureDefines, 0);
  lDefs := lCfg.RtlSourceDefines;
  for i := Low(cFpcFeatureDefines) to High(cFpcFeatureDefines) do
    AssertContains('empty feature override falls back to constant',
      cFpcFeatureDefines[i], lDefs);
  AssertContains('objpas still appended', 'objpas', lDefs);
end;


procedure TFpcSourceTest.EmptyTargetFallsBackToHost;

var
  lCfg: TFpSonarFpcSourceConfig;
  lDefault, lEmpty: TFpSonarStringArray;

begin
  // An unset TargetCPU/OS keeps the host default rather than clobbering it: the
  // define set is element-identical to the fully-defaulted (host) config's.
  lDefault := TFpSonarFpcSourceConfig.Default.RtlSourceDefines;
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.TargetCPU := '';
  lCfg.TargetOS := '';
  lEmpty := lCfg.RtlSourceDefines;
  AssertArray('empty target == host-defaulted target', lDefault, lEmpty);
end;


procedure TFpcSourceTest.EmptyTemplatesFallBackToBuiltIn;

var
  lCfg: TFpSonarFpcSourceConfig;
  lAnalysis: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;

begin
  // Empty template overrides fall back to the built-in constants in the assembly
  // entry point, so the full golden layout is still produced.
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.SourceDir := FRoot;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  SetLength(lCfg.UnitDirTemplates, 0);
  SetLength(lCfg.IncludeDirTemplates, 0);
  AssertTrue('located the valid tree',
    lCfg.BuildRtlSourceConfig(lAnalysis, lDiag));
  AssertEquals('empty unit templates fall back to built-in',
    4, Length(lAnalysis.UnitSearchPaths));
  AssertEquals('empty include templates fall back to built-in',
    6, Length(lAnalysis.IncludePaths));
  AssertContains('per-CPU include dir still present',
    AbsDir('rtl/linux/x86_64'), lAnalysis.IncludePaths);
end;


procedure TFpcSourceTest.TemplateOverrideWins;

var
  lCfg: TFpSonarFpcSourceConfig;
  lUnits, lIncludes: TFpSonarStringArray;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  // Override the unit templates to a single existing dir.
  SetLength(lCfg.UnitDirTemplates, 1);
  lCfg.UnitDirTemplates[0] := 'rtl/objpas';
  ExpandFpcSourceLayout(FRoot, 'linux', 'x86_64',
    lCfg.UnitDirTemplates, lCfg.IncludeDirTemplates, lUnits, lIncludes);
  AssertArray('overridden unit templates', [AbsDir('rtl/objpas')], lUnits);
end;


procedure TFpcSourceTest.InvalidRootDegradesToDiagnostic;

var
  lDiag: TFpSonarDiagnostic;
  lResult: string;

begin
  // An explicit-but-invalid override degrades (no fall-through to the host) and
  // NEVER raises.
  lResult := LocateFpcSourceDir(
    IncludeTrailingPathDelimiter(GetTempDir) + 'fpsonar_nope_xyz', lDiag);
  AssertEquals('invalid root => empty result', '', lResult);
  AssertTrue('diagnostic message set', lDiag.Message <> '');
  AssertEquals('diagnostic kind', Ord(dkParseError), Ord(lDiag.Kind));
end;


procedure TFpcSourceTest.BuildRtlSourceConfigOnValidTree;

var
  lCfg: TFpSonarFpcSourceConfig;
  lAnalysis: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.SourceDir := FRoot;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  AssertTrue('located the valid tree',
    lCfg.BuildRtlSourceConfig(lAnalysis, lDiag));
  AssertEquals('TargetCPU carried', 'x86_64', lAnalysis.TargetCPU);
  AssertEquals('TargetOS carried', 'linux', lAnalysis.TargetOS);
  AssertEquals('unit search paths populated', 4, Length(lAnalysis.UnitSearchPaths));
  AssertEquals('include paths populated', 6, Length(lAnalysis.IncludePaths));
  AssertContains('per-CPU include dir present',
    AbsDir('rtl/linux/x86_64'), lAnalysis.IncludePaths);
  AssertContains('defines carry objpas', 'objpas', lAnalysis.Defines);
end;


procedure TFpcSourceTest.BuildRtlSourceConfigDegradesOnMissingTree;

var
  lCfg: TFpSonarFpcSourceConfig;
  lAnalysis: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.SourceDir := IncludeTrailingPathDelimiter(GetTempDir) + 'fpsonar_absent_qq';
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  AssertFalse('missing tree => not located',
    lCfg.BuildRtlSourceConfig(lAnalysis, lDiag));
  AssertEquals('no unit paths fabricated', 0, Length(lAnalysis.UnitSearchPaths));
  AssertEquals('no include paths fabricated', 0, Length(lAnalysis.IncludePaths));
  AssertTrue('diagnostic message set', lDiag.Message <> '');
  // Defines are still produced (config, not disk-dependent).
  AssertContains('defines still produced', 'objpas', lAnalysis.Defines);
end;


procedure TFpcSourceTest.DiscoveryIsDeterministic;

var
  lCfg: TFpSonarFpcSourceConfig;
  lA, lB: TFpSonarStringArray;
  lUA, lIA, lUB, lIB: TFpSonarStringArray;
  i: Integer;

begin
  lCfg := TFpSonarFpcSourceConfig.Default;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  lA := lCfg.RtlSourceDefines;
  lB := lCfg.RtlSourceDefines;
  AssertEquals('defines same length across runs', Length(lA), Length(lB));
  for i := 0 to High(lA) do
    AssertEquals('same define at ' + IntToStr(i), lA[i], lB[i]);

  ExpandFpcSourceLayout(FRoot, 'linux', 'x86_64',
    lCfg.UnitDirTemplates, lCfg.IncludeDirTemplates, lUA, lIA);
  ExpandFpcSourceLayout(FRoot, 'linux', 'x86_64',
    lCfg.UnitDirTemplates, lCfg.IncludeDirTemplates, lUB, lIB);
  AssertArray('unit dirs deterministic', lUA, lUB);
  AssertArray('include dirs deterministic', lIA, lIB);
end;


initialization
  RegisterTest(TFpcSourceTest);

end.
