{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the config-driven disallow-list tracker rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesTrackers;

{ The config-driven TRACKER rule tests: the three disallow-list rules }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.Config, FpSonar.RuleFramework,
  FpSonar.Rules.Trackers, UtstFixtures;

type
  { SEM-tier config-driven tracker-rule tests. }
  TTrackersRulesTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture
    // with aConfig threaded onto the engine; issues land in aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);
    // As RunRule, but the fixture source is supplied inline (one array element
    // per source line) and materialised to a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    // As RunRuleSrc for the cross-unit import fixture: writes the sibling
    // 'bannedunit' alongside 'noncompliant.pas' so it resolves cross-unit.
    procedure RunImportSrc(aRule: TRuleBase; const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // A config carrying rules.<aRuleId>.params.targets with one target per
    // pattern (no message, default severity).
    function TargetsConfig(const aRuleId: string;
      const aPatterns: array of string): TFpSonarConfig;
    // A DisallowedIdentifier config carrying both targets and the
    // matchUnresolvedByName bool (the two-mode switch).
    function IdentifierConfig(const aPatterns: array of string;
      aByName: Boolean): TFpSonarConfig;
    // Fresh, separately-owned rule instances (metadata mirrors the unit's
    // self-registration, including the declared 'targets' param).
    function NewDisallowedImportByPath: TRuleBase;
    function NewDisallowedConstant: TRuleBase;
    function NewDisallowedEnumValue: TRuleBase;
    function NewDisallowedField: TRuleBase;
    function NewDisallowedIdentifier: TRuleBase;
    function NewDisallowedProperty: TRuleBase;
    function NewDisallowedRoutine: TRuleBase;
    function NewDisallowedType: TRuleBase;
    function NewTrackTypeAliases: TRuleBase;
  published
    procedure DisallowedImportByPathFires;
    procedure DisallowedImportByPathExcludesSynthetic;
    procedure DisallowedConstantFires;
    procedure DisallowedConstantQualifiedMatches;
    procedure DisallowedConstantResolvedOnly;
    procedure DisallowedEnumValueFires;
    procedure DisallowedEnumValueQualifiedMatches;
    procedure DisallowedEnumValueCompliant;
    procedure DisallowedFieldFires;
    procedure DisallowedFieldQualifiedMatches;
    procedure DisallowedFieldResolvedOnly;
    procedure DisallowedFieldExcludesProperty;
    procedure DisallowedFieldExcludesClassConst;
    procedure DisallowedPropertyFires;
    procedure DisallowedPropertyQualifiedMatches;
    procedure DisallowedPropertyExcludesField;
    procedure DisallowedIdentifierResolvedMode;
    procedure DisallowedIdentifierNameMode;
    procedure DisallowedIdentifierNameModeCountsEachLeaf;
    procedure DisallowedFieldPerTargetSeverity;
    procedure DisallowedRoutineFires;
    procedure DisallowedRoutineQualifiedMatches;
    procedure DisallowedRoutineResolvedOnly;
    procedure DisallowedTypeFires;
    procedure DisallowedTypeQualifiedMatches;
    procedure DisallowedTypeCompliant;
    procedure RoutineTypePartition;
    procedure TrackTypeAliasesFires;
    procedure TrackTypeAliasesWildcardReportsAll;
    procedure TrackTypeAliasesExcludesNonAlias;
    procedure TrackTypeAliasesPerTargetSeverity;
    procedure UnconfiguredRulesAreSilent;
    procedure PerTargetSeverityOverridesDefault;
    procedure TrackersSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cImportId = 'DisallowedImportByPath';
  cConstId = 'DisallowedConstant';
  cEnumId = 'DisallowedEnumValue';
  cFieldId = 'DisallowedField';
  cIdentId = 'DisallowedIdentifier';
  cPropId = 'DisallowedProperty';
  cRoutineId = 'DisallowedRoutine';
  cTypeId = 'DisallowedType';
  cAliasId = 'TrackTypeAliases';

  // Embedded tracker-rule fixtures (Approach A rollout): line i+1 == [i].

  cDisallowedConstantCompliant: array[0..20] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function GetIt: Integer;',
    '',
    'implementation',
    '',
    'function GetIt: Integer;',
    '',
    'var',
    '  Forbidden: Integer;',
    '',
    'begin',
    '  Forbidden := 7;',
    '  Result := Forbidden;',
    'end;',
    '',
    'end.');

  cDisallowedConstantNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  Forbidden = 42;',
    '',
    'function GetIt: Integer;',
    '',
    'implementation',
    '',
    'function GetIt: Integer;',
    '',
    'begin',
    '  Result := Forbidden;',
    'end;',
    '',
    'end.');

  cDisallowedEnumValueCompliant: array[0..19] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TColor = (Red, Green, Blue);',
    '',
    'function GetIt: TColor;',
    '',
    'implementation',
    '',
    'function GetIt: TColor;',
    '',
    'begin',
    '  Result := Green;',
    'end;',
    '',
    'end.');

  cDisallowedEnumValueNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TColor = (Red, Green, Blue);',
    '',
    'function GetIt: TColor;',
    '',
    'implementation',
    '',
    'function GetIt: TColor;',
    '',
    'begin',
    '  Result := Red;',
    'end;',
    '',
    'end.');

  cDisallowedFieldClassconst: array[0..21] of string = (
    'unit classconst;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  public',
    '    const Forbidden = 5;',
    '    function GetIt: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TThing.GetIt: Integer;',
    '',
    'begin',
    '  Result := Forbidden;',
    'end;',
    '',
    'end.');

  cDisallowedFieldCompliant: array[0..20] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function GetIt: Integer;',
    '',
    'implementation',
    '',
    'function GetIt: Integer;',
    '',
    'var',
    '  FBanned: Integer;',
    '',
    'begin',
    '  FBanned := 7;',
    '  Result := FBanned;',
    'end;',
    '',
    'end.');

  cDisallowedFieldNoncompliant: array[0..22] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FBanned: Integer;',
    '  public',
    '    function GetIt: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TThing.GetIt: Integer;',
    '',
    'begin',
    '  Result := FBanned;',
    'end;',
    '',
    'end.');

  cDisallowedIdentifierCompliant: array[0..19] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  Allowed = 1;',
    '',
    'function GetIt: Integer;',
    '',
    'implementation',
    '',
    'function GetIt: Integer;',
    '',
    'begin',
    '  Result := Allowed;',
    'end;',
    '',
    'end.');

  cDisallowedIdentifierNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  Banned = 42;',
    '',
    'function GetIt: Integer;',
    '',
    'implementation',
    '',
    'function GetIt: Integer;',
    '',
    'begin',
    '  Result := Banned;',
    'end;',
    '',
    'end.');

  cDisallowedIdentifierTwice: array[0..19] of string = (
    'unit twice;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  Banned = 42;',
    '',
    'function GetIt: Integer;',
    '',
    'implementation',
    '',
    'function GetIt: Integer;',
    '',
    'begin',
    '  Result := Banned + Banned;',
    'end;',
    '',
    'end.');

  cDisallowedImportByPathBannedunit: array[0..15] of string = (
    'unit bannedunit;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Nothing;',
    '',
    'implementation',
    '',
    'procedure Nothing;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cDisallowedImportByPathCompliant: array[0..19] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'function GetIt: string;',
    '',
    'implementation',
    '',
    'function GetIt: string;',
    '',
    'begin',
    '  Result := IntToStr(1);',
    'end;',
    '',
    'end.');

  cDisallowedImportByPathNoncompliant: array[0..19] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  bannedunit;',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  Nothing;',
    'end;',
    '',
    'end.');

  cDisallowedPropertyCompliant: array[0..22] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    Banned: Integer;',
    '  public',
    '    function GetIt: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TThing.GetIt: Integer;',
    '',
    'begin',
    '  Result := Banned;',
    'end;',
    '',
    'end.');

  cDisallowedPropertyNoncompliant: array[0..23] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    property Banned: Integer read FValue write FValue;',
    '    function GetIt: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TThing.GetIt: Integer;',
    '',
    'begin',
    '  Result := Banned;',
    'end;',
    '',
    'end.');

  cDisallowedRoutineCompliant: array[0..29] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure DoIt;',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    '',
    'var',
    '  Banned: Integer;',
    '',
    'begin',
    '  Banned := 7;',
    '  if Banned > 0 then',
    '    Banned := 0;',
    'end;',
    '',
    '// A routine reference (DoIt) so the resolved-only test can prove this fixture',
    '// actually resolves (a positive control against the silent-skip false-green).',
    'procedure Probe;',
    '',
    'begin',
    '  DoIt;',
    'end;',
    '',
    'end.');

  cDisallowedRoutineNoncompliant: array[0..37] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  public',
    '    procedure Banned;',
    '  end;',
    '',
    'procedure Banned;',
    'procedure DoIt;',
    '',
    'implementation',
    '',
    'procedure Banned;',
    '',
    'begin',
    'end;',
    '',
    'procedure TThing.Banned;',
    '',
    'begin',
    'end;',
    '',
    'procedure DoIt;',
    '',
    'var',
    '  lThing: TThing;',
    '',
    'begin',
    '  Banned;',
    '  lThing.Banned;',
    'end;',
    '',
    'end.');

  cDisallowedTypeCompliant: array[0..28] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBanned = class',
    '  end;',
    '  TOther = class',
    '  end;',
    '',
    'procedure DoIt;',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    '',
    'var',
    '  lObj: TObject;',
    '  lOther: TOther;',
    '',
    'begin',
    '  lOther := TOther(lObj);',
    '  if lOther = nil then',
    '    lObj := nil;',
    'end;',
    '',
    'end.');

  cDisallowedTypeNoncompliant: array[0..30] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBanned = class',
    '  end;',
    '  TOther = class',
    '  end;',
    '',
    'procedure DoIt;',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    '',
    'var',
    '  lObj: TObject;',
    '  lBanned: TBanned;',
    '  lIsIt: Boolean;',
    '',
    'begin',
    '  lBanned := TBanned(lObj);',
    '  lIsIt := lObj is TBanned;',
    '  if lBanned = nil then',
    '    lIsIt := False;',
    'end;',
    '',
    'end.');

  cTrackTypeAliasesCompliant: array[0..15] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPlain = class',
    '  end;',
    '  TRec = record',
    '    X: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cTrackTypeAliasesNoncompliant: array[0..20] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class',
    '  end;',
    '  TBannedAlias = TBase;',
    '  TIntAlias = Integer;',
    '  TStrong = type Integer;',
    '  TPlain = class',
    '  end;',
    '  TRec = record',
    '    X: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

procedure TTrackersRulesTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lDir: string;

begin
  lDir := ExtractFilePath(aFixture);
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := aConfig;
    lEngine.Analyze(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
      [lDir], [], aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TTrackersRulesTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aConfig: TFpSonarConfig;
  const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  // Materialise the inline fixture (one array element per source line) to a
  // temp dir, run with aConfig, and delete.
  lFix := TTempFixtures.Create;
  try
    RunRule(aRule, lFix.Add(aName, aSrc), aConfig, aCollector);
  finally
    lFix.Free;
  end;
end;


procedure TTrackersRulesTest.RunImportSrc(aRule: TRuleBase;
  const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  // The import fixture 'uses bannedunit'; the sibling must be present in the
  // search dir for cross-unit resolution (else the rfResolver feed is absent).
  lFix := TTempFixtures.Create;
  try
    lFix.Add('bannedunit.pas', cDisallowedImportByPathBannedunit);
    RunRule(aRule,
      lFix.Add('noncompliant.pas', cDisallowedImportByPathNoncompliant),
      aConfig, aCollector);
  finally
    lFix.Free;
  end;
end;


function TTrackersRulesTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TTrackersRulesTest.FirstById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

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


function TTrackersRulesTest.TargetsConfig(const aRuleId: string;
  const aPatterns: array of string): TFpSonarConfig;

var
  lSetting: TFpSonarRuleSetting;
  lParam: TFpSonarRuleParam;
  i: Integer;

begin
  Result := TFpSonarConfig.Default;
  lSetting.RuleId := aRuleId;
  lSetting.HasEnabled := False;
  lSetting.Enabled := True;
  lSetting.HasSeverity := False;
  lSetting.Severity := sevInfo;
  SetLength(lSetting.Params, 1);
  lParam.Key := 'targets';
  lParam.Kind := cpkTargets;
  lParam.IntVal := 0;
  lParam.StrVal := '';
  lParam.BoolVal := False;
  SetLength(lParam.Targets, Length(aPatterns));
  for i := Low(aPatterns) to High(aPatterns) do
    begin
      lParam.Targets[i].Pattern := aPatterns[i];
      lParam.Targets[i].Message := '';
      lParam.Targets[i].Severity := sevInfo;
      lParam.Targets[i].HasSeverity := False;
    end;
  lSetting.Params[0] := lParam;
  SetLength(Result.Rules, 1);
  Result.Rules[0] := lSetting;
end;


function TTrackersRulesTest.IdentifierConfig(
  const aPatterns: array of string; aByName: Boolean): TFpSonarConfig;

var
  lBool: TFpSonarRuleParam;

begin
  // Start from the standard targets config, then append the bool mode switch.
  Result := TargetsConfig(cIdentId, aPatterns);
  lBool.Key := 'matchUnresolvedByName';
  lBool.Kind := cpkBool;
  lBool.IntVal := 0;
  lBool.StrVal := '';
  lBool.BoolVal := aByName;
  SetLength(lBool.Targets, 0);
  SetLength(Result.Rules[0].Params, 2);
  Result.Rules[0].Params[1] := lBool;
end;


function TTrackersRulesTest.NewDisallowedImportByPath: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cImportId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedImportByPath.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedConstant: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cConstId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedConstant.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedEnumValue: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cEnumId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedEnumValue.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedField: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cFieldId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedField.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedIdentifier: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  // DisallowedIdentifier declares BOTH params, mirroring the unit's IdentifierMeta.
  lMeta := TRuleMetadata.Make(cIdentId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  lMeta.AddParam('matchUnresolvedByName', rpkBool);
  Result := TRuleDisallowedIdentifier.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedProperty: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cPropId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedProperty.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedRoutine: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cRoutineId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedRoutine.Create(lMeta);
end;


function TTrackersRulesTest.NewDisallowedType: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cTypeId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleDisallowedType.Create(lMeta);
end;


function TTrackersRulesTest.NewTrackTypeAliases: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  // TrackTypeAliases is the AST-tier exception: rtAst / rfAst (the engine feeds it the parsed
  // Module without a resolver), single 'targets' param like the SEM trackers.
  lMeta := TRuleMetadata.Make(cAliasId, rtAst, rfAst, sevMajor, itCodeSmell,
    cfHigh, True, '');
  lMeta.AddParam('targets', rpkTargets);
  Result := TRuleTypeAliases.Create(lMeta);
end;


procedure TTrackersRulesTest.DisallowedImportByPathFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The sibling 'bannedunit' resolves to a real path; a path glob matches it -> one
  // issue at the 'uses' entry row (8, probe-locked), arg[0] = the unit name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunImportSrc(NewDisallowedImportByPath, TargetsConfig(cImportId, ['*bannedunit.pas']), lc);
    AssertEquals('one import issue', 1, CountById(lc, cImportId));
    k := FirstById(lc, cImportId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('two args (symbol + message)', 2,
      Length(lc.Issues[k].MessageArgs));
    AssertEquals('arg is the unit name', 'bannedunit',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedImportByPathExcludesSynthetic;

var
  lc: TFpSonarIssueCollector;

begin
  // The compliant fixture imports the synthetic-RTL SysUtils only; its path is
  // never located, so even a match-all glob produces zero (synthetic
  // exclusion) — and the fixture resolves clean (silent-skip canary).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedImportByPath, 'compliant.pas', cDisallowedImportByPathCompliant,
      TargetsConfig(cImportId, ['*']), lc);
    AssertEquals('synthetic import never matched', 0, CountById(lc, cImportId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedConstantFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A reference resolving to the disallowed const -> one issue at the reference row
  // (17, probe-locked), arg[0] = the bare const name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'noncompliant.pas', cDisallowedConstantNoncompliant,
      TargetsConfig(cConstId, ['Forbidden']), lc);
    AssertEquals('one const issue', 1, CountById(lc, cConstId));
    k := FirstById(lc, cConstId);
    AssertEquals('start line', 17, lc.Issues[k].StartLine);
    AssertEquals('arg is the const name', 'Forbidden',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedConstantQualifiedMatches;

var
  lc: TFpSonarIssueCollector;

begin
  // A Unit-qualified glob also matches the same reference (one issue).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'noncompliant.pas', cDisallowedConstantNoncompliant,
      TargetsConfig(cConstId, ['*.Forbidden']), lc);
    AssertEquals('qualified glob matches', 1, CountById(lc, cConstId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedConstantResolvedOnly;

var
  lc: TFpSonarIssueCollector;

begin
  // The compliant fixture has a LOCAL variable named Forbidden, not the const; the
  // reference resolves to the variable, so the const rule never matches (resolved
  // identity, not spelling) -> zero.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'compliant.pas', cDisallowedConstantCompliant,
      TargetsConfig(cConstId, ['Forbidden']), lc);
    AssertEquals('local var of same name not matched', 0,
      CountById(lc, cConstId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedEnumValueFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A reference resolving to the disallowed enum value -> one issue at the
  // reference row (17, probe-locked), arg[0] = the bare value name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedEnumValue, 'noncompliant.pas', cDisallowedEnumValueNoncompliant,
      TargetsConfig(cEnumId, ['Red']), lc);
    AssertEquals('one enum issue', 1, CountById(lc, cEnumId));
    k := FirstById(lc, cEnumId);
    AssertEquals('start line', 17, lc.Issues[k].StartLine);
    AssertEquals('arg is the value name', 'Red', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedEnumValueQualifiedMatches;

var
  lc: TFpSonarIssueCollector;

begin
  // The EnumType.Value qualified form matches (probe-locked: parent enum type is
  // named TColor).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedEnumValue, 'noncompliant.pas', cDisallowedEnumValueNoncompliant,
      TargetsConfig(cEnumId, ['TColor.Red']), lc);
    AssertEquals('enum-type-qualified glob matches', 1, CountById(lc, cEnumId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedEnumValueCompliant;

var
  lc: TFpSonarIssueCollector;

begin
  // The compliant fixture references Green; a target of Red does not match -> zero
  // (and the fixture resolves clean).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedEnumValue, 'compliant.pas', cDisallowedEnumValueCompliant,
      TargetsConfig(cEnumId, ['Red']), lc);
    AssertEquals('non-matching value silent', 0, CountById(lc, cEnumId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedFieldFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A reference resolving to the disallowed member field FBanned -> one issue at the
  // reference row (20, probe-locked), arg[0] = the bare field name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      TargetsConfig(cFieldId, ['FBanned']), lc);
    AssertEquals('one field issue', 1, CountById(lc, cFieldId));
    k := FirstById(lc, cFieldId);
    AssertEquals('start line', 20, lc.Issues[k].StartLine);
    AssertEquals('arg is the field name', 'FBanned',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedFieldQualifiedMatches;

var
  lc: TFpSonarIssueCollector;

begin
  // The OwnerType-qualified form (probe-locked owner TThing) and the Unit-qualified
  // glob both match the same reference.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      TargetsConfig(cFieldId, ['TThing.FBanned']), lc);
    AssertEquals('owner-qualified matches', 1, CountById(lc, cFieldId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      TargetsConfig(cFieldId, ['*.FBanned']), lc);
    AssertEquals('unit-qualified glob matches', 1, CountById(lc, cFieldId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedFieldResolvedOnly;

var
  lc: TFpSonarIssueCollector;

begin
  // The compliant fixture has a LOCAL variable named FBanned (Parent is a block,
  // not a members type); the reference resolves to it, so the field rule never
  // matches (resolved identity + member filter, not spelling) -> zero.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'compliant.pas', cDisallowedFieldCompliant,
      TargetsConfig(cFieldId, ['FBanned']), lc);
    AssertEquals('local var of same name not matched', 0,
      CountById(lc, cFieldId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedFieldExcludesProperty;

var
  lc: TFpSonarIssueCollector;

begin
  // The property fixture's Banned reference resolves to a TPasProperty, which the
  // field filter excludes (TPasProperty descends TPasVariable) -> zero (partition
  // with the property rule).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedPropertyNoncompliant,
      TargetsConfig(cFieldId, ['Banned']), lc);
    AssertEquals('property not matched by field rule', 0,
      CountById(lc, cFieldId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedFieldExcludesClassConst;

var
  lc: TFpSonarIssueCollector;

begin
  // A CLASS const (Parent IS a members type) is the one case where the 'not
  // TPasConst' arm of the field partition actually bears load: the Parent-is-member
  // check alone would NOT exclude it (a section-level const is excluded by Parent
  // anyway). The const rule firing on the same fixture/target proves the const resolves
  // (no silent-skip false-green); the field rule staying silent proves the const arm.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'classconst.pas', cDisallowedFieldClassconst,
      TargetsConfig(cConstId, ['Forbidden']), lc);
    AssertEquals('class const resolves + fires under #122', 1,
      CountById(lc, cConstId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'classconst.pas', cDisallowedFieldClassconst,
      TargetsConfig(cFieldId, ['Forbidden']), lc);
    AssertEquals('class const not matched by field rule', 0,
      CountById(lc, cFieldId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedPropertyFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A reference resolving to the disallowed property Banned -> one issue at the
  // reference row (21, probe-locked), arg[0] = the bare property name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedProperty, 'noncompliant.pas', cDisallowedPropertyNoncompliant,
      TargetsConfig(cPropId, ['Banned']), lc);
    AssertEquals('one property issue', 1, CountById(lc, cPropId));
    k := FirstById(lc, cPropId);
    AssertEquals('start line', 21, lc.Issues[k].StartLine);
    AssertEquals('arg is the property name', 'Banned',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedPropertyQualifiedMatches;

var
  lc: TFpSonarIssueCollector;

begin
  // The OwnerType-qualified form (probe-locked owner TThing) matches.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedProperty, 'noncompliant.pas', cDisallowedPropertyNoncompliant,
      TargetsConfig(cPropId, ['TThing.Banned']), lc);
    AssertEquals('owner-qualified matches', 1, CountById(lc, cPropId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedPropertyExcludesField;

var
  lc: TFpSonarIssueCollector;

begin
  // The field fixture's FBanned reference resolves to a plain member TPasVariable,
  // not a property -> zero (partition with the field rule).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedProperty, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      TargetsConfig(cPropId, ['FBanned']), lc);
    AssertEquals('field not matched by property rule', 0,
      CountById(lc, cPropId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedIdentifierResolvedMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Resolved mode (matchUnresolvedByName=false): the reference resolves to the const
  // Banned -> one issue at row 17 (probe-locked), arg[0] = resolved decl name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedIdentifier, 'noncompliant.pas', cDisallowedIdentifierNoncompliant,
      IdentifierConfig(['Banned'], False), lc);
    AssertEquals('one identifier issue', 1, CountById(lc, cIdentId));
    k := FirstById(lc, cIdentId);
    AssertEquals('start line', 17, lc.Issues[k].StartLine);
    AssertEquals('arg is the resolved decl name', 'Banned',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
  // A different resolved name does not match.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedIdentifier, 'noncompliant.pas', cDisallowedIdentifierNoncompliant,
      IdentifierConfig(['Nope'], False), lc);
    AssertEquals('non-matching name silent', 0, CountById(lc, cIdentId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedIdentifierNameMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Name mode (matchUnresolvedByName=true): the identifier leaf Banned appears once
  // in expression position -> one issue at row 17 (probe-locked), arg[0] = the
  // textual spelling.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedIdentifier, 'noncompliant.pas', cDisallowedIdentifierNoncompliant,
      IdentifierConfig(['Banned'], True), lc);
    AssertEquals('one identifier issue (name mode)', 1, CountById(lc, cIdentId));
    k := FirstById(lc, cIdentId);
    AssertEquals('start line', 17, lc.Issues[k].StartLine);
    AssertEquals('arg is the spelling', 'Banned',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedIdentifierNameModeCountsEachLeaf;

var
  lc: TFpSonarIssueCollector;

begin
  // Name mode is per-LEAF: the spelling Banned appears twice in expression position
  // (Banned + Banned) -> two issues. This is the behaviour that distinguishes name
  // mode (one issue per identifier leaf) from a single resolved-decl match.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedIdentifier, 'twice.pas', cDisallowedIdentifierTwice,
      IdentifierConfig(['Banned'], True), lc);
    AssertEquals('one issue per identifier leaf', 2, CountById(lc, cIdentId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedFieldPerTargetSeverity;

var
  lc: TFpSonarIssueCollector;
  lCfg: TFpSonarConfig;
  k: Integer;

begin
  // A target WITHOUT a severity uses the rule default (Major).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      TargetsConfig(cFieldId, ['FBanned']), lc);
    k := FirstById(lc, cFieldId);
    AssertTrue('default severity is major', lc.Issues[k].Severity = sevMajor);
  finally
    lc.Free;
  end;
  // A target WITH a severity overrides it (Minor).
  lCfg := TargetsConfig(cFieldId, ['FBanned']);
  lCfg.Rules[0].Params[0].Targets[0].Severity := sevMinor;
  lCfg.Rules[0].Params[0].Targets[0].HasSeverity := True;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      lCfg, lc);
    k := FirstById(lc, cFieldId);
    AssertTrue('per-target severity wins', lc.Issues[k].Severity = sevMinor);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedRoutineFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A free routine Banned (row 34) AND a method TThing.Banned (row 35) are both
  // called -> target Banned matches both (probe-locked), two issues; arg[0] is the
  // bare routine name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'noncompliant.pas', cDisallowedRoutineNoncompliant,
      TargetsConfig(cRoutineId, ['Banned']), lc);
    AssertEquals('two routine issues (free + method)', 2,
      CountById(lc, cRoutineId));
    k := FirstById(lc, cRoutineId);
    AssertEquals('first start line', 34, lc.Issues[k].StartLine);
    AssertEquals('arg is the routine name', 'Banned',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedRoutineQualifiedMatches;

var
  lc: TFpSonarIssueCollector;

begin
  // The OwnerType-qualified form matches the METHOD only (probe-locked owner
  // TThing) -> one issue.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'noncompliant.pas', cDisallowedRoutineNoncompliant,
      TargetsConfig(cRoutineId, ['TThing.Banned']), lc);
    AssertEquals('owner-qualified matches the method', 1,
      CountById(lc, cRoutineId));
  finally
    lc.Free;
  end;
  // The Unit-qualified glob matches both routines (each carries the Unit form).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'noncompliant.pas', cDisallowedRoutineNoncompliant,
      TargetsConfig(cRoutineId, ['*.Banned']), lc);
    AssertEquals('unit-qualified glob matches both', 2,
      CountById(lc, cRoutineId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedRoutineResolvedOnly;

var
  lc: TFpSonarIssueCollector;

begin
  // Positive control: the fixture calls the routine DoIt, which DOES resolve to a
  // TPasProcedure and fires. This proves the fixture resolves clean, so the zero
  // below is a real exclusion and not a silent-skip false-green.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'compliant.pas', cDisallowedRoutineCompliant,
      TargetsConfig(cRoutineId, ['DoIt']), lc);
    AssertTrue('control: routine reference resolves + fires',
      CountById(lc, cRoutineId) >= 1);
  finally
    lc.Free;
  end;
  // The compliant fixture has a LOCAL variable named Banned; references resolve to
  // the variable (a TPasVariable), which the is-TPasProcedure filter excludes ->
  // zero (resolved identity, not spelling).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'compliant.pas', cDisallowedRoutineCompliant,
      TargetsConfig(cRoutineId, ['Banned']), lc);
    AssertEquals('local var of same name not matched', 0,
      CountById(lc, cRoutineId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedTypeFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // TBanned is referenced in expression position twice: a typecast (row 25) and an
  // 'is' operand (row 26), both probe-locked to resolve to the TPasType -> two
  // issues; arg[0] is the bare type name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedType, 'noncompliant.pas', cDisallowedTypeNoncompliant,
      TargetsConfig(cTypeId, ['TBanned']), lc);
    AssertEquals('two type issues (typecast + is)', 2, CountById(lc, cTypeId));
    k := FirstById(lc, cTypeId);
    AssertEquals('first start line', 25, lc.Issues[k].StartLine);
    AssertEquals('arg is the type name', 'TBanned',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedTypeQualifiedMatches;

var
  lc: TFpSonarIssueCollector;

begin
  // The Unit-qualified glob matches the same two references.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedType, 'noncompliant.pas', cDisallowedTypeNoncompliant,
      TargetsConfig(cTypeId, ['*.TBanned']), lc);
    AssertEquals('unit-qualified glob matches', 2, CountById(lc, cTypeId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.DisallowedTypeCompliant;

var
  lc: TFpSonarIssueCollector;

begin
  // Positive control: the fixture references TOther in expression position, which
  // DOES resolve and fires. This proves the fixture resolves clean, so the zero
  // below is a real non-match and not a silent-skip false-green.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedType, 'compliant.pas', cDisallowedTypeCompliant,
      TargetsConfig(cTypeId, ['TOther']), lc);
    AssertTrue('control: type reference resolves + fires',
      CountById(lc, cTypeId) >= 1);
  finally
    lc.Free;
  end;
  // The compliant fixture references TOther only; a target of TBanned does not
  // match -> zero (and the fixture resolves clean).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedType, 'compliant.pas', cDisallowedTypeCompliant,
      TargetsConfig(cTypeId, ['TBanned']), lc);
    AssertEquals('non-referenced type silent', 0, CountById(lc, cTypeId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.RoutineTypePartition;

var
  lc: TFpSonarIssueCollector;

begin
  // Partition: a type reference is never caught by the routine rule (a TPasType is not a
  // TPasProcedure) ...
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'noncompliant.pas', cDisallowedTypeNoncompliant,
      TargetsConfig(cRoutineId, ['TBanned']), lc);
    AssertEquals('type not matched by routine rule', 0,
      CountById(lc, cRoutineId));
  finally
    lc.Free;
  end;
  // ... and a routine reference is never caught by the type rule (disjoint).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedType, 'noncompliant.pas', cDisallowedRoutineNoncompliant,
      TargetsConfig(cTypeId, ['Banned']), lc);
    AssertEquals('routine not matched by type rule', 0,
      CountById(lc, cTypeId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.TrackTypeAliasesFires;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The type-alias declaration TBannedAlias (probe-locked at line 10) matches its
  // target -> one issue at the alias's OWN declaration row, arg[0] = the alias name.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackTypeAliases, 'noncompliant.pas', cTrackTypeAliasesNoncompliant,
      TargetsConfig(cAliasId, ['TBannedAlias']), lc);
    AssertEquals('one alias issue', 1, CountById(lc, cAliasId));
    k := FirstById(lc, cAliasId);
    AssertEquals('alias decl line', 10, lc.Issues[k].StartLine);
    AssertEquals('arg is the alias name', 'TBannedAlias',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.TrackTypeAliasesWildcardReportsAll;

var
  lc: TFpSonarIssueCollector;

begin
  // A single '*' target reports EVERY alias: the fixture declares three
  // (TBannedAlias, TIntAlias, the strong-alias TStrong — probe-locked count), while
  // the plain class/record/base are not aliases.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackTypeAliases, 'noncompliant.pas', cTrackTypeAliasesNoncompliant,
      TargetsConfig(cAliasId, ['*']), lc);
    AssertEquals('wildcard reports every alias', 3, CountById(lc, cAliasId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.TrackTypeAliasesExcludesNonAlias;

var
  lc: TFpSonarIssueCollector;

begin
  // A plain class of a matching name (TPlain) is not a TPasAliasType -> zero
  // (alias-only filter).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackTypeAliases, 'noncompliant.pas', cTrackTypeAliasesNoncompliant,
      TargetsConfig(cAliasId, ['TPlain']), lc);
    AssertEquals('plain class not matched', 0, CountById(lc, cAliasId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.TrackTypeAliasesPerTargetSeverity;

var
  lc: TFpSonarIssueCollector;
  lCfg: TFpSonarConfig;
  k: Integer;

begin
  // TrackTypeAliases emits via EmitTargetLine; prove it honours per-target severity too. A
  // target WITHOUT a severity uses the rule default (Major).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackTypeAliases, 'noncompliant.pas', cTrackTypeAliasesNoncompliant,
      TargetsConfig(cAliasId, ['TBannedAlias']), lc);
    k := FirstById(lc, cAliasId);
    AssertTrue('default severity is major', lc.Issues[k].Severity = sevMajor);
  finally
    lc.Free;
  end;
  // A target WITH a severity overrides it (Minor).
  lCfg := TargetsConfig(cAliasId, ['TBannedAlias']);
  lCfg.Rules[0].Params[0].Targets[0].Severity := sevMinor;
  lCfg.Rules[0].Params[0].Targets[0].HasSeverity := True;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackTypeAliases, 'noncompliant.pas', cTrackTypeAliasesNoncompliant,
      lCfg, lc);
    k := FirstById(lc, cAliasId);
    AssertTrue('per-target severity wins', lc.Issues[k].Severity = sevMinor);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.UnconfiguredRulesAreSilent;

var
  lc: TFpSonarIssueCollector;

begin
  // With NO targets configured (TFpSonarConfig.Default), every rule emits nothing on its own
  // noncompliant fixture.
  lc := TFpSonarIssueCollector.Create;
  try
    RunImportSrc(NewDisallowedImportByPath, TFpSonarConfig.Default, lc);
    AssertEquals('import unconfigured => 0', 0, CountById(lc, cImportId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'noncompliant.pas', cDisallowedConstantNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('const unconfigured => 0', 0, CountById(lc, cConstId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedEnumValue, 'noncompliant.pas', cDisallowedEnumValueNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('enum unconfigured => 0', 0, CountById(lc, cEnumId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedField, 'noncompliant.pas', cDisallowedFieldNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('field unconfigured => 0', 0, CountById(lc, cFieldId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedIdentifier, 'noncompliant.pas', cDisallowedIdentifierNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('identifier unconfigured => 0', 0, CountById(lc, cIdentId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedProperty, 'noncompliant.pas', cDisallowedPropertyNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('property unconfigured => 0', 0, CountById(lc, cPropId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedRoutine, 'noncompliant.pas', cDisallowedRoutineNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('routine unconfigured => 0', 0, CountById(lc, cRoutineId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedType, 'noncompliant.pas', cDisallowedTypeNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('type unconfigured => 0', 0, CountById(lc, cTypeId));
  finally
    lc.Free;
  end;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackTypeAliases, 'noncompliant.pas', cTrackTypeAliasesNoncompliant,
      TFpSonarConfig.Default, lc);
    AssertEquals('alias unconfigured => 0', 0, CountById(lc, cAliasId));
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.PerTargetSeverityOverridesDefault;

var
  lc: TFpSonarIssueCollector;
  lCfg: TFpSonarConfig;
  k: Integer;

begin
  // A target without a severity uses the rule default (Major).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'noncompliant.pas', cDisallowedConstantNoncompliant,
      TargetsConfig(cConstId, ['Forbidden']), lc);
    k := FirstById(lc, cConstId);
    AssertTrue('default severity is major', lc.Issues[k].Severity = sevMajor);
  finally
    lc.Free;
  end;
  // A target WITH a severity overrides it (Minor).
  lCfg := TargetsConfig(cConstId, ['Forbidden']);
  lCfg.Rules[0].Params[0].Targets[0].Severity := sevMinor;
  lCfg.Rules[0].Params[0].Targets[0].HasSeverity := True;
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDisallowedConstant, 'noncompliant.pas', cDisallowedConstantNoncompliant,
      lCfg, lc);
    k := FirstById(lc, cConstId);
    AssertTrue('per-target severity wins', lc.Issues[k].Severity = sevMinor);
  finally
    lc.Free;
  end;
end;


procedure TTrackersRulesTest.TrackersSelfRegisterGlobally;

begin
  // The production initialization registered all three trackers into the GLOBAL
  // registry (this is what the CLI process runs).
  AssertTrue('DisallowedImportByPath registered',
    RuleRegistry.FindById(cImportId) <> nil);
  AssertTrue('DisallowedConstant registered',
    RuleRegistry.FindById(cConstId) <> nil);
  AssertTrue('DisallowedEnumValue registered',
    RuleRegistry.FindById(cEnumId) <> nil);
  AssertTrue('DisallowedField registered',
    RuleRegistry.FindById(cFieldId) <> nil);
  AssertTrue('DisallowedIdentifier registered',
    RuleRegistry.FindById(cIdentId) <> nil);
  AssertTrue('DisallowedProperty registered',
    RuleRegistry.FindById(cPropId) <> nil);
  AssertTrue('DisallowedRoutine registered',
    RuleRegistry.FindById(cRoutineId) <> nil);
  AssertTrue('DisallowedType registered',
    RuleRegistry.FindById(cTypeId) <> nil);
  AssertTrue('TrackTypeAliases registered',
    RuleRegistry.FindById(cAliasId) <> nil);
end;


initialization
  RegisterTest(TTrackersRulesTest);

end.
