{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the LCL form-file rule

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesForms;

{ The LCL form-file rule tests: LfmFormFileExists,
  rtAst / rfAst / sevMinor / itCodeSmell / cfMedium. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.Config, FpSonar.RuleFramework,
  FpSonar.Rules.Forms, UtstFixtures;

type
  { AST-tier form-file rule position + registration + config tests. }
  TRulesFormsTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture
    // with aConfig threaded onto the engine; issues land in aCollector.
    procedure RunRuleCfg(aRule: TRuleBase; const aFixture: string;
      const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // A config carrying rules.LfmFormFileExists.params.formBaseTypes = aValue.
    function FormBaseTypesConfig(const aValue: string): TFpSonarConfig;
    // A fresh, separately-owned rule instance (metadata mirrors the unit's
    // self-registration, including the declared formBaseTypes param).
    function NewLfmFormFileExists: TRuleBase;
  published
    procedure LfmFormFileExistsFlagsMissingLfm;
    procedure LfmFormFileExistsSilentWhenLfmPresent;
    procedure LfmFormFileExistsIgnoresNonFormUnit;
    procedure LfmFormFileExistsHonorsConfiguredBaseType;
    procedure FormsRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cLfmFormFileExistsId = 'LfmFormFileExists';
  cLfmFormFileExistsKey = 'rule.LfmFormFileExists.message';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cFormClassLine = 11;  // probe-locked line of 'TMyForm = class(...)'

  // Embedded LfmFormFileExists fixtures (Approach A rollout): line i+1 == [i].
  // The compliant case needs BOTH compliant.pas AND its sibling
  // compliant.lfm written to the same temp dir (the rule checks disk).

  cLfmNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  TMyForm = class(TForm)',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cLfmCompliant: array[0..15] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  TMyForm = class(TForm)',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cLfmCompliantLfm: array[0..4] of string = (
    'object MyForm: TMyForm',
    '  Left = 0',
    '  Top = 0',
    '  Caption = ''MyForm''',
    'end');

  cLfmNonform: array[0..21] of string = (
    'unit nonform;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  TThing = class(TObject)',
    '  end;',
    '',
    'procedure DoStuff;',
    '',
    'implementation',
    '',
    'procedure DoStuff;',
    'begin',
    'end;',
    '',
    'end.');

  cLfmConfigBase: array[0..15] of string = (
    'unit configbase;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    'type',
    '  TMyForm = class(TMyBaseForm)',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

procedure TRulesFormsTest.RunRuleCfg(aRule: TRuleBase; const aFixture: string;
  const aConfig: TFpSonarConfig; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := aConfig;
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesFormsTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesFormsTest.FirstById(const aCollector: TFpSonarIssueCollector;
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


function TRulesFormsTest.FormBaseTypesConfig(const aValue: string): TFpSonarConfig;

var
  lSetting: TFpSonarRuleSetting;
  lParam: TFpSonarRuleParam;

begin
  Result := TFpSonarConfig.Default;
  lSetting.RuleId := cLfmFormFileExistsId;
  lSetting.HasEnabled := False;
  lSetting.Enabled := True;
  lSetting.HasSeverity := False;
  lSetting.Severity := sevInfo;
  lParam.Key := 'formBaseTypes';
  lParam.Kind := cpkStr;
  lParam.IntVal := 0;
  lParam.StrVal := aValue;
  lParam.BoolVal := False;
  SetLength(lParam.Targets, 0);
  SetLength(lSetting.Params, 1);
  lSetting.Params[0] := lParam;
  SetLength(Result.Rules, 1);
  Result.Rules[0] := lSetting;
end;


function TRulesFormsTest.NewLfmFormFileExists: TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  lMeta := TRuleMetadata.Make(cLfmFormFileExistsId, rtAst, rfAst, sevMinor,
    itCodeSmell, cfMedium, True, cLfmFormFileExistsKey);
  lMeta.AddParam('formBaseTypes', rpkString);
  Result := TRuleLfmFormFileExists.Create(lMeta);
end;


// A form unit with no sibling .lfm fires exactly one issue at the first
// form-class line, column 1, with no message args.
procedure TRulesFormsTest.LfmFormFileExistsFlagsMissingLfm;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lIdx: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      // No sibling .lfm written => the rule fires.
      RunRuleCfg(NewLfmFormFileExists,
        lFix.Add('noncompliant.pas', cLfmNoncompliant), TFpSonarConfig.Default, lc);
      AssertEquals('one missing-lfm issue', 1, CountById(lc, cLfmFormFileExistsId));
      lIdx := FirstById(lc, cLfmFormFileExistsId);
      AssertEquals('start line', cFormClassLine, lc.Issues[lIdx].StartLine);
      AssertEquals('start col', 1, lc.Issues[lIdx].StartCol);
      AssertEquals('end line', cFormClassLine, lc.Issues[lIdx].EndLine);
      AssertEquals('end col', 1, lc.Issues[lIdx].EndCol);
      AssertEquals('message key', cLfmFormFileExistsKey, lc.Issues[lIdx].MessageKey);
      AssertEquals('no message args', 0, Length(lc.Issues[lIdx].MessageArgs));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


// A form unit WITH a sibling .lfm (compliant.lfm exists) stays silent.
procedure TRulesFormsTest.LfmFormFileExistsSilentWhenLfmPresent;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPas: string;

begin
  lFix := TTempFixtures.Create;
  try
    // Write BOTH the unit AND its sibling .lfm (same basename) into the temp dir;
    // the rule checks disk for <basename>.lfm, so its presence keeps it silent.
    lPas := lFix.Add('compliant.pas', cLfmCompliant);
    lFix.Add('compliant.lfm', cLfmCompliantLfm);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleCfg(NewLfmFormFileExists, lPas, TFpSonarConfig.Default, lc);
      AssertEquals('no findings when .lfm present', 0,
        CountById(lc, cLfmFormFileExistsId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


// A non-form unit (TThing = class(TObject), no form ancestor) never
// fires, even with no .lfm sibling.
procedure TRulesFormsTest.LfmFormFileExistsIgnoresNonFormUnit;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRuleCfg(NewLfmFormFileExists,
        lFix.Add('nonform.pas', cLfmNonform), TFpSonarConfig.Default, lc);
      AssertEquals('non-form unit not flagged', 0,
        CountById(lc, cLfmFormFileExistsId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


// formBaseTypes extends the matched base set: configbase.pas
// (TMyForm = class(TMyBaseForm), no .lfm) fires only when the config names
// TMyBaseForm; with the default list it abstains (TMyBaseForm not a default).
procedure TRulesFormsTest.LfmFormFileExistsHonorsConfiguredBaseType;

var
  lCfg, lDef: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPas: string;

begin
  lFix := TTempFixtures.Create;
  lCfg := TFpSonarIssueCollector.Create;
  lDef := TFpSonarIssueCollector.Create;
  try
    // configbase.pas has no sibling .lfm; the config-named base type decides.
    lPas := lFix.Add('configbase.pas', cLfmConfigBase);
    RunRuleCfg(NewLfmFormFileExists, lPas,
      FormBaseTypesConfig('TMyBaseForm'), lCfg);
    AssertEquals('configured base type matched', 1,
      CountById(lCfg, cLfmFormFileExistsId));

    RunRuleCfg(NewLfmFormFileExists, lPas, TFpSonarConfig.Default, lDef);
    AssertEquals('default list does not match TMyBaseForm', 0,
      CountById(lDef, cLfmFormFileExistsId));
  finally
    lCfg.Free;
    lDef.Free;
    lFix.Free;
  end;
end;


// The rule self-registers globally as rtAst/rfAst (assert via FindById,
// not a brittle registry-count total).
procedure TRulesFormsTest.FormsRulesSelfRegisterGlobally;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cLfmFormFileExistsId);
  AssertTrue('LfmFormFileExists registered', lRule <> nil);
  AssertEquals('tier rtAst', Ord(rtAst), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfAst', Ord(rfAst), Ord(lRule.Metadata.Feed));
  AssertTrue('default-enabled', lRule.Metadata.DefaultEnabled);
  AssertEquals('one declared param', 1, Length(lRule.Metadata.ParamSpecs));
  AssertEquals('param name formBaseTypes', 'formBaseTypes',
    lRule.Metadata.ParamSpecs[0].Name);
  AssertEquals('param kind rpkString', Ord(rpkString),
    Ord(lRule.Metadata.ParamSpecs[0].Kind));
end;


initialization
  RegisterTest(TRulesFormsTest);

end.
