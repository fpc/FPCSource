{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the AnalysisConfig tuple (defines, merge, precedence)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstAnalysisConfig;

{ AnalysisConfig tuple tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types;

type
  { AnalysisConfig tuple tests. }
  TAnalysisConfigTest = class(TTestCase)
  private
    // Asserts aActual equals aExpected element-for-element (and same length).
    procedure AssertArray(const aMsg: string; const aExpected: array of string;
      const aActual: TFpSonarStringArray);
  published
    procedure DefaultsHaveModeAndHostTarget;
    procedure EffectiveDefinesSynthesisAndDedup;
    procedure MergeAppliesScalarAndArrayPrecedence;
    procedure EffectiveDefinesIsDeterministic;
  end;


implementation

procedure TAnalysisConfigTest.AssertArray(const aMsg: string;
  const aExpected: array of string; const aActual: TFpSonarStringArray);

var
  i: Integer;

begin
  AssertEquals(aMsg + ' (length)', Length(aExpected), Length(aActual));
  for i := 0 to High(aExpected) do
    AssertEquals(aMsg + ' [' + IntToStr(i) + ']', aExpected[i], aActual[i]);
end;


procedure TAnalysisConfigTest.DefaultsHaveModeAndHostTarget;

var
  lCfg: TFpSonarAnalysisConfig;

begin
  lCfg := TFpSonarAnalysisConfig.Default;
  AssertEquals('default mode is OBJFPC', 'OBJFPC', lCfg.Mode);
  AssertTrue('host TargetCPU populated', lCfg.TargetCPU <> '');
  AssertTrue('host TargetOS populated', lCfg.TargetOS <> '');
end;


procedure TAnalysisConfigTest.EffectiveDefinesSynthesisAndDedup;

var
  lCfg: TFpSonarAnalysisConfig;

begin
  lCfg := TFpSonarAnalysisConfig.Default;
  lCfg.TargetCPU := 'x86_64';
  lCfg.TargetOS := 'linux';
  // The user passes FPC again plus MYFLAG; FPC must not be duplicated.
  SetLength(lCfg.Defines, 2);
  lCfg.Defines[0] := 'FPC';
  lCfg.Defines[1] := 'MYFLAG';

  // CPU/OS defines synthesised; FPC kept once; MYFLAG appended last.
  AssertArray('effective defines for x86_64/linux',
    ['FPC', 'CPUX86_64', 'UNIX', 'LINUX', 'MYFLAG'], lCfg.EffectiveDefines);
end;


procedure TAnalysisConfigTest.MergeAppliesScalarAndArrayPrecedence;

var
  lBase, lOverride, lMerged: TFpSonarAnalysisConfig;

begin
  lBase.Mode := 'OBJFPC';
  SetLength(lBase.Defines, 1);
  lBase.Defines[0] := 'BASE';
  SetLength(lBase.UnitSearchPaths, 1);
  lBase.UnitSearchPaths[0] := 'baselib';

  // Empty override scalar must keep the base value; arrays append after base.
  lOverride.Mode := '';
  SetLength(lOverride.Defines, 1);
  lOverride.Defines[0] := 'CLI';

  lMerged := lBase.Merge(lOverride);
  AssertEquals('empty override keeps base mode', 'OBJFPC', lMerged.Mode);
  AssertArray('defines append base then override', ['BASE', 'CLI'],
    lMerged.Defines);
  AssertArray('base-only array preserved', ['baselib'],
    lMerged.UnitSearchPaths);

  // Non-empty override scalar wins.
  lOverride.Mode := 'DELPHI';
  lMerged := lBase.Merge(lOverride);
  AssertEquals('non-empty override mode wins', 'DELPHI', lMerged.Mode);
end;


procedure TAnalysisConfigTest.EffectiveDefinesIsDeterministic;

var
  lA, lB: TFpSonarAnalysisConfig;
  lDefA, lDefB: TFpSonarStringArray;
  i: Integer;

begin
  lA := TFpSonarAnalysisConfig.Default;
  lA.TargetCPU := 'x86_64';
  lA.TargetOS := 'linux';
  SetLength(lA.Defines, 1);
  lA.Defines[0] := 'D1';
  lB := lA;  // identical inputs

  lDefA := lA.EffectiveDefines;
  lDefB := lB.EffectiveDefines;
  AssertEquals('same length across runs', Length(lDefA), Length(lDefB));
  for i := 0 to High(lDefA) do
    AssertEquals('same define at ' + IntToStr(i), lDefA[i], lDefB[i]);
  AssertEquals('identical Description', lA.Description,
    lB.Description);
end;


initialization
  RegisterTest(TAnalysisConfigTest);

end.
