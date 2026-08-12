{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the .lpi/.lpk/fpc.cfg project-model parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstProjectModel;

{ ProjectModel tests. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, UtstFixtures;

type
  { ProjectModel parse + robustness tests. }
  TProjectModelTest = class(TTestCase)
  private
    procedure AssertArray(const aMsg: string; const aExpected: array of string;
      const aActual: TFpSonarStringArray);
    function ArrayContains(const aValues: TFpSonarStringArray;
      const aValue: string): Boolean;
  published
    procedure ParsesFpcCfgDirectives;
    procedure ParsesLpiProjectFields;
    procedure LpiRelativeSearchPathsAnchoredToProjectDir;
    procedure MissingFileYieldsDiagnosticNotException;
    procedure MalformedXmlYieldsDiagnosticNotException;
  end;


implementation

const
  // Embedded ProjectModel fixtures (Approach A): line i+1 == [i].

  cFpcFixture: array[0..9] of string = (
    '# fpc.cfg fixture for the ProjectModel text-parse test.',
    '# Comments (# and ;) and blank lines are ignored.',
    '',
    '-Fu/usr/lib/fpc/units',
    '-Fi./inc',
    '',
    '-dDEBUG',
    '-dFEATURE_X',
    '-Mobjfpc',
    '; a trailing semicolon comment');

  cProjFixture: array[0..40] of string = (
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<CONFIG>',
    '  <ProjectOptions>',
    '    <Version Value="12"/>',
    '    <General>',
    '      <Flags>',
    '        <MainUnitHasCreateFormStatements Value="False"/>',
    '      </Flags>',
    '      <Title Value="projfixture"/>',
    '    </General>',
    '    <Units>',
    '      <Unit0>',
    '        <Filename Value="smokefixture.pas"/>',
    '        <IsPartOfProject Value="True"/>',
    '      </Unit0>',
    '      <Unit1>',
    '        <Filename Value="scannerfixture.pas"/>',
    '        <IsPartOfProject Value="True"/>',
    '      </Unit1>',
    '      <Unit2>',
    '        <Filename Value="notpartof.pas"/>',
    '        <IsPartOfProject Value="False"/>',
    '      </Unit2>',
    '    </Units>',
    '  </ProjectOptions>',
    '  <CompilerOptions>',
    '    <Version Value="11"/>',
    '    <SearchPaths>',
    '      <OtherUnitFiles Value="lib;src/units"/>',
    '      <IncludeFiles Value="inc;include"/>',
    '    </SearchPaths>',
    '    <Parsing>',
    '      <SyntaxOptions>',
    '        <SyntaxMode Value="Delphi"/>',
    '      </SyntaxOptions>',
    '    </Parsing>',
    '    <Other>',
    '      <CustomOptions Value="-dPROJDEF -dEXTRA"/>',
    '    </Other>',
    '  </CompilerOptions>',
    '</CONFIG>');

  cBadProj: array[0..6] of string = (
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<CONFIG>',
    '  <ProjectOptions>',
    '    <Units>',
    '      <Unit0>',
    '        <Filename Value="oops.pas"',
    '      </Unit0>');

  // A .lpi whose search paths use a '..' relative segment — the real-world case
  // that regressed in the IDE: it must anchor to the PROJECT dir, not the CWD.
  cRelPathLpi: array[0..9] of string = (
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<CONFIG>',
    '  <CompilerOptions>',
    '    <Version Value="11"/>',
    '    <SearchPaths>',
    '      <OtherUnitFiles Value="../CanvasEngine;lib"/>',
    '      <IncludeFiles Value="../shared/inc"/>',
    '    </SearchPaths>',
    '  </CompilerOptions>',
    '</CONFIG>');

procedure TProjectModelTest.AssertArray(const aMsg: string;
  const aExpected: array of string; const aActual: TFpSonarStringArray);

var
  i: Integer;

begin
  AssertEquals(aMsg + ' (length)', Length(aExpected), Length(aActual));
  for i := 0 to High(aExpected) do
    AssertEquals(aMsg + ' [' + IntToStr(i) + ']', aExpected[i], aActual[i]);
end;


function TProjectModelTest.ArrayContains(const aValues: TFpSonarStringArray;
  const aValue: string): Boolean;

var
  i: Integer;

begin
  Result := False;
  for i := 0 to High(aValues) do
    if aValues[i] = aValue then
      Exit(True);
end;


procedure TProjectModelTest.ParsesFpcCfgDirectives;

var
  lFix: TTempFixtures;
  lModel: TFpSonarProjectModel;
  lCfg: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;
  lCfgPath, lDir: string;

begin
  lFix := TTempFixtures.Create;
  lModel := TFpSonarProjectModel.Create;
  try
    lCfgPath := lFix.Add('fpcfixture.cfg', cFpcFixture);
    AssertTrue('fpc.cfg loads', lModel.TryLoad(lCfgPath, lCfg, lDiag));
    lDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(lCfgPath)));
    // Absolute -Fu passes through; relative -Fi (./inc) anchors to the .cfg dir.
    AssertArray('unit search paths from -Fu', ['/usr/lib/fpc/units'],
      lCfg.UnitSearchPaths);
    AssertArray('include paths from -Fi (anchored)',
      [ExpandFileName(lDir + 'inc')], lCfg.IncludePaths);
    AssertArray('defines from -d', ['DEBUG', 'FEATURE_X'], lCfg.Defines);
    AssertEquals('mode from -M', 'objfpc', lCfg.Mode);
  finally
    lModel.Free;
    lFix.Free;
  end;
end;


procedure TProjectModelTest.ParsesLpiProjectFields;

var
  lFix: TTempFixtures;
  lModel: TFpSonarProjectModel;
  lCfg: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;
  lLpi, lDir, lSmoke: string;

begin
  lFix := TTempFixtures.Create;
  lModel := TFpSonarProjectModel.Create;
  try
    lLpi := lFix.Add('projfixture.lpi', cProjFixture);
    AssertTrue('.lpi loads', lModel.TryLoad(lLpi, lCfg, lDiag));
    lDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(lLpi)));
    // Relative search paths anchor to the .lpi dir (like the project units),
    // not the process CWD.
    AssertArray('unit search paths (anchored)',
      [ExpandFileName(lDir + 'lib'), ExpandFileName(lDir + 'src/units')],
      lCfg.UnitSearchPaths);
    AssertArray('include paths (anchored)',
      [ExpandFileName(lDir + 'inc'), ExpandFileName(lDir + 'include')],
      lCfg.IncludePaths);
    AssertEquals('syntax mode', 'Delphi', lCfg.Mode);
    AssertArray('defines from CustomOptions', ['PROJDEF', 'EXTRA'], lCfg.Defines);

    // Project units resolved to ABSOLUTE paths relative to the .lpi dir; the
    // IsPartOfProject=False unit (notpartof.pas) is excluded.
    lSmoke := ExpandFileName(lDir + 'smokefixture.pas');
    AssertEquals('two project units (notpartof excluded)', 2,
      Length(lCfg.TargetFiles));
    AssertTrue('smokefixture.pas resolved absolute',
      ArrayContains(lCfg.TargetFiles, lSmoke));
    AssertTrue('no non-project unit',
      not ArrayContains(lCfg.TargetFiles,
        ExpandFileName(lDir + 'notpartof.pas')));
  finally
    lModel.Free;
    lFix.Free;
  end;
end;


procedure TProjectModelTest.LpiRelativeSearchPathsAnchoredToProjectDir;

var
  lFix: TTempFixtures;
  lModel: TFpSonarProjectModel;
  lCfg: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;
  lLpi, lDir: string;

begin
  lFix := TTempFixtures.Create;
  lModel := TFpSonarProjectModel.Create;
  try
    lLpi := lFix.Add('relproj.lpi', cRelPathLpi);
    AssertTrue('.lpi loads', lModel.TryLoad(lLpi, lCfg, lDiag));
    lDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(lLpi)));
    // '../CanvasEngine' resolves against the PROJECT dir (collapsing '..'), so
    // it lands beside the project, not under the process CWD.
    AssertArray('unit search paths anchored to project dir',
      [ExpandFileName(lDir + '../CanvasEngine'), ExpandFileName(lDir + 'lib')],
      lCfg.UnitSearchPaths);
    AssertArray('include paths anchored to project dir',
      [ExpandFileName(lDir + '../shared/inc')], lCfg.IncludePaths);
    // The anchored value differs from the OLD CWD-relative resolution (the bug):
    // lFix.Dir is a fresh temp dir, never the process CWD.
    AssertTrue('anchored to project dir, not the CWD',
      lCfg.UnitSearchPaths[0] <> ExpandFileName('../CanvasEngine'));
  finally
    lModel.Free;
    lFix.Free;
  end;
end;


procedure TProjectModelTest.MissingFileYieldsDiagnosticNotException;

var
  lFix: TTempFixtures;
  lModel: TFpSonarProjectModel;
  lCfg: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;

begin
  lFix := TTempFixtures.Create;
  lModel := TFpSonarProjectModel.Create;
  try
    // A missing .cfg must not raise — it becomes a dkParseError diagnostic.
    // A path inside the temp dir that is deliberately never written.
    AssertFalse('missing file returns False',
      lModel.TryLoad(IncludeTrailingPathDelimiter(lFix.Dir) +
        'does-not-exist.cfg', lCfg, lDiag));
    AssertEquals('diagnostic kind', Ord(dkParseError), Ord(lDiag.Kind));
    AssertTrue('diagnostic carries a message', lDiag.Message <> '');
  finally
    lModel.Free;
    lFix.Free;
  end;
end;


procedure TProjectModelTest.MalformedXmlYieldsDiagnosticNotException;

var
  lFix: TTempFixtures;
  lModel: TFpSonarProjectModel;
  lCfg: TFpSonarAnalysisConfig;
  lDiag: TFpSonarDiagnostic;

begin
  lFix := TTempFixtures.Create;
  lModel := TFpSonarProjectModel.Create;
  try
    // Malformed XML must not crash — it becomes a dkParseError diagnostic.
    AssertFalse('malformed .lpi returns False',
      lModel.TryLoad(lFix.Add('badproj.lpi', cBadProj), lCfg, lDiag));
    AssertEquals('diagnostic kind', Ord(dkParseError), Ord(lDiag.Kind));
  finally
    lModel.Free;
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TProjectModelTest);

end.
