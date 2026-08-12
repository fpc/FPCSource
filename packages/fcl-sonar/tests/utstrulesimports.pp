{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the uses-clause (SEM) import rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesImports;

{ The resolver-backed (SEM) import-rule tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Imports, UtstFixtures;

type
  { SEM-tier import-rule position + registration tests. }
  TRulesImportsTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned). Threads the fixture's own
    // directory as the unit search path so cross-unit helper units resolve.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires exactly once at aLine, column 1, with key
    // rule.<aId>.message and message args = aArgs; and zero on the compliant
    // fixture (the silent-skip canary). The noncompliant/compliant sources are
    // supplied inline and materialised into aFix (which the caller may pre-
    // populate with sibling units for cross-unit resolution); aFix's directory is
    // the unit search path.
    procedure CheckImportRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aLine: Integer; const aArgs: array of string;
      aFix: TTempFixtures; const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the unit's
    // self-registration; empty key defaults to rule.<RuleId>.message).
    function NewFullyQualifiedImports: TRuleBase;
    function NewMoveImportToImplementation: TRuleBase;
  published
    procedure FullyQualifiedImportsPositions;
    procedure MoveImportToImplementationPositions;
    procedure ImportRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cFullyQualifiedImportsId = 'FullyQualifiedImports';
  cMoveImportToImplementationId = 'MoveImportToImplementation';

  // Embedded import-rule fixtures (Approach A rollout): line i+1 == [i].

  cFqiNoncompliant: array[0..28] of string = (
    'unit NonCompliant;',
    '',
    '{ FullyQualifiedImports (#60) noncompliant fixture.',
    '',
    '  Both UnitA and UnitB export Helper, so the bare `Helper(aX)` call on its own',
    '  known line is ambiguous (shadowable across two in-scope source modules) and',
    '  should be written `UnitA.Helper(aX)`. Exactly one issue at the call''s row,',
    '  arg [''Helper'']. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  UnitA, UnitB;',
    '',
    'procedure DoIt(const aX: Integer);',
    '',
    '',
    'implementation',
    '',
    'procedure DoIt(const aX: Integer);',
    '',
    'begin',
    '  Helper(aX);',
    'end;',
    '',
    '',
    'end.');

  cFqiCompliant: array[0..37] of string = (
    'unit Compliant;',
    '',
    '{ FullyQualifiedImports compliant fixture (the FP guards;',
    '  must resolve clean AND stay silent — the silent-skip false-green canary).',
    '',
    '    * UnitA.Helper(aX) — explicitly unit-qualified -> silent.',
    '    * Solo(aX)         — single-source name (only UnitA exports it), not',
    '                         shadowable -> silent.',
    '    * lLocal := aX     — a local/parameter reference, same module -> silent.',
    '    * StrToInt(''1'')    — a SysUtils symbol used unqualified; SysUtils is in the',
    '                         default ignoreUnits set -> silent. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  UnitA, UnitB, SysUtils;',
    '',
    'procedure DoIt(const aX: Integer);',
    '',
    '',
    'implementation',
    '',
    'procedure DoIt(const aX: Integer);',
    '',
    'var',
    '  lLocal: Integer;',
    '',
    'begin',
    '  UnitA.Helper(aX);',
    '  Solo(aX);',
    '  lLocal := aX;',
    '  lLocal := StrToInt(''1'');',
    'end;',
    '',
    '',
    'end.');

  cFqiUnitA: array[0..32] of string = (
    'unit UnitA;',
    '',
    '{ FullyQualifiedImports (#60) fixture helper unit A.',
    '',
    '  Exports Helper (the shadowable simple name — UnitB ALSO exports Helper, so a',
    '  bare Helper reference over `uses UnitA, UnitB` is ambiguous) AND Solo (a',
    '  single-source name — only UnitA exports it, so a bare Solo reference is NOT',
    '  shadowable and must stay silent). Resolved cross-unit via the fixture-directory',
    '  search path (the mainusesdep+depunit precedent). }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Helper(const aX: Integer);',
    'procedure Solo(const aX: Integer);',
    '',
    '',
    'implementation',
    '',
    'procedure Helper(const aX: Integer);',
    '',
    'begin',
    'end;',
    '',
    '',
    'procedure Solo(const aX: Integer);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cFqiUnitB: array[0..22] of string = (
    'unit UnitB;',
    '',
    '{ FullyQualifiedImports (#60) fixture helper unit B.',
    '',
    '  Also exports Helper — the SECOND source that makes the simple name Helper',
    '  shadowable across `uses UnitA, UnitB`. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Helper(const aX: Integer);',
    '',
    '',
    'implementation',
    '',
    'procedure Helper(const aX: Integer);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cMoveNoncompliant: array[0..29] of string = (
    'unit NonCompliant;',
    '',
    '{ MoveImportToImplementation (#61) noncompliant fixture.',
    '',
    '  SysUtils is listed in the INTERFACE uses clause, but no interface-section',
    '  declaration/signature references any SysUtils symbol — the only SysUtils use',
    '  (StrToInt) is in the implementation body of Hello. So SysUtils can be moved to',
    '  the implementation uses clause. Exactly one issue at the interface `SysUtils`',
    '  uses-unit row, arg [''SysUtils'']. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'function Hello: Integer;',
    '',
    '',
    'implementation',
    '',
    'function Hello: Integer;',
    '',
    'begin',
    '  Result := StrToInt(''1'');',
    'end;',
    '',
    '',
    'end.');

  cMoveCompliant: array[0..36] of string = (
    'unit Compliant;',
    '',
    '{ MoveImportToImplementation (#61) compliant fixture (the FP guards;',
    '  must resolve clean AND stay silent — the silent-skip false-green canary).',
    '',
    '    * Classes  — TStream is the result type of the interface signature',
    '                 `function Load: TStream`, an interface-visible reference ->',
    '                 NOT movable -> silent.',
    '    * SysUtils — TFormatSettings appears ONLY as the parameter type of the',
    '                 interface procedural type TFmtProc; a standalone procedural-type',
    '                 declaration''s argument/result types are interface-visible',
    '                 references -> SysUtils is NOT movable -> silent. (Guards the',
    '                 procedural-type traversal path of TryMovableInterfaceUnits.) }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes, SysUtils;',
    '',
    'type',
    '  TFmtProc = procedure(const aFmt: TFormatSettings);',
    '',
    'function Load: TStream;',
    '',
    '',
    'implementation',
    '',
    'function Load: TStream;',
    '',
    'begin',
    '  Result := nil;',
    'end;',
    '',
    '',
    'end.');

procedure TRulesImportsTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lDir: string;

begin
  // The fixture's own directory is the unit search path, so the FullyQualifiedImports
  // helper units UnitA / UnitB (siblings of noncompliant.pas) resolve cross-unit.
  // Harmless for the MoveImportToImplementation fixtures, whose imports are
  // synthetic-RTL units.
  lDir := ExtractFilePath(aFixture);
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Analyze(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
      [lDir], [], aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesImportsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesImportsTest.FirstById(
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


function TRulesImportsTest.NewFullyQualifiedImports: TRuleBase;

begin
  Result := TRuleFullyQualifiedImports.Create(TRuleMetadata.Make(
    cFullyQualifiedImportsId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesImportsTest.NewMoveImportToImplementation: TRuleBase;

begin
  Result := TRuleMoveImportToImplementation.Create(TRuleMetadata.Make(
    cMoveImportToImplementationId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


procedure TRulesImportsTest.CheckImportRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aLine: Integer; const aArgs: array of string;
  aFix: TTempFixtures; const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  k, m: Integer;

begin
  // Noncompliant: exactly one issue at the offending node's row, column 1 (SEM
  // nodes carry no column), carrying aArgs as the message args. Written into
  // aFix (whose dir RunRule threads as the unit search path), alongside any
  // sibling units the caller pre-added.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRule(aRule, aFix.Add('noncompliant.pas', aNoncompliant), lc);
    AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
    k := FirstById(lc, aId);
    AssertEquals('start line', aLine, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', aLine, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('arg count', Length(aArgs),
      Length(lc.Issues[k].MessageArgs));
    for m := 0 to High(aArgs) do
      AssertEquals('arg ' + IntToStr(m), aArgs[m],
        lc.Issues[k].MessageArgs[m]);
  finally
    lc.Free;
  end;

  // Compliant: the FP guards stay silent (and the fixture MUST resolve clean,
  // else the rfResolver feed is absent -> rule skipped -> a false 0).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRule(aCompliantRule, aFix.Add('compliant.pas', aCompliant), lc);
    AssertEquals('compliant => zero', 0, CountById(lc, aId));
  finally
    lc.Free;
  end;
end;


procedure TRulesImportsTest.FullyQualifiedImportsPositions;

var
  lFix: TTempFixtures;

begin
  // Noncompliant: bare 'Helper(aX)' over 'uses UnitA, UnitB' (both export Helper)
  // at row 25 (probe-locked); arg is the shadowable simple name. Compliant: an
  // explicitly-qualified UnitA.Helper, a single-source Solo, a local reference,
  // and a SysUtils symbol (ignoreUnits) — all silent.
  lFix := TTempFixtures.Create;
  try
    // UnitA/UnitB are the two source units that make 'Helper' shadowable; they
    // must be siblings in the search dir for cross-unit resolution.
    lFix.Add('unita.pas', cFqiUnitA);
    lFix.Add('unitb.pas', cFqiUnitB);
    CheckImportRuleSrc(NewFullyQualifiedImports, NewFullyQualifiedImports,
      cFullyQualifiedImportsId, 25, ['Helper'], lFix,
      cFqiNoncompliant, cFqiCompliant);
  finally
    lFix.Free;
  end;
end;


procedure TRulesImportsTest.MoveImportToImplementationPositions;

var
  lFix: TTempFixtures;

begin
  // Noncompliant: interface 'uses SysUtils' at row 16 (probe-locked) used only in
  // the implementation body (StrToInt); arg is the movable unit name. Compliant:
  // Classes (TStream result type) and SysUtils (TFormatSettings interface var)
  // are both interface-referenced -> silent. Imports are synthetic-RTL units, so
  // no sibling fixtures are needed.
  lFix := TTempFixtures.Create;
  try
    CheckImportRuleSrc(NewMoveImportToImplementation, NewMoveImportToImplementation,
      cMoveImportToImplementationId, 16, ['SysUtils'], lFix,
      cMoveNoncompliant, cMoveCompliant);
  finally
    lFix.Free;
  end;
end;


procedure TRulesImportsTest.ImportRulesSelfRegisterGlobally;

begin
  // The production initialization registered both SEM import rules into the
  // GLOBAL registry (this is what the CLI process runs).
  AssertTrue('FullyQualifiedImports registered',
    RuleRegistry.FindById(cFullyQualifiedImportsId) <> nil);
  AssertTrue('MoveImportToImplementation registered',
    RuleRegistry.FindById(cMoveImportToImplementationId) <> nil);
end;


initialization
  RegisterTest(TRulesImportsTest);

end.
