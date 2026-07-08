{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the TOK-tier declaration/keyword/punctuation rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesTokens;

{ The TOK declaration/keyword rules. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Tokens, UtstFixtures;

type
  { TOK declaration/keyword rule position + registration tests. }
  TRulesTokensTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    // As RunRule, but the fixture source is supplied inline (one array element
    // per source line) and materialised to a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewLowercaseKeywords: TRuleBase;
    function NewCombineConst: TRuleBase;
    function NewCombineType: TRuleBase;
    function NewCombineVar: TRuleBase;
    function NewDeclareFields: TRuleBase;
    function NewDeclareVariables: TRuleBase;
    function NewDeclareParameters: TRuleBase;
    function NewNoEmptyParens: TRuleBase;
    function NewNoStraySemicolons: TRuleBase;
    function NewNoOmittedSemicolons: TRuleBase;
    function NewNoExtraneousCommas: TRuleBase;
    function NewNoDisabledHints: TRuleBase;
    function NewNoDisabledWarnings: TRuleBase;
    function NewNoIndentUnitLevel: TRuleBase;
    function NewIndentVisibility: TRuleBase;
    function NewNoCommentedOutCode: TRuleBase;
    function NewTrackNoSonar: TRuleBase;
    function NewTrackComments: TRuleBase;
    function NewTrackStringLiterals: TRuleBase;
    function NewCombineVisibilitySections: TRuleBase;
    function NewRemoveEmptyVisibilitySection: TRuleBase;
    function NewRemoveEmptyFieldSection: TRuleBase;
  published
    procedure LowercaseKeywordsPositions;
    procedure CombineConstSectionsPositions;
    procedure CombineTypeSectionsPositions;
    procedure CombineVarSectionsPositions;
    procedure DeclareFieldsIndividuallyPositions;
    procedure DeclareVariablesIndividuallyPositions;
    procedure DeclareParametersIndividuallyPositions;
    procedure NoEmptyParenthesesOnRoutinesPositions;
    procedure NoStraySemicolonsPositions;
    procedure NoOmittedSemicolonsPositions;
    procedure NoExtraneousCommasPositions;
    procedure NoDisabledCompilerHintsPositions;
    procedure NoDisabledCompilerWarningsPositions;
    procedure NoIndentUnitLevelKeywordsPositions;
    procedure IndentVisibilitySpecifiersPositions;
    procedure NoCommentedOutCodePositions;
    procedure TrackNoSonarPositions;
    procedure TrackCommentsPositions;
    procedure TrackStringLiteralsPositions;
    procedure CombineVisibilitySectionsPositions;
    procedure RemoveEmptyVisibilitySectionPositions;
    procedure RemoveEmptyFieldSectionPositions;
    procedure RulesSelfRegisterGlobally;
    procedure RunIsDeterministic;
  end;


implementation

const
  cMode = 'OBJFPC';
  cLowercaseId = 'LowercaseKeywords';
  cCombineConstId = 'CombineConstSections';
  cCombineTypeId = 'CombineTypeSections';
  cCombineVarId = 'CombineVarSections';
  cDeclareFieldsId = 'DeclareFieldsIndividually';
  cDeclareVariablesId = 'DeclareVariablesIndividually';
  cDeclareParametersId = 'DeclareParametersIndividually';
  cNoEmptyParensId = 'NoEmptyParenthesesOnRoutines';
  cNoStraySemicolonsId = 'NoStraySemicolons';
  cNoOmittedSemicolonsId = 'NoOmittedSemicolons';
  cNoExtraneousCommasId = 'NoExtraneousCommas';
  cNoDisabledHintsId = 'NoDisabledCompilerHints';
  cNoDisabledWarningsId = 'NoDisabledCompilerWarnings';
  cNoIndentUnitLevelId = 'NoIndentUnitLevelKeywords';
  cIndentVisibilityId = 'IndentVisibilitySpecifiers';
  cNoCommentedOutCodeId = 'NoCommentedOutCode';
  cTrackNoSonarId = 'TrackNoSonar';
  cTrackCommentsId = 'TrackComments';
  cTrackStringLiteralsId = 'TrackStringLiterals';
  cCombineVisibilityId = 'CombineVisibilitySections';
  cRemoveEmptyVisibilityId = 'RemoveEmptyVisibilitySection';
  cRemoveEmptyFieldId = 'RemoveEmptyFieldSection';

  // Embedded token-rule fixtures (Approach A rollout): line i+1 == [i].
  // In-line tabs and trailing spaces are preserved verbatim inside the literals.

  cCombineConstSectionsNoncompliant: array[0..11] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const A = 1;',
    'const B = 2;',
    '',
    'implementation',
    '',
    'end.');

  cCombineConstSectionsCompliant: array[0..34] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  A = 1;',
    '  B = 2;',
    '',
    'implementation',
    '',
    '// A const section in the implementation is a separate declaration scope from',
    '// the interface one — the interface/implementation boundary is NOT combinable.',
    'const',
    '  C = 3;',
    '',
    '// A nested routine with its own const section is an intervening routine:',
    '// the two enclosing const sections must NOT be flagged as combinable.',
    'procedure DoIt;',
    'const',
    '  D = 4;',
    '',
    '  procedure Inner;',
    '  const',
    '    E = 5;',
    '  begin',
    '  end;',
    '',
    'const',
    '  F = 6;',
    'begin',
    'end;',
    '',
    'end.');

  cCombineTypeSectionsNoncompliant: array[0..13] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TA = Integer;',
    'type',
    '  TB = Integer;',
    '',
    'implementation',
    '',
    'end.');

  cCombineTypeSectionsCompliant: array[0..34] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TA = Integer;',
    '  TB = Integer;',
    '',
    'implementation',
    '',
    '// A type section in the implementation is a separate declaration scope from the',
    '// interface one — the interface/implementation boundary is NOT combinable.',
    'type',
    '  TC = Integer;',
    '',
    '// A nested routine with its own type section is an intervening routine:',
    '// the two enclosing type sections must NOT be flagged as combinable.',
    'procedure DoIt;',
    'type',
    '  TD = Integer;',
    '',
    '  procedure Inner;',
    '  type',
    '    TE = Integer;',
    '  begin',
    '  end;',
    '',
    'type',
    '  TF = Integer;',
    'begin',
    'end;',
    '',
    'end.');

  cCombineVarSectionsNoncompliant: array[0..13] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'var',
    '  A: Integer;',
    'var',
    '  B: Integer;',
    '',
    'end.');

  cCombineVarSectionsCompliant: array[0..35] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// A var section here, then a routine, then another var section: the routine is',
    '// an intervening routine so the two unit-level var sections must NOT be',
    '// flagged as combinable.',
    'var',
    '  A: Integer;',
    '',
    'procedure DoIt;',
    'var',
    '  B: Integer;',
    '',
    '  procedure Inner;',
    '  var',
    '    C: Integer;',
    '  begin',
    '    C := 0;',
    '  end;',
    '',
    'var',
    '  D: Integer;',
    'begin',
    '  B := 0;',
    '  D := 0;',
    'end;',
    '',
    'var',
    '  E: Integer;',
    '',
    'end.');

  cCombineVisibilitySectionsNoncompliant: array[0..16] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    FName: string;',
    '  private',
    '    FSize: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cCombineVisibilitySectionsCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    FName: string;',
    '    FSize: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDeclareFieldsIndividuallyNoncompliant: array[0..13] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint = class',
    '    FX, FY: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDeclareFieldsIndividuallyCompliant: array[0..14] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint = class',
    '    FX: Integer;',
    '    FY: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDeclareParametersIndividuallyNoncompliant: array[0..12] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P(A, B: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  cDeclareParametersIndividuallyCompliant: array[0..12] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P(A: Integer; B: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  cDeclareVariablesIndividuallyNoncompliant: array[0..14] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    'var',
    '  A, B: Integer;',
    'begin',
    'end;',
    '',
    'end.');

  cDeclareVariablesIndividuallyCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    'var',
    '  A: Integer;',
    '  B: Integer;',
    'begin',
    'end;',
    '',
    'end.');

  cIndentVisibilitySpecifiersNoncompliant: array[0..14] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFoo = class',
    '    private',
    '    FX: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cIndentVisibilitySpecifiersCompliant: array[0..14] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFoo = class',
    '  private',
    '    FX: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cLowercaseKeywordsNoncompliant: array[0..13] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    '',
    'Begin',
    'end;',
    '',
    'end.');

  cLowercaseKeywordsCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure DoIt;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cNoCommentedOutCodeNoncompliant: array[0..16] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  Total: Integer;',
    'begin',
    '  // Total := Total + 1;',
    '  Total := 0;',
    'end;',
    '',
    'end.');

  cNoCommentedOutCodeCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'begin',
    '  // Compute the running total for the active period.',
    '  // NOSONAR Total := Total + 1;',
    '  {$WARNINGS ON}',
    'end;',
    '',
    'end.');

  cNoDisabledCompilerHintsNoncompliant: array[0..9] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$HINTS OFF}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cNoDisabledCompilerHintsCompliant: array[0..9] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$HINTS ON}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cNoDisabledCompilerWarningsNoncompliant: array[0..9] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$WARNINGS OFF}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cNoDisabledCompilerWarningsCompliant: array[0..9] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$WARNINGS ON}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cNoEmptyParenthesesOnRoutinesNoncompliant: array[0..12] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P();',
    'begin',
    'end;',
    '',
    'end.');

  cNoEmptyParenthesesOnRoutinesCompliant: array[0..12] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'begin',
    'end;',
    '',
    'end.');

  cNoExtraneousCommasNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: set of Byte;',
    'begin',
    '  X := [1, 2, 3,];',
    'end;',
    '',
    'end.');

  cNoExtraneousCommasCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: set of Byte;',
    'begin',
    '  X := [1, 2, 3];',
    'end;',
    '',
    'end.');

  cNoIndentUnitLevelKeywordsNoncompliant: array[0..8] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '  implementation',
    '',
    'end.');

  cNoIndentUnitLevelKeywordsCompliant: array[0..21] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  TFoo = Integer;',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  X := 1;',
    'end;',
    '',
    'end.');

  cNoOmittedSemicolonsNoncompliant: array[0..16] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  X := 1;',
    '  X := 2',
    'end;',
    '',
    'end.');

  cNoOmittedSemicolonsCompliant: array[0..41] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TRec = record',
    '    X: Integer;',
    '  end;',
    '',
    '  TFoo = class',
    '    Y: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  X := 1;',
    '  case X of',
    '    1: X := 2;',
    '    2: X := 3;',
    '  end;',
    '  X := 4;',
    'end;',
    '',
    'procedure Q;',
    'begin',
    'end;',
    '',
    '// An asm block''s instructions are newline-separated, not '';''-terminated, so',
    '// its closing ''end'' must NOT be flagged as an omitted separator.',
    'procedure R; assembler;',
    'asm',
    '  movq $1, %rax',
    '  movq $2, %rbx',
    'end;',
    '',
    'end.');

  cNoStraySemicolonsNoncompliant: array[0..16] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  X := 1;;',
    '  X := 2;',
    'end;',
    '',
    'end.');

  cNoStraySemicolonsCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  X := 1;',
    '  X := 2;',
    'end;',
    '',
    'end.');

  cRemoveEmptyFieldSectionNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  strict private',
    '    class var',
    '  public',
    '    procedure DoIt;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoIt;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cRemoveEmptyFieldSectionCompliant: array[0..21] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  strict private',
    '    class var FCount: Integer;',
    '  public',
    '    procedure DoIt;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoIt;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cRemoveEmptyVisibilitySectionNoncompliant: array[0..20] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '  public',
    '    procedure DoIt;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoIt;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cRemoveEmptyVisibilitySectionCompliant: array[0..21] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    FName: string;',
    '  public',
    '    procedure DoIt;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoIt;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cTrackCommentsNoncompliant: array[0..13] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'begin',
    '  // TODO: handle the empty-list case',
    'end;',
    '',
    'end.');

  cTrackCommentsCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'begin',
    '  // todo handle the empty-list case (lowercase, not flagged)',
    'end;',
    '',
    'end.');

  cTrackNoSonarNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  X := 1; // NOSONAR  reason: legacy API',
    'end;',
    '',
    'end.');

  cTrackNoSonarCompliant: array[0..17] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  X: Integer;',
    'begin',
    '  // plain comment, no marker',
    '  {$WARNINGS ON}',
    '  X := 1;',
    'end;',
    '',
    'end.');

  cTrackStringLiteralsNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  Url: string;',
    'begin',
    '  Url := ''http://internal.example'';',
    'end;',
    '',
    'end.');

  cTrackStringLiteralsCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure P;',
    'var',
    '  Url: string;',
    'begin',
    '  Url := ''https://secure.example'';',
    'end;',
    '',
    'end.');

procedure TRulesTokensTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Analyze(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
      aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TRulesTokensTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  // Materialise the inline fixture (one array element per source line, in-line
  // tabs / trailing spaces preserved) to a temp dir, run, and delete.
  lFix := TTempFixtures.Create;
  try
    RunRule(aRule, lFix.Add(aName, aSrc), aCollector);
  finally
    lFix.Free;
  end;
end;


function TRulesTokensTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesTokensTest.FirstById(const aCollector: TFpSonarIssueCollector;
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


function TRulesTokensTest.NewLowercaseKeywords: TRuleBase;

begin
  Result := TRuleLowercaseKeywords.Create(TRuleMetadata.Make(cLowercaseId, rtTok,
    rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewCombineConst: TRuleBase;

begin
  Result := TRuleCombineConstSections.Create(TRuleMetadata.Make(cCombineConstId, rtTok,
    rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewCombineType: TRuleBase;

begin
  Result := TRuleCombineTypeSections.Create(TRuleMetadata.Make(cCombineTypeId, rtTok,
    rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewCombineVar: TRuleBase;

begin
  Result := TRuleCombineVarSections.Create(TRuleMetadata.Make(cCombineVarId, rtTok,
    rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewDeclareFields: TRuleBase;

begin
  Result := TRuleDeclareFieldsIndividually.Create(TRuleMetadata.Make(cDeclareFieldsId,
    rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewDeclareVariables: TRuleBase;

begin
  Result := TRuleDeclareVariablesIndividually.Create(TRuleMetadata.Make(
    cDeclareVariablesId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewDeclareParameters: TRuleBase;

begin
  Result := TRuleDeclareParametersIndividually.Create(TRuleMetadata.Make(
    cDeclareParametersId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewNoEmptyParens: TRuleBase;

begin
  Result := TRuleNoEmptyParenthesesOnRoutines.Create(TRuleMetadata.Make(
    cNoEmptyParensId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewNoStraySemicolons: TRuleBase;

begin
  Result := TRuleNoStraySemicolons.Create(TRuleMetadata.Make(cNoStraySemicolonsId,
    rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewNoOmittedSemicolons: TRuleBase;

begin
  Result := TRuleNoOmittedSemicolons.Create(TRuleMetadata.Make(cNoOmittedSemicolonsId,
    rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewNoExtraneousCommas: TRuleBase;

begin
  Result := TRuleNoExtraneousCommas.Create(TRuleMetadata.Make(cNoExtraneousCommasId,
    rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewNoDisabledHints: TRuleBase;

begin
  Result := TRuleNoDisabledCompilerHints.Create(TRuleMetadata.Make(cNoDisabledHintsId,
    rtTok, rfTokenStream, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewNoDisabledWarnings: TRuleBase;

begin
  Result := TRuleNoDisabledCompilerWarnings.Create(TRuleMetadata.Make(
    cNoDisabledWarningsId, rtTok, rfTokenStream, sevMajor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewNoIndentUnitLevel: TRuleBase;

begin
  Result := TRuleNoIndentUnitLevelKeywords.Create(TRuleMetadata.Make(
    cNoIndentUnitLevelId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewIndentVisibility: TRuleBase;

begin
  Result := TRuleIndentVisibilitySpecifiers.Create(TRuleMetadata.Make(
    cIndentVisibilityId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewNoCommentedOutCode: TRuleBase;

begin
  Result := TRuleNoCommentedOutCode.Create(TRuleMetadata.Make(cNoCommentedOutCodeId,
    rtTok, rfTokenStream, sevMajor, itCodeSmell, cfMedium, True, ''));
end;


function TRulesTokensTest.NewTrackNoSonar: TRuleBase;

begin
  Result := TRuleTrackNoSonar.Create(TRuleMetadata.Make(cTrackNoSonarId, rtTok,
    rfTokenStream, sevInfo, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewTrackComments: TRuleBase;

begin
  Result := TRuleTrackComments.Create(TRuleMetadata.Make(cTrackCommentsId, rtTok,
    rfTokenStream, sevInfo, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewTrackStringLiterals: TRuleBase;

begin
  Result := TRuleTrackStringLiterals.Create(TRuleMetadata.Make(cTrackStringLiteralsId,
    rtTok, rfTokenStream, sevInfo, itCodeSmell, cfHigh, True, ''));
end;


function TRulesTokensTest.NewCombineVisibilitySections: TRuleBase;

begin
  Result := TRuleCombineVisibilitySections.Create(TRuleMetadata.Make(
    cCombineVisibilityId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesTokensTest.NewRemoveEmptyVisibilitySection: TRuleBase;

begin
  Result := TRuleRemoveEmptyVisibilitySection.Create(TRuleMetadata.Make(
    cRemoveEmptyVisibilityId, rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesTokensTest.NewRemoveEmptyFieldSection: TRuleBase;

begin
  Result := TRuleRemoveEmptyFieldSection.Create(TRuleMetadata.Make(
    cRemoveEmptyFieldId, rtTok, rfTokenStream, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


procedure TRulesTokensTest.LowercaseKeywordsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "Begin" on line 11 col 1 => range cols 1..5, arg "begin".
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewLowercaseKeywords, 'noncompliant.pas', cLowercaseKeywordsNoncompliant, lc);
    AssertEquals('one lowercase-keyword issue', 1, CountById(lc, cLowercaseId));
    k := FirstById(lc, cLowercaseId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 11, lc.Issues[k].EndLine);
    AssertEquals('end col', 5, lc.Issues[k].EndCol);
    // The rule emits a message KEY + the lowercase spelling as its single arg
    // (the template text itself is a shared-catalog concern, asserted by the
    // output-adapter tests; here we verify the arg the rule supplies).
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('arg is the lowercase spelling', 'begin',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('key is the dotted rule key', 'rule.LowercaseKeywords.message',
      lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: every keyword already lowercase.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewLowercaseKeywords, 'compliant.pas', cLowercaseKeywordsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cLowercaseId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.CombineConstSectionsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 2nd "const" on line 8 col 1 => range cols 1..5.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineConst, 'noncompliant.pas', cCombineConstSectionsNoncompliant, lc);
    AssertEquals('one combine-const issue', 1, CountById(lc, cCombineConstId));
    k := FirstById(lc, cCombineConstId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end col', 5, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: a single const block.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineConst, 'compliant.pas', cCombineConstSectionsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cCombineConstId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.CombineTypeSectionsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 2nd "type" on line 9 col 1 => range cols 1..4.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineType, 'noncompliant.pas', cCombineTypeSectionsNoncompliant, lc);
    AssertEquals('one combine-type issue', 1, CountById(lc, cCombineTypeId));
    k := FirstById(lc, cCombineTypeId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end col', 4, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: a single type block.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineType, 'compliant.pas', cCombineTypeSectionsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cCombineTypeId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.CombineVarSectionsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 2nd "var" on line 11 col 1 => range cols 1..3.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineVar, 'noncompliant.pas', cCombineVarSectionsNoncompliant, lc);
    AssertEquals('one combine-var issue', 1, CountById(lc, cCombineVarId));
    k := FirstById(lc, cCombineVarId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end col', 3, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: a single var block.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineVar, 'compliant.pas', cCombineVarSectionsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cCombineVarId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.DeclareFieldsIndividuallyPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "FX, FY" on line 9 => range cols 5..10 (FX@5, FY@9).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareFields, 'noncompliant.pas', cDeclareFieldsIndividuallyNoncompliant, lc);
    AssertEquals('one declare-fields issue', 1, CountById(lc, cDeclareFieldsId));
    k := FirstById(lc, cDeclareFieldsId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('end line', 9, lc.Issues[k].EndLine);
    AssertEquals('end col', 10, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: one field per declaration.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareFields, 'compliant.pas', cDeclareFieldsIndividuallyCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cDeclareFieldsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.DeclareVariablesIndividuallyPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "A, B" on line 11 => range cols 3..6 (A@3, B@6).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareVariables, 'noncompliant.pas', cDeclareVariablesIndividuallyNoncompliant, lc);
    AssertEquals('one declare-variables issue', 1,
      CountById(lc, cDeclareVariablesId));
    k := FirstById(lc, cDeclareVariablesId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('end col', 6, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: one variable per declaration.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareVariables, 'compliant.pas', cDeclareVariablesIndividuallyCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cDeclareVariablesId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.DeclareParametersIndividuallyPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "A, B" on line 9 => range cols 13..16 (A@13, B@16).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareParameters, 'noncompliant.pas', cDeclareParametersIndividuallyNoncompliant, lc);
    AssertEquals('one declare-parameters issue', 1,
      CountById(lc, cDeclareParametersId));
    k := FirstById(lc, cDeclareParametersId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 13, lc.Issues[k].StartCol);
    AssertEquals('end col', 16, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: one parameter per group.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareParameters, 'compliant.pas', cDeclareParametersIndividuallyCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cDeclareParametersId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoEmptyParenthesesOnRoutinesPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "P()" on line 9 => point at the open paren, col 12.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoEmptyParens, 'noncompliant.pas', cNoEmptyParenthesesOnRoutinesNoncompliant, lc);
    AssertEquals('one empty-parens issue', 1, CountById(lc, cNoEmptyParensId));
    k := FirstById(lc, cNoEmptyParensId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 12, lc.Issues[k].StartCol);
    AssertEquals('point end line', 9, lc.Issues[k].EndLine);
    AssertEquals('point end col', 12, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: no parentheses on the parameterless routine.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoEmptyParens, 'compliant.pas', cNoEmptyParenthesesOnRoutinesCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoEmptyParensId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoStraySemicolonsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "X := 1;;" on line 13 => stray 2nd ';' (prev sig is ';') at
  // col 10 (point).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoStraySemicolons, 'noncompliant.pas', cNoStraySemicolonsNoncompliant, lc);
    AssertEquals('one stray-semicolon issue', 1,
      CountById(lc, cNoStraySemicolonsId));
    k := FirstById(lc, cNoStraySemicolonsId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 10, lc.Issues[k].StartCol);
    AssertEquals('point end line', 13, lc.Issues[k].EndLine);
    AssertEquals('point end col', 10, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key', 'rule.NoStraySemicolons.message',
      lc.Issues[k].MessageKey);
    AssertEquals('no message args', 0, Length(lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // Compliant: every ';' terminates a real statement.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoStraySemicolons, 'compliant.pas', cNoStraySemicolonsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoStraySemicolonsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoOmittedSemicolonsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "X := 2" (no ';') before "end" => flag the terminator keyword
  // on line 15, range cols 1..3.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoOmittedSemicolons, 'noncompliant.pas', cNoOmittedSemicolonsNoncompliant, lc);
    AssertEquals('one omitted-semicolon issue', 1,
      CountById(lc, cNoOmittedSemicolonsId));
    k := FirstById(lc, cNoOmittedSemicolonsId);
    AssertEquals('start line', 15, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 15, lc.Issues[k].EndLine);
    AssertEquals('end col', 3, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.NoOmittedSemicolons.message', lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: enriched with record/class/case/empty-block ends that must NOT
  // be flagged (the conservative scope guards).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoOmittedSemicolons, 'compliant.pas', cNoOmittedSemicolonsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoOmittedSemicolonsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoExtraneousCommasPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: trailing comma in "[1, 2, 3,]" on line 13 at col 16 (point).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoExtraneousCommas, 'noncompliant.pas', cNoExtraneousCommasNoncompliant, lc);
    AssertEquals('one extraneous-comma issue', 1,
      CountById(lc, cNoExtraneousCommasId));
    k := FirstById(lc, cNoExtraneousCommasId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 16, lc.Issues[k].StartCol);
    AssertEquals('point end line', 13, lc.Issues[k].EndLine);
    AssertEquals('point end col', 16, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: a well-formed set literal, no redundant commas.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoExtraneousCommas, 'compliant.pas', cNoExtraneousCommasCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoExtraneousCommasId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoDisabledCompilerHintsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "{$HINTS OFF}" on line 4 col 1 (point at the directive).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoDisabledHints, 'noncompliant.pas', cNoDisabledCompilerHintsNoncompliant, lc);
    AssertEquals('one disabled-hints issue', 1,
      CountById(lc, cNoDisabledHintsId));
    k := FirstById(lc, cNoDisabledHintsId);
    AssertEquals('start line', 4, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('point end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.NoDisabledCompilerHints.message', lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: "{$HINTS ON}" leaves hints enabled.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoDisabledHints, 'compliant.pas', cNoDisabledCompilerHintsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoDisabledHintsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoDisabledCompilerWarningsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "{$WARNINGS OFF}" on line 4 col 1 (point at the directive).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoDisabledWarnings, 'noncompliant.pas', cNoDisabledCompilerWarningsNoncompliant, lc);
    AssertEquals('one disabled-warnings issue', 1,
      CountById(lc, cNoDisabledWarningsId));
    k := FirstById(lc, cNoDisabledWarningsId);
    AssertEquals('start line', 4, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('point end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.NoDisabledCompilerWarnings.message', lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: "{$WARNINGS ON}" leaves warnings enabled.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoDisabledWarnings, 'compliant.pas', cNoDisabledCompilerWarningsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoDisabledWarningsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoIndentUnitLevelKeywordsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "  implementation" indented to col 3 on line 7 => range
  // cols 3..16 (Length('implementation') = 14).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoIndentUnitLevel, 'noncompliant.pas', cNoIndentUnitLevelKeywordsNoncompliant, lc);
    AssertEquals('one indent-keyword issue', 1,
      CountById(lc, cNoIndentUnitLevelId));
    k := FirstById(lc, cNoIndentUnitLevelId);
    AssertEquals('start line', 7, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('end line', 7, lc.Issues[k].EndLine);
    AssertEquals('end col', 16, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  // Compliant: unit/interface/uses/type/implementation all at column 1; the
  // routine-local var is NOT flagged (provably-unit-level guard + column 1).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoIndentUnitLevel, 'compliant.pas', cNoIndentUnitLevelKeywordsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoIndentUnitLevelId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.IndentVisibilitySpecifiersPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "private" indented to col 5 on line 9, while TFoo is at col 3
  // => range cols 5..11 (Length('private') = 7).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewIndentVisibility, 'noncompliant.pas', cIndentVisibilitySpecifiersNoncompliant, lc);
    AssertEquals('one indent-visibility issue', 1,
      CountById(lc, cIndentVisibilityId));
    k := FirstById(lc, cIndentVisibilityId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('end line', 9, lc.Issues[k].EndLine);
    AssertEquals('end col', 11, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.IndentVisibilitySpecifiers.message', lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: "private" at the same column as TFoo.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewIndentVisibility, 'compliant.pas', cIndentVisibilitySpecifiersCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cIndentVisibilityId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.NoCommentedOutCodePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "// Total := Total + 1;" on line 13 col 3 (point at comment).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoCommentedOutCode, 'noncompliant.pas', cNoCommentedOutCodeNoncompliant, lc);
    AssertEquals('one commented-out-code issue', 1,
      CountById(lc, cNoCommentedOutCodeId));
    k := FirstById(lc, cNoCommentedOutCodeId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('point end line', 13, lc.Issues[k].EndLine);
    AssertEquals('point end col', 3, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.NoCommentedOutCode.message', lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: prose comment, a NOSONAR-carrying assignment shape, and a
  // dollar-directive comment — all excluded.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoCommentedOutCode, 'compliant.pas', cNoCommentedOutCodeCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cNoCommentedOutCodeId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.TrackNoSonarPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "// NOSONAR  reason: legacy API" trailing line 13 at col 11
  // (point at the comment).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackNoSonar, 'noncompliant.pas', cTrackNoSonarNoncompliant, lc);
    AssertEquals('one track-nosonar issue', 1, CountById(lc, cTrackNoSonarId));
    k := FirstById(lc, cTrackNoSonarId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 11, lc.Issues[k].StartCol);
    AssertEquals('point end line', 13, lc.Issues[k].EndLine);
    AssertEquals('point end col', 11, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key', 'rule.TrackNoSonar.message',
      lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: a plain comment and a dollar-directive comment, no NOSONAR.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackNoSonar, 'compliant.pas', cTrackNoSonarCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cTrackNoSonarId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.TrackCommentsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: "// TODO: ..." on line 11 col 3 (point at comment, arg TODO).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackComments, 'noncompliant.pas', cTrackCommentsNoncompliant, lc);
    AssertEquals('one track-comments issue', 1, CountById(lc, cTrackCommentsId));
    k := FirstById(lc, cTrackCommentsId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('point end line', 11, lc.Issues[k].EndLine);
    AssertEquals('point end col', 3, lc.Issues[k].EndCol);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('arg is the matched marker', 'TODO',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('key is the dotted rule key', 'rule.TrackComments.message',
      lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: lowercase "todo" is NOT a marker.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackComments, 'compliant.pas', cTrackCommentsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cTrackCommentsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.TrackStringLiteralsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 'http://internal.example' literal on line 13 at col 10 (point
  // at the literal, arg 'http://').
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackStringLiterals, 'noncompliant.pas', cTrackStringLiteralsNoncompliant, lc);
    AssertEquals('one track-string-literals issue', 1,
      CountById(lc, cTrackStringLiteralsId));
    k := FirstById(lc, cTrackStringLiteralsId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 10, lc.Issues[k].StartCol);
    AssertEquals('point end line', 13, lc.Issues[k].EndLine);
    AssertEquals('point end col', 10, lc.Issues[k].EndCol);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('arg is the matched pattern', 'http://',
      lc.Issues[k].MessageArgs[0]);
    AssertEquals('key is the dotted rule key',
      'rule.TrackStringLiterals.message', lc.Issues[k].MessageKey);
  finally
    lc.Free;
  end;

  // Compliant: 'https://secure.example' does NOT contain 'http://'.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewTrackStringLiterals, 'compliant.pas', cTrackStringLiteralsCompliant, lc);
    AssertEquals('compliant => zero', 0,
      CountById(lc, cTrackStringLiteralsId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.CombineVisibilitySectionsPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: a 2nd "private" section on line 11 col 3 => point, arg
  // "private".
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineVisibilitySections, 'noncompliant.pas', cCombineVisibilitySectionsNoncompliant, lc);
    AssertEquals('one combine-visibility issue', 1,
      CountById(lc, cCombineVisibilityId));
    k := FirstById(lc, cCombineVisibilityId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('point end line', 11, lc.Issues[k].EndLine);
    AssertEquals('point end col', 3, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.CombineVisibilitySections.message', lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('arg 0 is the visibility keyword', 'private',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  // Compliant: a single private section.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCombineVisibilitySections, 'compliant.pas', cCombineVisibilitySectionsCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cCombineVisibilityId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.RemoveEmptyVisibilitySectionPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: an empty "private" section (line 9 col 3) before "public" =>
  // point, arg "private".
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveEmptyVisibilitySection, 'noncompliant.pas', cRemoveEmptyVisibilitySectionNoncompliant, lc);
    AssertEquals('one empty-visibility issue', 1,
      CountById(lc, cRemoveEmptyVisibilityId));
    k := FirstById(lc, cRemoveEmptyVisibilityId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('point end line', 9, lc.Issues[k].EndLine);
    AssertEquals('point end col', 3, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.RemoveEmptyVisibilitySection.message', lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('arg 0 is the visibility keyword', 'private',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  // Compliant: both sections populated.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveEmptyVisibilitySection, 'compliant.pas', cRemoveEmptyVisibilitySectionCompliant, lc);
    AssertEquals('compliant => zero', 0,
      CountById(lc, cRemoveEmptyVisibilityId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.RemoveEmptyFieldSectionPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: an empty "class var" section => point at the "var" token
  // (line 10 col 11), no message args.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveEmptyFieldSection, 'noncompliant.pas', cRemoveEmptyFieldSectionNoncompliant, lc);
    AssertEquals('one empty-field-section issue', 1,
      CountById(lc, cRemoveEmptyFieldId));
    k := FirstById(lc, cRemoveEmptyFieldId);
    AssertEquals('start line', 10, lc.Issues[k].StartLine);
    AssertEquals('start col', 11, lc.Issues[k].StartCol);
    AssertEquals('point end line', 10, lc.Issues[k].EndLine);
    AssertEquals('point end col', 11, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.RemoveEmptyFieldSection.message', lc.Issues[k].MessageKey);
    AssertEquals('no message args', 0, Length(lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // Compliant: a "class var" with a field.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRemoveEmptyFieldSection, 'compliant.pas', cRemoveEmptyFieldSectionCompliant, lc);
    AssertEquals('compliant => zero', 0, CountById(lc, cRemoveEmptyFieldId));
  finally
    lc.Free;
  end;
end;


procedure TRulesTokensTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all 8 TOK rules into the GLOBAL
  // registry (this is what the CLI process runs).
  AssertTrue('LowercaseKeywords registered',
    RuleRegistry.FindById(cLowercaseId) <> nil);
  AssertTrue('CombineConstSections registered',
    RuleRegistry.FindById(cCombineConstId) <> nil);
  AssertTrue('CombineTypeSections registered',
    RuleRegistry.FindById(cCombineTypeId) <> nil);
  AssertTrue('CombineVarSections registered',
    RuleRegistry.FindById(cCombineVarId) <> nil);
  AssertTrue('DeclareFieldsIndividually registered',
    RuleRegistry.FindById(cDeclareFieldsId) <> nil);
  AssertTrue('DeclareVariablesIndividually registered',
    RuleRegistry.FindById(cDeclareVariablesId) <> nil);
  AssertTrue('DeclareParametersIndividually registered',
    RuleRegistry.FindById(cDeclareParametersId) <> nil);
  AssertTrue('NoEmptyParenthesesOnRoutines registered',
    RuleRegistry.FindById(cNoEmptyParensId) <> nil);
  // Six more TOK rules (14 TOK IDs total in the global registry).
  AssertTrue('NoStraySemicolons registered',
    RuleRegistry.FindById(cNoStraySemicolonsId) <> nil);
  AssertTrue('NoOmittedSemicolons registered',
    RuleRegistry.FindById(cNoOmittedSemicolonsId) <> nil);
  AssertTrue('NoExtraneousCommas registered',
    RuleRegistry.FindById(cNoExtraneousCommasId) <> nil);
  AssertTrue('NoDisabledCompilerHints registered',
    RuleRegistry.FindById(cNoDisabledHintsId) <> nil);
  AssertTrue('NoDisabledCompilerWarnings registered',
    RuleRegistry.FindById(cNoDisabledWarningsId) <> nil);
  AssertTrue('NoIndentUnitLevelKeywords registered',
    RuleRegistry.FindById(cNoIndentUnitLevelId) <> nil);
  // Five more TOK rules (19 TOK IDs total in the global registry).
  AssertTrue('IndentVisibilitySpecifiers registered',
    RuleRegistry.FindById(cIndentVisibilityId) <> nil);
  AssertTrue('NoCommentedOutCode registered',
    RuleRegistry.FindById(cNoCommentedOutCodeId) <> nil);
  AssertTrue('TrackNoSonar registered',
    RuleRegistry.FindById(cTrackNoSonarId) <> nil);
  AssertTrue('TrackComments registered',
    RuleRegistry.FindById(cTrackCommentsId) <> nil);
  AssertTrue('TrackStringLiterals registered',
    RuleRegistry.FindById(cTrackStringLiteralsId) <> nil);
  // Three more TOK rules (22 TOK IDs total in the global registry).
  AssertTrue('CombineVisibilitySections registered',
    RuleRegistry.FindById(cCombineVisibilityId) <> nil);
  AssertTrue('RemoveEmptyVisibilitySection registered',
    RuleRegistry.FindById(cRemoveEmptyVisibilityId) <> nil);
  AssertTrue('RemoveEmptyFieldSection registered',
    RuleRegistry.FindById(cRemoveEmptyFieldId) <> nil);
end;


procedure TRulesTokensTest.RunIsDeterministic;

var
  lFirst, lSecond: TFpSonarIssueCollector;
  i: Integer;

begin
  // Two runs of the same rule over the same noncompliant fixture must produce
  // identical issues (count, RuleId, positions, fingerprints).
  lFirst := TFpSonarIssueCollector.Create;
  lSecond := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeclareFields, 'noncompliant.pas', cDeclareFieldsIndividuallyNoncompliant, lFirst);
    RunRuleSrc(NewDeclareFields, 'noncompliant.pas', cDeclareFieldsIndividuallyNoncompliant, lSecond);

    AssertTrue('at least one issue', lFirst.Count > 0);
    AssertEquals('identical count', lFirst.Count, lSecond.Count);
    for i := 0 to lFirst.Count - 1 do
      begin
        AssertEquals('same RuleId at ' + IntToStr(i),
          lFirst.Issues[i].RuleId, lSecond.Issues[i].RuleId);
        AssertEquals('same start col at ' + IntToStr(i),
          lFirst.Issues[i].StartCol, lSecond.Issues[i].StartCol);
        AssertEquals('same fingerprint at ' + IntToStr(i),
          lFirst.Issues[i].Fingerprint, lSecond.Issues[i].Fingerprint);
        AssertTrue('fingerprinted at ' + IntToStr(i),
          lFirst.Issues[i].Fingerprint <> '');
      end;
  finally
    lFirst.Free;
    lSecond.Free;
  end;
end;


initialization
  RegisterTest(TRulesTokensTest);

end.
