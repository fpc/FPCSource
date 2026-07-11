{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the structural (AST) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesStructure;

{ The four AST-tier "volume" rules }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Config, FpSonar.Rules.Structure, UtstFixtures;

type
  { AST-tier volume-rule position + registration tests. }
  TRulesStructureTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    // As RunRule, threading aConfig onto the engine so a migrated rule reads its
    // configured params.
    procedure RunRuleCfg(aRule: TRuleBase; const aFixture: string;
      const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    // As RunRule / RunRuleCfg, but the fixture source is supplied inline (one
    // array element per source line) and materialised to a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector);
    procedure RunRuleCfgSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aConfig: TFpSonarConfig;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts the rule fires exactly once at aDeclLine, column 1, with message
    // args [aValue, aLimit]; and zero on the compliant fixture. Fixtures inline.
    procedure CheckVolumeRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aValue, aLimit: string;
      const aNoncompliant, aCompliant: array of string);
    // Asserts aRule fires exactly once at aDeclLine, column 1, with key
    // rule.<aId>.message and message args = aArgs; and zero on the compliant
    // fixture. Fixtures supplied inline, materialised to a temp dir.
    procedure CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewCyclomatic: TRuleBase;
    function NewCognitive: TRuleBase;
    function NewRoutineTooLarge: TRuleBase;
    function NewRoutineTooDeeplyNested: TRuleBase;
    function NewTooManyNestedRoutines: TRuleBase;
    function NewTooManyParameters: TRuleBase;
    function NewTooManyVariables: TRuleBase;
    function NewTooManyDefaultParameters: TRuleBase;
    function NewBeginEndRequired: TRuleBase;
    function NewNoGoto: TRuleBase;
    function NewNoWith: TRuleBase;
    function NewNoSelfAssignment: TRuleBase;
    function NewNoInlineAssembly: TRuleBase;
    function NewCaseAtLeastTwoItems: TRuleBase;
    function NewNoEmptyBlock: TRuleBase;
    function NewRoutineNotEmpty: TRuleBase;
    function NewUnitNotEmpty: TRuleBase;
    function NewRedundantJump: TRuleBase;
    function NewFunctionReturnTypeRequired: TRuleBase;
    function NewRedundantBooleanLiteral: TRuleBase;
    function NewNilCheckViaAssigned: TRuleBase;
    function NewNoObjectTypes: TRuleBase;
    function NewNoLegacyInitializationSection: TRuleBase;
    function NewInlineConstNoTypeInference: TRuleBase;
    function NewInlineLoopVarNoTypeInference: TRuleBase;
    function NewInlineVarNoTypeInference: TRuleBase;
    function NewProjectFileNoRoutines: TRuleBase;
    function NewProjectFileNoVariables: TRuleBase;
  published
    procedure CyclomaticComplexityPositions;
    procedure CognitiveComplexityPositions;
    procedure RoutineTooLargePositions;
    procedure RoutineTooDeeplyNestedPositions;
    procedure TooManyNestedRoutinesPositions;
    procedure TooManyParametersPositions;
    procedure TooManyVariablesPositions;
    procedure TooManyDefaultParametersPositions;
    procedure BeginEndRequiredPositions;
    procedure BeginEndRequiredExemptsElseIfAndReachesInit;
    procedure NoGotoPositions;
    procedure NoWithPositions;
    procedure NoSelfAssignmentPositions;
    procedure NoSelfAssignmentFlagsDottedMemberChain;
    procedure NoSelfAssignmentFlagsSelfMemberAccess;
    procedure NoSelfAssignmentIgnoresCompoundAssign;
    procedure NoInlineAssemblyPositions;
    procedure CaseAtLeastTwoItemsPositions;
    procedure NoEmptyBlockPositions;
    procedure RoutineNotEmptyPositions;
    procedure UnitNotEmptyPositions;
    procedure RedundantJumpPositions;
    procedure RedundantJumpFlagsTrailingContinueInLoop;
    procedure FunctionReturnTypeRequiredPositions;
    procedure RedundantBooleanLiteralPositions;
    procedure NilCheckViaAssignedPositions;
    procedure NoObjectTypesPositions;
    procedure NoLegacyInitializationSectionPositions;
    procedure InlineVarNoTypeInferencePositions;
    procedure InlineLoopVarNoTypeInferencePositions;
    procedure ProjectFileNoRoutinesPositions;
    procedure ProjectFileNoVariablesPositions;
    procedure InlineConstNoTypeInferenceIsInert;
    // A configured param changes DETECTION end-to-end.
    procedure ParamOverrideChangesDetection;
    procedure RulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cCyclomaticId = 'CyclomaticComplexity';
  cCognitiveId = 'CognitiveComplexity';
  cRoutineTooLargeId = 'RoutineTooLarge';
  cRoutineTooDeeplyNestedId = 'RoutineTooDeeplyNested';
  cTooManyNestedRoutinesId = 'TooManyNestedRoutines';
  cTooManyParametersId = 'TooManyParameters';
  cTooManyVariablesId = 'TooManyVariables';
  cTooManyDefaultParametersId = 'TooManyDefaultParameters';
  cBeginEndRequiredId = 'BeginEndRequired';
  cNoGotoId = 'NoGoto';
  cNoWithId = 'NoWith';
  cNoSelfAssignmentId = 'NoSelfAssignment';
  cNoInlineAssemblyId = 'NoInlineAssembly';
  cCaseAtLeastTwoItemsId = 'CaseAtLeastTwoItems';
  cNoEmptyBlockId = 'NoEmptyBlock';
  cRoutineNotEmptyId = 'RoutineNotEmpty';
  cUnitNotEmptyId = 'UnitNotEmpty';
  cRedundantJumpId = 'RedundantJump';
  cFunctionReturnTypeRequiredId = 'FunctionReturnTypeRequired';
  cRedundantBooleanLiteralId = 'RedundantBooleanLiteral';
  cNilCheckViaAssignedId = 'NilCheckViaAssigned';
  cNoObjectTypesId = 'NoObjectTypes';
  cNoLegacyInitializationSectionId = 'NoLegacyInitializationSection';
  cInlineConstNoTypeInferenceId = 'InlineConstNoTypeInference';
  cInlineLoopVarNoTypeInferenceId = 'InlineLoopVarNoTypeInference';
  cInlineVarNoTypeInferenceId = 'InlineVarNoTypeInference';
  cProjectFileNoRoutinesId = 'ProjectFileNoRoutines';
  cProjectFileNoVariablesId = 'ProjectFileNoVariables';

  // Embedded structure-rule fixtures (Approach A rollout): line i+1 == [i].

  cCyclomaticCompliant: array[0..24] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Cyclomatic complexity = 10 (base 1 + nine if statements): exactly at the',
    '// limit of 10, so nothing is flagged.',
    'procedure Complex(a: Integer);',
    '',
    'begin',
    '  if a = 1 then Writeln(''1'');',
    '  if a = 2 then Writeln(''2'');',
    '  if a = 3 then Writeln(''3'');',
    '  if a = 4 then Writeln(''4'');',
    '  if a = 5 then Writeln(''5'');',
    '  if a = 6 then Writeln(''6'');',
    '  if a = 7 then Writeln(''7'');',
    '  if a = 8 then Writeln(''8'');',
    '  if a = 9 then Writeln(''9'');',
    'end;',
    '',
    'end.');

  cCyclomaticNoncompliant: array[0..25] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Cyclomatic complexity = 11 (base 1 + ten independent if statements): one',
    '// over the limit of 10, so this routine is flagged with the value 11.',
    'procedure Complex(a: Integer);',
    '',
    'begin',
    '  if a = 1 then Writeln(''1'');',
    '  if a = 2 then Writeln(''2'');',
    '  if a = 3 then Writeln(''3'');',
    '  if a = 4 then Writeln(''4'');',
    '  if a = 5 then Writeln(''5'');',
    '  if a = 6 then Writeln(''6'');',
    '  if a = 7 then Writeln(''7'');',
    '  if a = 8 then Writeln(''8'');',
    '  if a = 9 then Writeln(''9'');',
    '  if a = 10 then Writeln(''10'');',
    'end;',
    '',
    'end.');

  cCognitiveCompliant: array[0..43] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Cognitive complexity = 15 (exactly at the limit, so nothing is flagged). Same',
    '// shape as the noncompliant fixture but the innermost condition drops its',
    '// `and` run, removing 1 point (16 -> 15).',
    'procedure Tangled(a, b, c, d, e, f, g: Boolean; n: Integer);',
    '',
    'var',
    '  i: Integer;',
    '',
    'begin',
    '  if a and b then',
    '  begin',
    '    if c then',
    '    begin',
    '      if d then',
    '        Writeln(''x'');',
    '    end;',
    '  end',
    '  else if e then',
    '    Writeln(''y'')',
    '  else',
    '    Writeln(''z'');',
    '  while f do',
    '  begin',
    '    if g then',
    '      Writeln(''w'');',
    '  end;',
    '  for i := 1 to n do',
    '  begin',
    '    case i of',
    '      1: Writeln(''1'');',
    '      2: Writeln(''2'');',
    '    end;',
    '  end;',
    'end;',
    '',
    'end.');

  cCognitiveNoncompliant: array[0..50] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Cognitive complexity = 16 (one over the limit of 15). Breakdown:',
    '//   if a and b      +1 (nest 0) +1 (and-run)        = 2',
    '//   if c            +2 (nest 1)                      = 4',
    '//   if d and a      +3 (nest 2) +1 (and-run)        = 8',
    '//   else if e       +1 (flat)                        = 9',
    '//   else            +1 (flat)                        = 10',
    '//   while f         +1 (nest 0)                      = 11',
    '//   if g            +2 (nest 1)                      = 13',
    '//   for i           +1 (nest 0)                      = 14',
    '//   case i          +2 (nest 1)                      = 16',
    'procedure Tangled(a, b, c, d, e, f, g: Boolean; n: Integer);',
    '',
    'var',
    '  i: Integer;',
    '',
    'begin',
    '  if a and b then',
    '  begin',
    '    if c then',
    '    begin',
    '      if d and a then',
    '        Writeln(''x'');',
    '    end;',
    '  end',
    '  else if e then',
    '    Writeln(''y'')',
    '  else',
    '    Writeln(''z'');',
    '  while f do',
    '  begin',
    '    if g then',
    '      Writeln(''w'');',
    '  end;',
    '  for i := 1 to n do',
    '  begin',
    '    case i of',
    '      1: Writeln(''1'');',
    '      2: Writeln(''2'');',
    '    end;',
    '  end;',
    'end;',
    '',
    'end.');

  cRoutineTooLargeCompliant: array[0..74] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Routine with exactly 60 executable statements: at the limit of 60, not over.',
    'procedure Big;',
    '',
    'begin',
    '  Writeln(''01'');',
    '  Writeln(''02'');',
    '  Writeln(''03'');',
    '  Writeln(''04'');',
    '  Writeln(''05'');',
    '  Writeln(''06'');',
    '  Writeln(''07'');',
    '  Writeln(''08'');',
    '  Writeln(''09'');',
    '  Writeln(''10'');',
    '  Writeln(''11'');',
    '  Writeln(''12'');',
    '  Writeln(''13'');',
    '  Writeln(''14'');',
    '  Writeln(''15'');',
    '  Writeln(''16'');',
    '  Writeln(''17'');',
    '  Writeln(''18'');',
    '  Writeln(''19'');',
    '  Writeln(''20'');',
    '  Writeln(''21'');',
    '  Writeln(''22'');',
    '  Writeln(''23'');',
    '  Writeln(''24'');',
    '  Writeln(''25'');',
    '  Writeln(''26'');',
    '  Writeln(''27'');',
    '  Writeln(''28'');',
    '  Writeln(''29'');',
    '  Writeln(''30'');',
    '  Writeln(''31'');',
    '  Writeln(''32'');',
    '  Writeln(''33'');',
    '  Writeln(''34'');',
    '  Writeln(''35'');',
    '  Writeln(''36'');',
    '  Writeln(''37'');',
    '  Writeln(''38'');',
    '  Writeln(''39'');',
    '  Writeln(''40'');',
    '  Writeln(''41'');',
    '  Writeln(''42'');',
    '  Writeln(''43'');',
    '  Writeln(''44'');',
    '  Writeln(''45'');',
    '  Writeln(''46'');',
    '  Writeln(''47'');',
    '  Writeln(''48'');',
    '  Writeln(''49'');',
    '  Writeln(''50'');',
    '  Writeln(''51'');',
    '  Writeln(''52'');',
    '  Writeln(''53'');',
    '  Writeln(''54'');',
    '  Writeln(''55'');',
    '  Writeln(''56'');',
    '  Writeln(''57'');',
    '  Writeln(''58'');',
    '  Writeln(''59'');',
    '  Writeln(''60'');',
    'end;',
    '',
    'end.');

  cRoutineTooLargeNoncompliant: array[0..75] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Routine with 61 executable statements: one over the limit of 60.',
    'procedure Big;',
    '',
    'begin',
    '  Writeln(''01'');',
    '  Writeln(''02'');',
    '  Writeln(''03'');',
    '  Writeln(''04'');',
    '  Writeln(''05'');',
    '  Writeln(''06'');',
    '  Writeln(''07'');',
    '  Writeln(''08'');',
    '  Writeln(''09'');',
    '  Writeln(''10'');',
    '  Writeln(''11'');',
    '  Writeln(''12'');',
    '  Writeln(''13'');',
    '  Writeln(''14'');',
    '  Writeln(''15'');',
    '  Writeln(''16'');',
    '  Writeln(''17'');',
    '  Writeln(''18'');',
    '  Writeln(''19'');',
    '  Writeln(''20'');',
    '  Writeln(''21'');',
    '  Writeln(''22'');',
    '  Writeln(''23'');',
    '  Writeln(''24'');',
    '  Writeln(''25'');',
    '  Writeln(''26'');',
    '  Writeln(''27'');',
    '  Writeln(''28'');',
    '  Writeln(''29'');',
    '  Writeln(''30'');',
    '  Writeln(''31'');',
    '  Writeln(''32'');',
    '  Writeln(''33'');',
    '  Writeln(''34'');',
    '  Writeln(''35'');',
    '  Writeln(''36'');',
    '  Writeln(''37'');',
    '  Writeln(''38'');',
    '  Writeln(''39'');',
    '  Writeln(''40'');',
    '  Writeln(''41'');',
    '  Writeln(''42'');',
    '  Writeln(''43'');',
    '  Writeln(''44'');',
    '  Writeln(''45'');',
    '  Writeln(''46'');',
    '  Writeln(''47'');',
    '  Writeln(''48'');',
    '  Writeln(''49'');',
    '  Writeln(''50'');',
    '  Writeln(''51'');',
    '  Writeln(''52'');',
    '  Writeln(''53'');',
    '  Writeln(''54'');',
    '  Writeln(''55'');',
    '  Writeln(''56'');',
    '  Writeln(''57'');',
    '  Writeln(''58'');',
    '  Writeln(''59'');',
    '  Writeln(''60'');',
    '  Writeln(''61'');',
    'end;',
    '',
    'end.');

  cRoutineTooDeeplyNestedCompliant: array[0..32] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Four nested ifs (depth 4, exactly the limit) plus a long else-if ladder that',
    '// must stay flat at depth 1 -- neither is flagged.',
    'procedure Shallow(a, b, c, d: Boolean; n: Integer);',
    '',
    'begin',
    '  if a then',
    '    if b then',
    '      if c then',
    '        if d then',
    '          Writeln(''depth4'');',
    '  if n = 1 then',
    '    Writeln(''one'')',
    '  else if n = 2 then',
    '    Writeln(''two'')',
    '  else if n = 3 then',
    '    Writeln(''three'')',
    '  else if n = 4 then',
    '    Writeln(''four'')',
    '  else if n = 5 then',
    '    Writeln(''five'')',
    '  else',
    '    Writeln(''many'');',
    'end;',
    '',
    'end.');

  cRoutineTooDeeplyNestedNoncompliant: array[0..20] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    '// Five nested if statements => nesting depth 5, one over the limit of 4.',
    'procedure Deep(a, b, c, d, e: Boolean);',
    '',
    'begin',
    '  if a then',
    '    if b then',
    '      if c then',
    '        if d then',
    '          if e then',
    '            Writeln(''deep'');',
    'end;',
    '',
    'end.');

  cTooManyNestedRoutinesCompliant: array[0..26] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Outer;',
    '',
    '  procedure Inner1;',
    '  begin',
    '  end;',
    '',
    '  procedure Inner2;',
    '  begin',
    '  end;',
    '',
    '  procedure Inner3;',
    '  begin',
    '  end;',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyNestedRoutinesNoncompliant: array[0..30] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Outer;',
    '',
    '  procedure Inner1;',
    '  begin',
    '  end;',
    '',
    '  procedure Inner2;',
    '  begin',
    '  end;',
    '',
    '  procedure Inner3;',
    '  begin',
    '  end;',
    '',
    '  procedure Inner4;',
    '  begin',
    '  end;',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyParametersCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Configure(A1: Integer; A2: Integer; A3: Integer; A4: Integer;',
    '  A5: Integer; A6: Integer; A7: Integer);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyParametersNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Configure(A1: Integer; A2: Integer; A3: Integer; A4: Integer;',
    '  A5: Integer; A6: Integer; A7: Integer; A8: Integer);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyVariablesCompliant: array[0..40] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Configure;',
    '',
    'var',
    '  L01: Integer;',
    '  L02: Integer;',
    '  L03: Integer;',
    '  L04: Integer;',
    '  L05: Integer;',
    '  L06: Integer;',
    '  L07: Integer;',
    '  L08: Integer;',
    '  L09: Integer;',
    '  L10: Integer;',
    '  L11: Integer;',
    '  L12: Integer;',
    '  L13: Integer;',
    '  L14: Integer;',
    '  L15: Integer;',
    '',
    'const',
    '  // Consts descend TPasVariable; they must NOT be counted as locals (the',
    '  // `not TPasConst` guard). With 15 vars exactly at the limit, these three',
    '  // consts would push a naive count to 18 (>15) and wrongly flag — so this',
    '  // fixture''s 0-issue assertion locks the guard against regression.',
    '  C1 = 1;',
    '  C2 = 2;',
    '  C3 = 3;',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyVariablesNoncompliant: array[0..32] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Configure;',
    '',
    'var',
    '  L01: Integer;',
    '  L02: Integer;',
    '  L03: Integer;',
    '  L04: Integer;',
    '  L05: Integer;',
    '  L06: Integer;',
    '  L07: Integer;',
    '  L08: Integer;',
    '  L09: Integer;',
    '  L10: Integer;',
    '  L11: Integer;',
    '  L12: Integer;',
    '  L13: Integer;',
    '  L14: Integer;',
    '  L15: Integer;',
    '  L16: Integer;',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyDefaultParametersCompliant: array[0..14] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Configure(A1: Integer = 1; A2: Integer = 2; A3: Integer = 3);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cTooManyDefaultParametersNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'procedure Configure(A1: Integer = 1; A2: Integer = 2; A3: Integer = 3;',
    '  A4: Integer = 4);',
    '',
    'begin',
    'end;',
    '',
    '',
    'end.');

  cBeginEndRequiredCompliant: array[0..23] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  if lValue > 0 then',
    '    begin',
    '      lValue := 1;',
    '    end;',
    'end;',
    '',
    'end.');

  cBeginEndRequiredElseifchain: array[0..29] of string = (
    'unit ElseIfChain;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  if lValue > 0 then',
    '    lValue := 1',
    '  else if lValue < 0 then',
    '    lValue := 2',
    '  else',
    '    lValue := 3;',
    'end;',
    '',
    'initialization',
    '  if 1 > 0 then',
    '    WriteLn(''init'');',
    '',
    'end.');

  cBeginEndRequiredNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  if lValue > 0 then',
    '    lValue := 1;',
    'end;',
    '',
    'end.');

  cNoGotoCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''done'');',
    'end;',
    '',
    'end.');

  cNoGotoNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}{$goto on}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'label',
    '  Done;',
    '',
    'begin',
    '  goto Done;',
    'Done:',
    '  WriteLn(''done'');',
    'end;',
    '',
    'end.');

  cNoWithCompliant: array[0..24] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'type',
    '  TPoint = record',
    '    FValue: Integer;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'var',
    '  lPoint: TPoint;',
    '',
    'begin',
    '  lPoint.FValue := 1;',
    'end;',
    '',
    'end.');

  cNoWithNoncompliant: array[0..27] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'type',
    '  TPoint = record',
    '    FValue: Integer;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'var',
    '  lPoint: TPoint;',
    '',
    'begin',
    '  with lPoint do',
    '    begin',
    '      FValue := 1;',
    '    end;',
    'end;',
    '',
    'end.');

  cNoSelfAssignmentCompliant: array[0..29] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'type',
    '  TRec = record',
    '    B, C: Integer;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'var',
    '  x, y: Integer;',
    '  a: TRec;',
    '  arr: array[0..3] of Integer;',
    '',
    'begin',
    '  x := y;',
    '  a.B := a.C;',
    '  x := x + 1;',
    '  arr[x] := arr[x];',
    'end;',
    '',
    'end.');

  cNoSelfAssignmentCompound: array[0..20] of string = (
    'unit Compound;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  x: Integer;',
    '',
    'begin',
    '  x := 1;',
    '  x += x;',
    'end;',
    '',
    'end.');

  cNoSelfAssignmentDotted: array[0..24] of string = (
    'unit Dotted;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'type',
    '  TRec = record',
    '    B: Integer;',
    '  end;',
    '',
    'procedure Run;',
    '',
    'var',
    '  a: TRec;',
    '',
    'begin',
    '  a.B := a.B;',
    'end;',
    '',
    'end.');

  cNoSelfAssignmentNoncompliant: array[0..20] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  x: Integer;',
    '',
    'begin',
    '  x := 1;',
    '  x := x;',
    'end;',
    '',
    'end.');

  cNoSelfAssignmentSelf: array[0..20] of string = (
    'unit SelfRef;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFoo = class',
    '    F: Integer;',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TFoo.Run;',
    '',
    'begin',
    '  Self.F := Self.F;',
    'end;',
    '',
    'end.');

  cNoInlineAssemblyCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''done'');',
    'end;',
    '',
    'end.');

  cNoInlineAssemblyNoncompliant: array[0..18] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  asm',
    '    nop',
    '  end;',
    'end;',
    '',
    'end.');

  cCaseAtLeastTwoItemsCompliant: array[0..23] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  case lValue of',
    '    1: WriteLn(''one'');',
    '    2: WriteLn(''two'');',
    '  end;',
    'end;',
    '',
    'end.');

  cCaseAtLeastTwoItemsNoncompliant: array[0..24] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  case lValue of',
    '    1: WriteLn(''one'');',
    '  else',
    '    WriteLn(''other'');',
    '  end;',
    'end;',
    '',
    'end.');

  cNoEmptyBlockCompliant: array[0..23] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  if lValue > 0 then',
    '    begin',
    '      lValue := 1;',
    '    end;',
    'end;',
    '',
    'end.');

  cNoEmptyBlockNoncompliant: array[0..22] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  if lValue > 0 then',
    '    begin',
    '    end;',
    'end;',
    '',
    'end.');

  cRoutineNotEmptyCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    'end;',
    '',
    'end.');

  cRoutineNotEmptyNoncompliant: array[0..15] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cUnitNotEmptyCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    'end;',
    '',
    'end.');

  cUnitNotEmptyNoncompliant: array[0..8] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cRedundantJumpCompliant: array[0..24] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lValue: Integer;',
    '',
    'begin',
    '  lValue := 0;',
    '  if lValue > 0 then',
    '    begin',
    '      Exit;',
    '    end;',
    '  WriteLn(''done'');',
    'end;',
    '',
    'end.');

  cRedundantJumpNoncompliant: array[0..17] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    '  Exit;',
    'end;',
    '',
    'end.');

  cRedundantJumpTrailingcontinue: array[0..23] of string = (
    'unit TrailingContinue;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lIndex: Integer;',
    '',
    'begin',
    '  for lIndex := 1 to 3 do',
    '    begin',
    '      WriteLn(lIndex);',
    '      Continue;',
    '    end;',
    'end;',
    '',
    'end.');

  cFunctionReturnTypeRequiredCompliant: array[0..19] of string = (
    'unit Compliant;',
    '',
    '{$mode delphi}',
    '',
    'interface',
    '',
    'type',
    '  TComputer = class',
    '    function Compute: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TComputer.Compute: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'end.');

  cFunctionReturnTypeRequiredNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode delphi}',
    '',
    'interface',
    '',
    'type',
    '  TComputer = class',
    '    function Compute: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TComputer.Compute;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'end.');

  cRedundantBooleanLiteralCompliant: array[0..23] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFlag: Boolean;',
    '',
    'begin',
    '  lFlag := False;',
    '  if lFlag then',
    '    begin',
    '      WriteLn(''yes'');',
    '    end;',
    'end;',
    '',
    'end.');

  cRedundantBooleanLiteralNoncompliant: array[0..23] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lFlag: Boolean;',
    '',
    'begin',
    '  lFlag := False;',
    '  if lFlag = True then',
    '    begin',
    '      WriteLn(''yes'');',
    '    end;',
    'end;',
    '',
    'end.');

  cNilCheckViaAssignedCompliant: array[0..23] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lObj: TObject;',
    '',
    'begin',
    '  lObj := nil;',
    '  if Assigned(lObj) then',
    '    begin',
    '      WriteLn(''set'');',
    '    end;',
    'end;',
    '',
    'end.');

  cNilCheckViaAssignedNoncompliant: array[0..23] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'var',
    '  lObj: TObject;',
    '',
    'begin',
    '  lObj := nil;',
    '  if lObj <> nil then',
    '    begin',
    '      WriteLn(''set'');',
    '    end;',
    'end;',
    '',
    'end.');

  cNoObjectTypesCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TModern = record',
    '    FValue: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cNoObjectTypesNoncompliant: array[0..13] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TLegacy = object',
    '    FValue: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cNoLegacyInitializationSectionCompliant: array[0..18] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    'end;',
    '',
    'initialization',
    '  Run;',
    'end.');

  cNoLegacyInitializationSectionNoncompliant: array[0..18] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    'end;',
    '',
    'begin',
    '  Run;',
    'end.');

  cInlineConstNoTypeInferenceCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    'end;',
    '',
    'end.');

  cInlineConstNoTypeInferenceNoncompliant: array[0..20] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch inlinevars}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  // NOTE: statement-level `const` is a syntax error in the vendored snapshot',
    '  // — the unit fails to parse, so the AST rule never runs.',
    '  const lValue = 1;',
    '  WriteLn(lValue);',
    'end;',
    '',
    'end.');

  cInlineLoopVarNoTypeInferenceCompliant: array[0..20] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch inlinevars}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  for var lIndex: Integer := 1 to 10 do',
    '    begin',
    '      WriteLn(lIndex);',
    '    end;',
    'end;',
    '',
    'end.');

  cInlineLoopVarNoTypeInferenceNoncompliant: array[0..20] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch inlinevars}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  for var lIndex := 1 to 10 do',
    '    begin',
    '      WriteLn(lIndex);',
    '    end;',
    'end;',
    '',
    'end.');

  cInlineVarNoTypeInferenceCompliant: array[0..18] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch inlinevars}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  var lValue: Integer = 1;',
    '  WriteLn(lValue);',
    'end;',
    '',
    'end.');

  cInlineVarNoTypeInferenceNoncompliant: array[0..18] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch inlinevars}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  var lValue := 1;',
    '  WriteLn(lValue);',
    'end;',
    '',
    'end.');

  cProjectFileNoRoutinesCompliant: array[0..6] of string = (
    'program Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'begin',
    '  WriteLn(''work'');',
    'end.');

  cProjectFileNoRoutinesNoncompliant: array[0..12] of string = (
    'program NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'procedure Helper;',
    '',
    'begin',
    '  WriteLn(''work'');',
    'end;',
    '',
    'begin',
    '  Helper;',
    'end.');

  cProjectFileNoVariablesCompliant: array[0..9] of string = (
    'program Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'const',
    '  cTotal = 1;',
    '',
    'begin',
    '  WriteLn(cTotal);',
    'end.');

  cProjectFileNoVariablesNoncompliant: array[0..10] of string = (
    'program NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'var',
    '  lTotal: Integer;',
    '',
    'begin',
    '  lTotal := 1;',
    '  WriteLn(lTotal);',
    'end.');

procedure TRulesStructureTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    RunRule(aRule, lFix.Add(aName, aSrc), aCollector);
  finally
    lFix.Free;
  end;
end;


procedure TRulesStructureTest.RunRuleCfgSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aConfig: TFpSonarConfig;
  const aCollector: TFpSonarIssueCollector);

var
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    RunRuleCfg(aRule, lFix.Add(aName, aSrc), aConfig, aCollector);
  finally
    lFix.Free;
  end;
end;


procedure TRulesStructureTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


procedure TRulesStructureTest.RunRuleCfg(aRule: TRuleBase;
  const aFixture: string; const aConfig: TFpSonarConfig;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := aConfig;
    lEngine.Analyze(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
      aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesStructureTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesStructureTest.FirstById(
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


function TRulesStructureTest.NewCyclomatic: TRuleBase;

begin
  Result := TRuleCyclomaticComplexity.Create(TRuleMetadata.Make(cCyclomaticId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewCognitive: TRuleBase;

begin
  Result := TRuleCognitiveComplexity.Create(TRuleMetadata.Make(cCognitiveId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewRoutineTooLarge: TRuleBase;

begin
  Result := TRuleRoutineTooLarge.Create(TRuleMetadata.Make(cRoutineTooLargeId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewRoutineTooDeeplyNested: TRuleBase;

begin
  Result := TRuleRoutineTooDeeplyNested.Create(TRuleMetadata.Make(
    cRoutineTooDeeplyNestedId, rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewTooManyNestedRoutines: TRuleBase;

begin
  Result := TRuleTooManyNestedRoutines.Create(TRuleMetadata.Make(
    cTooManyNestedRoutinesId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewTooManyParameters: TRuleBase;

begin
  Result := TRuleTooManyParameters.Create(TRuleMetadata.Make(cTooManyParametersId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewTooManyVariables: TRuleBase;

begin
  Result := TRuleTooManyVariables.Create(TRuleMetadata.Make(cTooManyVariablesId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewTooManyDefaultParameters: TRuleBase;

begin
  Result := TRuleTooManyDefaultParameters.Create(TRuleMetadata.Make(
    cTooManyDefaultParametersId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewBeginEndRequired: TRuleBase;

begin
  Result := TRuleBeginEndRequired.Create(TRuleMetadata.Make(cBeginEndRequiredId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewNoGoto: TRuleBase;

begin
  Result := TRuleNoGoto.Create(TRuleMetadata.Make(cNoGotoId, rtAst, rfAst, sevMajor,
    itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewNoWith: TRuleBase;

begin
  Result := TRuleNoWith.Create(TRuleMetadata.Make(cNoWithId, rtAst, rfAst, sevMajor,
    itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewNoSelfAssignment: TRuleBase;

begin
  Result := TRuleNoSelfAssignment.Create(TRuleMetadata.Make(cNoSelfAssignmentId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewNoInlineAssembly: TRuleBase;

begin
  Result := TRuleNoInlineAssembly.Create(TRuleMetadata.Make(cNoInlineAssemblyId,
    rtAst, rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewCaseAtLeastTwoItems: TRuleBase;

begin
  Result := TRuleCaseAtLeastTwoItems.Create(TRuleMetadata.Make(cCaseAtLeastTwoItemsId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewNoEmptyBlock: TRuleBase;

begin
  Result := TRuleNoEmptyBlock.Create(TRuleMetadata.Make(cNoEmptyBlockId, rtAst, rfAst,
    sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewRoutineNotEmpty: TRuleBase;

begin
  Result := TRuleRoutineNotEmpty.Create(TRuleMetadata.Make(cRoutineNotEmptyId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewUnitNotEmpty: TRuleBase;

begin
  Result := TRuleUnitNotEmpty.Create(TRuleMetadata.Make(cUnitNotEmptyId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewRedundantJump: TRuleBase;

begin
  Result := TRuleRedundantJump.Create(TRuleMetadata.Make(cRedundantJumpId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewFunctionReturnTypeRequired: TRuleBase;

begin
  Result := TRuleFunctionReturnTypeRequired.Create(TRuleMetadata.Make(
    cFunctionReturnTypeRequiredId, rtAst, rfAst, sevMajor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewRedundantBooleanLiteral: TRuleBase;

begin
  Result := TRuleRedundantBooleanLiteral.Create(TRuleMetadata.Make(
    cRedundantBooleanLiteralId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewNilCheckViaAssigned: TRuleBase;

begin
  Result := TRuleNilCheckViaAssigned.Create(TRuleMetadata.Make(
    cNilCheckViaAssignedId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewNoObjectTypes: TRuleBase;

begin
  Result := TRuleNoObjectTypes.Create(TRuleMetadata.Make(
    cNoObjectTypesId, rtAst, rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesStructureTest.NewNoLegacyInitializationSection: TRuleBase;

begin
  Result := TRuleNoLegacyInitializationSection.Create(TRuleMetadata.Make(
    cNoLegacyInitializationSectionId, rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesStructureTest.NewInlineConstNoTypeInference: TRuleBase;

begin
  Result := TRuleInlineConstNoTypeInference.Create(TRuleMetadata.Make(
    cInlineConstNoTypeInferenceId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewInlineLoopVarNoTypeInference: TRuleBase;

begin
  Result := TRuleInlineLoopVarNoTypeInference.Create(TRuleMetadata.Make(
    cInlineLoopVarNoTypeInferenceId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewInlineVarNoTypeInference: TRuleBase;

begin
  Result := TRuleInlineVarNoTypeInference.Create(TRuleMetadata.Make(
    cInlineVarNoTypeInferenceId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewProjectFileNoRoutines: TRuleBase;

begin
  Result := TRuleProjectFileNoRoutines.Create(TRuleMetadata.Make(
    cProjectFileNoRoutinesId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesStructureTest.NewProjectFileNoVariables: TRuleBase;

begin
  Result := TRuleProjectFileNoVariables.Create(TRuleMetadata.Make(
    cProjectFileNoVariablesId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


procedure TRulesStructureTest.CheckVolumeRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aValue, aLimit: string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at the routine declaration line, column 1
    // (AST nodes carry no column), carrying [computed value, limit] as args.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('two message args', 2, Length(lc.Issues[k].MessageArgs));
      AssertEquals('arg 0 is the computed value', aValue,
        lc.Issues[k].MessageArgs[0]);
      AssertEquals('arg 1 is the limit', aLimit, lc.Issues[k].MessageArgs[1]);
    finally
      lc.Free;
    end;

    // Compliant: a routine exactly at the threshold => nothing flagged.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesStructureTest.CheckStmtRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at aDeclLine, column 1 (AST nodes carry no
    // column), carrying aArgs as the message args.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
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

    // Compliant: nothing flagged.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesStructureTest.CyclomaticComplexityPositions;

begin
  // Noncompliant: Complex (decl line 11) scores 11 (base 1 + 10 ifs), limit 10.
  CheckVolumeRuleSrc(NewCyclomatic, NewCyclomatic, cCyclomaticId, 11, '11', '10', cCyclomaticNoncompliant, cCyclomaticCompliant);
end;


procedure TRulesStructureTest.CognitiveComplexityPositions;

begin
  // Noncompliant: Tangled (decl line 19) scores 16, limit 15.
  CheckVolumeRuleSrc(NewCognitive, NewCognitive, cCognitiveId, 19, '16', '15', cCognitiveNoncompliant, cCognitiveCompliant);
end;


procedure TRulesStructureTest.RoutineTooLargePositions;

begin
  // Noncompliant: Big (decl line 10) has 61 statements, limit 60.
  CheckVolumeRuleSrc(NewRoutineTooLarge, NewRoutineTooLarge, cRoutineTooLargeId,
    10, '61', '60', cRoutineTooLargeNoncompliant, cRoutineTooLargeCompliant);
end;


procedure TRulesStructureTest.RoutineTooDeeplyNestedPositions;

begin
  // Noncompliant: Deep (decl line 10) reaches nesting depth 5, limit 4.
  CheckVolumeRuleSrc(NewRoutineTooDeeplyNested, NewRoutineTooDeeplyNested,
    cRoutineTooDeeplyNestedId, 10, '5', '4', cRoutineTooDeeplyNestedNoncompliant, cRoutineTooDeeplyNestedCompliant);
end;


procedure TRulesStructureTest.TooManyNestedRoutinesPositions;

begin
  // Noncompliant: Outer (decl line 9) has 4 nested routines, limit 3.
  CheckVolumeRuleSrc(NewTooManyNestedRoutines, NewTooManyNestedRoutines,
    cTooManyNestedRoutinesId, 9, '4', '3', cTooManyNestedRoutinesNoncompliant, cTooManyNestedRoutinesCompliant);
end;


procedure TRulesStructureTest.TooManyParametersPositions;

begin
  // Noncompliant: Configure (decl line 9) has 8 parameters, limit 7.
  CheckVolumeRuleSrc(NewTooManyParameters, NewTooManyParameters,
    cTooManyParametersId, 9, '8', '7', cTooManyParametersNoncompliant, cTooManyParametersCompliant);
end;


procedure TRulesStructureTest.TooManyVariablesPositions;

begin
  // Noncompliant: Configure (decl line 9) has 16 local variables, limit 15.
  // Compliant: 15 vars (at limit) PLUS 3 consts — its 0-issue assertion locks
  // the mandatory 'not TPasConst' guard (consts descend TPasVariable).
  CheckVolumeRuleSrc(NewTooManyVariables, NewTooManyVariables,
    cTooManyVariablesId, 9, '16', '15', cTooManyVariablesNoncompliant, cTooManyVariablesCompliant);
end;


procedure TRulesStructureTest.TooManyDefaultParametersPositions;

begin
  // Noncompliant: Configure (decl line 9) has 4 default parameters, limit 3.
  CheckVolumeRuleSrc(NewTooManyDefaultParameters, NewTooManyDefaultParameters,
    cTooManyDefaultParametersId, 9, '4', '3', cTooManyDefaultParametersNoncompliant, cTooManyDefaultParametersCompliant);
end;


procedure TRulesStructureTest.BeginEndRequiredPositions;

begin
  // Noncompliant: the single-statement if then-branch (line 19) lacks a
  // begin..end block; arg is the control kind 'if'.
  CheckStmtRuleSrc(NewBeginEndRequired, NewBeginEndRequired, cBeginEndRequiredId,
    19, ['if'], cBeginEndRequiredNoncompliant, cBeginEndRequiredCompliant);
end;


procedure TRulesStructureTest.BeginEndRequiredExemptsElseIfAndReachesInit;

var
  lc: TFpSonarIssueCollector;
  i, lInitHits, lOuterElseHits: Integer;

begin
  // Locks two load-bearing carve-outs in ONE assertion (elseifchain.pas):
  //   if .. else if .. else  (three single-statement bodies in a routine)
  //   + a single-statement 'if' body in the INITIALIZATION section.
  // Expected exactly 4 issues: outer-if body (19), inner-if body (21),
  // inner-else body (23), init-section if body (28). The outer 'else if'
  // continuation is EXEMPT (not flagged as an 'else'). The count of 4 alone
  // distinguishes every failure mode: a broken else-if exemption would flag the
  // outer else too (5); a statement walk that missed init/final sections would
  // see only the routine body (3).
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewBeginEndRequired, 'elseifchain.pas', cBeginEndRequiredElseifchain, lc);
    AssertEquals('elseif chain + init: four issues', 4,
      CountById(lc, cBeginEndRequiredId));
    lInitHits := 0;
    lOuterElseHits := 0;
    for i := 0 to lc.Count - 1 do
      if lc.Issues[i].RuleId = cBeginEndRequiredId then
        begin
          // The init-section body proves the rule reaches statements outside
          // routine bodies (the raison d'etre of EnumerateStatementRoots).
          if lc.Issues[i].StartLine = 28 then
            Inc(lInitHits);
          // The outer 'else if' line (20) must NEVER be flagged: that is the
          // chained-else-if exemption.
          if lc.Issues[i].StartLine = 20 then
            Inc(lOuterElseHits);
        end;
    AssertEquals('init-section body reached', 1, lInitHits);
    AssertEquals('chained else-if not flagged', 0, lOuterElseHits);
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.NoGotoPositions;

begin
  // Noncompliant: the goto statement (line 17); no message args.
  CheckStmtRuleSrc(NewNoGoto, NewNoGoto, cNoGotoId, 17, [], cNoGotoNoncompliant, cNoGotoCompliant);
end;


procedure TRulesStructureTest.NoWithPositions;

begin
  // Noncompliant: the with statement (line 22); no message args.
  CheckStmtRuleSrc(NewNoWith, NewNoWith, cNoWithId, 22, [], cNoWithNoncompliant, cNoWithCompliant);
end;


procedure TRulesStructureTest.NoSelfAssignmentPositions;

begin
  // Noncompliant: the identifier self-assignment x := x (line 18); no args.
  // Compliant guards (x := y, a.B := a.C, x := x + 1) emit nothing.
  CheckStmtRuleSrc(NewNoSelfAssignment, NewNoSelfAssignment, cNoSelfAssignmentId,
    18, [], cNoSelfAssignmentNoncompliant, cNoSelfAssignmentCompliant);
end;


procedure TRulesStructureTest.NoSelfAssignmentFlagsDottedMemberChain;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The member-access half of NoSelfAssignment (its own fixture): a.B := a.B
  // (line 22) exercises the eopSubIdent recursion in SameLValue.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoSelfAssignment, 'dotted.pas', cNoSelfAssignmentDotted, lc);
    AssertEquals('one issue', 1, CountById(lc, cNoSelfAssignmentId));
    k := FirstById(lc, cNoSelfAssignmentId);
    AssertEquals('start line', 22, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.NoSelfAssignmentFlagsSelfMemberAccess;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Self.F := Self.F must flag. Self parses as TSelfExpr (not a pekIdent
  // leaf), so this exercises the dedicated TSelfExpr arm of SameLValue.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoSelfAssignment, 'self.pas', cNoSelfAssignmentSelf, lc);
    AssertEquals('one issue', 1, CountById(lc, cNoSelfAssignmentId));
    k := FirstById(lc, cNoSelfAssignmentId);
    AssertEquals('start line', 18, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.NoSelfAssignmentIgnoresCompoundAssign;

var
  lc: TFpSonarIssueCollector;

begin
  // A compound assignment x += x must never be flagged. The vendored
  // scanner does not enable po_CAssignments, so '+=' is rejected before the
  // rule runs; either way the rule emits nothing for the compound form.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewNoSelfAssignment, 'compound.pas', cNoSelfAssignmentCompound, lc);
    AssertEquals('compound => zero', 0, CountById(lc, cNoSelfAssignmentId));
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.NoInlineAssemblyPositions;

begin
  // Noncompliant: the asm..end block (line 14); no message args.
  CheckStmtRuleSrc(NewNoInlineAssembly, NewNoInlineAssembly, cNoInlineAssemblyId,
    14, [], cNoInlineAssemblyNoncompliant, cNoInlineAssemblyCompliant);
end;


procedure TRulesStructureTest.CaseAtLeastTwoItemsPositions;

begin
  // Noncompliant: the one-branch-plus-else case (line 18); arg is the branch
  // count '1'.
  CheckStmtRuleSrc(NewCaseAtLeastTwoItems, NewCaseAtLeastTwoItems,
    cCaseAtLeastTwoItemsId, 18, ['1'], cCaseAtLeastTwoItemsNoncompliant, cCaseAtLeastTwoItemsCompliant);
end;


procedure TRulesStructureTest.NoEmptyBlockPositions;

begin
  // Noncompliant: the empty if-branch begin..end (line 19); no message args.
  // The enclosing Run routine is non-empty (RoutineNotEmpty stays silent) and
  // the empty block is a CHILD statement, not a routine-body root.
  CheckStmtRuleSrc(NewNoEmptyBlock, NewNoEmptyBlock, cNoEmptyBlockId, 19, [], cNoEmptyBlockNoncompliant, cNoEmptyBlockCompliant);
end;


procedure TRulesStructureTest.RoutineNotEmptyPositions;

begin
  // Noncompliant: Run (decl line 11) has an empty body block; arg is the name.
  // The body block is a ROOT, so NoEmptyBlock never sees it (the NoEmptyBlock/
  // RoutineNotEmpty partition).
  CheckStmtRuleSrc(NewRoutineNotEmpty, NewRoutineNotEmpty, cRoutineNotEmptyId, 11,
    ['Run'], cRoutineNotEmptyNoncompliant, cRoutineNotEmptyCompliant);
end;


procedure TRulesStructureTest.UnitNotEmptyPositions;

begin
  // Noncompliant: the wholly empty unit, reported at the 'unit' keyword (line 1);
  // arg is the module name the parser records.
  CheckStmtRuleSrc(NewUnitNotEmpty, NewUnitNotEmpty, cUnitNotEmptyId, 1,
    ['NonCompliant'], cUnitNotEmptyNoncompliant, cUnitNotEmptyCompliant);
end;


procedure TRulesStructureTest.RedundantJumpPositions;

begin
  // Noncompliant: the bare trailing Exit (line 15); arg is the jump kind 'exit'.
  CheckStmtRuleSrc(NewRedundantJump, NewRedundantJump, cRedundantJumpId, 15,
    ['exit'], cRedundantJumpNoncompliant, cRedundantJumpCompliant);
end;


procedure TRulesStructureTest.RedundantJumpFlagsTrailingContinueInLoop;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The loop half of RedundantJump (its own fixture; CheckStmtRule reads
  // noncompliant.pas only): a bare trailing Continue (line 20) as the last
  // statement of a for body.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewRedundantJump, 'trailingcontinue.pas', cRedundantJumpTrailingcontinue, lc);
    AssertEquals('one issue', 1, CountById(lc, cRedundantJumpId));
    k := FirstById(lc, cRedundantJumpId);
    AssertEquals('start line', 20, lc.Issues[k].StartLine);
    AssertEquals('arg count', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('kind', 'continue', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.FunctionReturnTypeRequiredPositions;

begin
  // Noncompliant: a Delphi-mode class-method implementation that omits the result
  // type (decl line 14) — the only form this parser accepts while leaving the
  // result-type node nil (a bare 'function Name;' is a hard parse error). Arg is
  // the qualified routine name the parser records.
  CheckStmtRuleSrc(NewFunctionReturnTypeRequired, NewFunctionReturnTypeRequired,
    cFunctionReturnTypeRequiredId, 14, ['TComputer.Compute'], cFunctionReturnTypeRequiredNoncompliant, cFunctionReturnTypeRequiredCompliant);
end;


procedure TRulesStructureTest.RedundantBooleanLiteralPositions;

begin
  // Noncompliant: 'if lFlag = True then' (line 18); no args. The 'lFlag := False;'
  // assignment above is NOT a comparison and must not fire. Line CLI-probed.
  CheckStmtRuleSrc(NewRedundantBooleanLiteral, NewRedundantBooleanLiteral,
    cRedundantBooleanLiteralId, 18, [], cRedundantBooleanLiteralNoncompliant, cRedundantBooleanLiteralCompliant);
end;


procedure TRulesStructureTest.NilCheckViaAssignedPositions;

begin
  // Noncompliant: 'if lObj <> nil then' (line 18); no args. The 'lObj := nil;'
  // assignment above is NOT a comparison. Compliant uses Assigned() (FP guard).
  CheckStmtRuleSrc(NewNilCheckViaAssigned, NewNilCheckViaAssigned,
    cNilCheckViaAssignedId, 18, [], cNilCheckViaAssignedNoncompliant, cNilCheckViaAssignedCompliant);
end;


procedure TRulesStructureTest.NoObjectTypesPositions;

begin
  // Noncompliant: 'TLegacy = object' (decl line 8); arg is the type name.
  CheckStmtRuleSrc(NewNoObjectTypes, NewNoObjectTypes, cNoObjectTypesId, 8,
    ['TLegacy'], cNoObjectTypesNoncompliant, cNoObjectTypesCompliant);
end;


procedure TRulesStructureTest.NoLegacyInitializationSectionPositions;

begin
  // Noncompliant: the trailing 'begin' unit body (line 17); no args. The compliant
  // fixture's real 'initialization' section yields 0 (load-bearing FP guard).
  CheckStmtRuleSrc(NewNoLegacyInitializationSection,
    NewNoLegacyInitializationSection, cNoLegacyInitializationSectionId, 17, [], cNoLegacyInitializationSectionNoncompliant, cNoLegacyInitializationSectionCompliant);
end;


procedure TRulesStructureTest.InlineVarNoTypeInferencePositions;

begin
  // Noncompliant: 'var lValue := 1;' (line 15, CLI-probed); arg is the var name.
  // Compliant uses an explicitly-typed inline var ('var lValue: Integer = 1;' —
  // VarType <> nil), a load-bearing false-positive guard.
  CheckStmtRuleSrc(NewInlineVarNoTypeInference, NewInlineVarNoTypeInference,
    cInlineVarNoTypeInferenceId, 15, ['lValue'], cInlineVarNoTypeInferenceNoncompliant, cInlineVarNoTypeInferenceCompliant);
end;


procedure TRulesStructureTest.InlineLoopVarNoTypeInferencePositions;

begin
  // Noncompliant: 'for var lIndex := 1 to 10 do' (line 15, CLI-probed); no args.
  // Compliant uses an explicitly-typed inline loop var (FP guard).
  CheckStmtRuleSrc(NewInlineLoopVarNoTypeInference,
    NewInlineLoopVarNoTypeInference, cInlineLoopVarNoTypeInferenceId, 15, [], cInlineLoopVarNoTypeInferenceNoncompliant, cInlineLoopVarNoTypeInferenceCompliant);
end;


procedure TRulesStructureTest.ProjectFileNoRoutinesPositions;

begin
  // Noncompliant: 'procedure Helper;' in a program (line 5, CLI-probed); arg name.
  // Compliant is a program with no routine (FP guard).
  CheckStmtRuleSrc(NewProjectFileNoRoutines, NewProjectFileNoRoutines,
    cProjectFileNoRoutinesId, 5, ['Helper'], cProjectFileNoRoutinesNoncompliant, cProjectFileNoRoutinesCompliant);
end;


procedure TRulesStructureTest.ProjectFileNoVariablesPositions;

begin
  // Noncompliant: 'lTotal: Integer;' in a program (line 6, CLI-probed — the
  // declared-name line, not the 'var' keyword line); arg name. Compliant is a
  // program with only a const (FP guard — consts are not flagged).
  CheckStmtRuleSrc(NewProjectFileNoVariables, NewProjectFileNoVariables,
    cProjectFileNoVariablesId, 6, ['lTotal'], cProjectFileNoVariablesNoncompliant, cProjectFileNoVariablesCompliant);
end;


procedure TRulesStructureTest.InlineConstNoTypeInferenceIsInert;

var
  lc: TFpSonarIssueCollector;

begin
  // InlineConstNoTypeInference is inert in the vendored snapshot: a
  // statement-level 'const' is a syntax error, so neither fixture produces a
  // finding. We assert
  // the rule is registered and that it emits nothing on the parse-clean
  // compliant fixture.
  AssertTrue('InlineConstNoTypeInference registered',
    RuleRegistry.FindById(cInlineConstNoTypeInferenceId) <> nil);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewInlineConstNoTypeInference, 'compliant.pas', cInlineConstNoTypeInferenceCompliant, lc);
    AssertEquals('inert => zero', 0,
      CountById(lc, cInlineConstNoTypeInferenceId));
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.ParamOverrideChangesDetection;

var
  lc: TFpSonarIssueCollector;
  lCfg: TFpSonarConfig;
  lErr: string;

begin
  // (a) BeginEndRequired: allowSingleStatement=true suppresses every finding the
  // default behaviour would emit on the noncompliant fixture.
  AssertTrue('cfg (a) loads', lCfg.LoadFromJSON('{"rules":{"BeginEndRequired":{"params":{"allowSingleStatement":true}}}}', lErr));
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleCfgSrc(NewBeginEndRequired, 'noncompliant.pas', cBeginEndRequiredNoncompliant, lCfg, lc);
    AssertEquals('allowSingleStatement=true suppresses BeginEndRequired', 0,
      CountById(lc, cBeginEndRequiredId));
  finally
    lc.Free;
  end;

  // (b) CyclomaticComplexity: a lowered maxComplexity flags the COMPLIANT fixture
  // (which the default limit of 10 leaves clean) — proving the threshold tunes.
  AssertTrue('cfg (b) loads', lCfg.LoadFromJSON('{"rules":{"CyclomaticComplexity":{"params":{"maxComplexity":0}}}}', lErr));
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleCfgSrc(NewCyclomatic, 'compliant.pas', cCyclomaticCompliant, lCfg, lc);
    AssertTrue('lowered maxComplexity flags otherwise-compliant code',
      CountById(lc, cCyclomaticId) > 0);
  finally
    lc.Free;
  end;
end;


procedure TRulesStructureTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all four AST volume rules into the
  // GLOBAL registry (this is what the CLI process runs).
  AssertTrue('CyclomaticComplexity registered',
    RuleRegistry.FindById(cCyclomaticId) <> nil);
  AssertTrue('CognitiveComplexity registered',
    RuleRegistry.FindById(cCognitiveId) <> nil);
  AssertTrue('RoutineTooLarge registered',
    RuleRegistry.FindById(cRoutineTooLargeId) <> nil);
  AssertTrue('RoutineTooDeeplyNested registered',
    RuleRegistry.FindById(cRoutineTooDeeplyNestedId) <> nil);
  AssertTrue('TooManyNestedRoutines registered',
    RuleRegistry.FindById(cTooManyNestedRoutinesId) <> nil);
  AssertTrue('TooManyParameters registered',
    RuleRegistry.FindById(cTooManyParametersId) <> nil);
  AssertTrue('TooManyVariables registered',
    RuleRegistry.FindById(cTooManyVariablesId) <> nil);
  AssertTrue('TooManyDefaultParameters registered',
    RuleRegistry.FindById(cTooManyDefaultParametersId) <> nil);
  AssertTrue('BeginEndRequired registered',
    RuleRegistry.FindById(cBeginEndRequiredId) <> nil);
  AssertTrue('NoGoto registered',
    RuleRegistry.FindById(cNoGotoId) <> nil);
  AssertTrue('NoWith registered',
    RuleRegistry.FindById(cNoWithId) <> nil);
  AssertTrue('NoSelfAssignment registered',
    RuleRegistry.FindById(cNoSelfAssignmentId) <> nil);
  AssertTrue('NoInlineAssembly registered',
    RuleRegistry.FindById(cNoInlineAssemblyId) <> nil);
  AssertTrue('CaseAtLeastTwoItems registered',
    RuleRegistry.FindById(cCaseAtLeastTwoItemsId) <> nil);
  AssertTrue('NoEmptyBlock registered',
    RuleRegistry.FindById(cNoEmptyBlockId) <> nil);
  AssertTrue('RoutineNotEmpty registered',
    RuleRegistry.FindById(cRoutineNotEmptyId) <> nil);
  AssertTrue('UnitNotEmpty registered',
    RuleRegistry.FindById(cUnitNotEmptyId) <> nil);
  AssertTrue('RedundantJump registered',
    RuleRegistry.FindById(cRedundantJumpId) <> nil);
  AssertTrue('FunctionReturnTypeRequired registered',
    RuleRegistry.FindById(cFunctionReturnTypeRequiredId) <> nil);
  AssertTrue('RedundantBooleanLiteral registered',
    RuleRegistry.FindById(cRedundantBooleanLiteralId) <> nil);
  AssertTrue('NilCheckViaAssigned registered',
    RuleRegistry.FindById(cNilCheckViaAssignedId) <> nil);
  AssertTrue('NoObjectTypes registered',
    RuleRegistry.FindById(cNoObjectTypesId) <> nil);
  AssertTrue('NoLegacyInitializationSection registered',
    RuleRegistry.FindById(cNoLegacyInitializationSectionId) <> nil);
  AssertTrue('InlineConstNoTypeInference registered',
    RuleRegistry.FindById(cInlineConstNoTypeInferenceId) <> nil);
  AssertTrue('InlineLoopVarNoTypeInference registered',
    RuleRegistry.FindById(cInlineLoopVarNoTypeInferenceId) <> nil);
  AssertTrue('InlineVarNoTypeInference registered',
    RuleRegistry.FindById(cInlineVarNoTypeInferenceId) <> nil);
  AssertTrue('ProjectFileNoRoutines registered',
    RuleRegistry.FindById(cProjectFileNoRoutinesId) <> nil);
  AssertTrue('ProjectFileNoVariables registered',
    RuleRegistry.FindById(cProjectFileNoVariablesId) <> nil);
end;


initialization
  RegisterTest(TRulesStructureTest);

end.
