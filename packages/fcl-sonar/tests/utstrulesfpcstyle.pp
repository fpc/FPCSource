{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the TOK-tier FPC/FCL source-base convention rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesFpcStyle;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Resolver, FpSonar.Traversal, FpSonar.Engine,
  FpSonar.Rules.FpcStyle, UtstFixtures;

type
  { FpcStyle rule position, silence, degradation and registration tests. }
  TRulesFpcStyleTest = class(TTestCase)
  private
    { Runs aRule (taken into a fresh local registry, freed here) over aFixture
      with the rule explicitly enabled, collecting into aCollector. aWithhold
      withholds resolution the way the silence sweep's degraded pass does. }
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector; aWithhold: boolean = False);
    // As RunRule, but the fixture source is supplied inline and materialised to
    // a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector;
      aWithhold: boolean = False);
    // A config enabling aRuleId and nothing else.
    function EnabledConfig(const aRuleId: string): TFpSonarConfig;
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // The index of the aNth (0-based) issue carrying aId, or -1.
    function NthById(const aCollector: TFpSonarIssueCollector;
      const aId: string; aNth: Integer): Integer;
    // How often aId fires carrying aArg as its single message argument.
    function CountArg(const aCollector: TFpSonarIssueCollector;
      const aId, aArg: string): Integer;
    // Builds a project-wide index over aFiles, caller frees it.
    function BuildIndex(const aFiles: array of string): TFpSonarProjectIndex;
    // Runs aRule over aSubject with aIndex attached as the project index.
    procedure RunRuleWithIndex(aRule: TRuleBase; const aSubject: string;
      aIndex: TFpSonarProjectIndex;
      const aCollector: TFpSonarIssueCollector);
    // Materialises the six iub_*.pas fixtures into aTmp, iub_user first.
    function WriteUsesProject(aTmp: TTempFixtures): TStringArray;
    // How often aId fires when aRule runs over the inline source aSrc.
    function CountSrc(aRule: TRuleBase; const aName, aId: string;
      const aSrc: array of string; aWithhold: boolean = False): Integer;
    // Asserts aSrc resolves clean, so a zero-issue assert over it is a
    // measurement and not a vacuous silent-skip.
    procedure AssertResolvesClean(const aName: string;
      const aSrc: array of string);
    // Asserts aRule is silent on aSrc while aSibling still fires once.
    procedure CheckSilentWithLiveSibling(aRule, aSibling: TRuleBase;
      const aName, aId, aSiblingId: string; const aSrc: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewBranches: TRuleBase;
    function NewMissingGuard: TRuleBase;
    function NewAliasMismatch: TRuleBase;
    function NewCaseMismatch: TRuleBase;
    function NewMissingMode: TRuleBase;
    function NewMissingCopyright: TRuleBase;
    function NewDeprecatedSymbol: TRuleBase;
    function NewPlatformSymbol: TRuleBase;
    function NewExperimentalSymbol: TRuleBase;
    function NewMethodUndocumented: TRuleBase;
    function NewPropertyUndocumented: TRuleBase;
    function NewUsesTooBroad: TRuleBase;
    function NewIOResultNotChecked: TRuleBase;
  published
    procedure DottedUnitsBranchesInconsistentPositions;
    procedure DottedUnitsBranchesInconsistentSurvivesParseFailure;
    procedure DottedUnitsBranchesInconsistentSilentShapes;
    procedure MissingDottedUnitsGuardPositions;
    procedure MissingDottedUnitsGuardSurvivesParseFailure;
    procedure MissingDottedUnitsGuardIndirectSatisfaction;
    procedure MissingDottedUnitsGuardIgnoresCommentAndStringMentions;
    procedure DottedUnitAliasMismatchPositions;
    procedure DottedUnitAliasMismatchSilentOnUnknownName;
    procedure DottedUnitAliasMismatchSilentOnLengthMismatch;
    procedure DottedUnitAliasMismatchSilentOnReorderedBranches;
    procedure DottedUnitAliasMismatchReportsThroughAReorderedBranch;
    procedure DottedUnitAliasMismatchSilentWithoutGuard;
    procedure DottedUnitAliasMismatchSurvivesParseFailure;
    procedure DottedUnitAliasMismatchRegistersDisabled;
    procedure TwoClausesInOneFileArePairedApart;
    procedure ParenFormDirectivesAreDirectives;
    procedure ElseIfAtGuardLevelStopsTheComparison;
    procedure IncludeInsideGuardStopsTheComparison;
    procedure DuplicateTailEntryIsNoMissingUnit;
    procedure UnitFileNameCaseMismatchPositions;
    procedure UnitFileNameCaseMismatchSilentOnNamespacedMirror;
    procedure UnitFileNameCaseMismatchSilentOnProgramAndLibrary;
    procedure UnitFileNameCaseMismatchSurvivesParseFailure;
    procedure MissingModeDirectivePositions;
    procedure MissingModeDirectiveIndirectSatisfaction;
    procedure MissingModeDirectiveSurvivesParseFailure;
    procedure MissingCopyrightHeaderPositions;
    procedure MissingCopyrightHeaderIndirectSatisfaction;
    procedure MissingCopyrightHeaderSurvivesParseFailure;
    procedure UnitHygieneRulesSilentWithoutModuleKeyword;
    procedure UnitHygieneRulesSilentOnTruncatedModuleClause;
    procedure UnitHygieneRulesSilentOnBodyWordModuleName;
    procedure UnitHygieneRulesSilentOnUndelimitedWindow;
    procedure UnitHygieneRulesEitherGuardedClauseSatisfies;
    procedure UnitHygieneRulesIgnoreIoCheckDirectives;
    procedure UnitHygieneRulesRegisterDisabled;
    procedure DeprecatedSymbolUsedPositions;
    procedure DeprecatedSymbolUsedDegradesWithoutResolver;
    procedure DeprecatedSymbolUsedSilentOnUnresolvedOperand;
    procedure DeprecatedSymbolUsedRegistersDisabled;
    procedure PlatformSymbolUsedInPortableUnitPositions;
    procedure PlatformSymbolUsedInPortableUnitDegradesWithoutResolver;
    procedure PlatformSymbolUsedInPortableUnitSilentOnUnresolvedOperand;
    procedure PlatformSymbolUsedInPortableUnitRegistersDisabled;
    procedure ExperimentalSymbolUsedPositions;
    procedure ExperimentalSymbolUsedDegradesWithoutResolver;
    procedure ExperimentalSymbolUsedSilentOnUnresolvedOperand;
    procedure ExperimentalSymbolUsedRegistersDisabled;
    procedure PublicMethodUndocumentedPositions;
    procedure PublicMethodUndocumentedIndirectSatisfaction;
    procedure PublicMethodUndocumentedWalkStaysAligned;
    procedure PublicMethodUndocumentedReportsInDelphiMode;
    procedure PublicMethodUndocumentedSurvivesParseFailure;
    procedure PublicMethodUndocumentedRegistersDisabled;
    procedure PublicPropertyUndocumentedPositions;
    procedure PublicPropertyUndocumentedIndirectSatisfaction;
    procedure PublicPropertyUndocumentedReportsInDelphiMode;
    procedure PublicPropertyUndocumentedSurvivesParseFailure;
    procedure PublicPropertyUndocumentedRegistersDisabled;
    procedure InterfaceUsesTooBroadPositions;
    procedure InterfaceUsesTooBroadIndirectSatisfaction;
    procedure InterfaceUsesTooBroadDegradesWithoutIndex;
    procedure InterfaceUsesTooBroadRegistersDisabled;
    procedure IOResultNotCheckedPositions;
    procedure IOResultNotCheckedReadsALowercaseSwitch;
    procedure IOResultNotCheckedSilentOnCheckedCall;
    procedure IOResultNotCheckedSilentOnAnAssignedCheck;
    procedure IOResultNotCheckedSilentOnPossibleIndirectCheck;
    procedure IOResultNotCheckedSilentOutsideTheRegion;
    procedure IOResultNotCheckedSilentAfterTheRegionCloses;
    procedure IOResultNotCheckedSilentWhenTheRegionEndsFirst;
    procedure IOResultNotCheckedDegradesWithoutResolver;
    procedure IOResultNotCheckedSilentOnInlineAssembler;
    procedure IOResultNotCheckedRegistersDisabled;
    procedure FpcStyleRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cBranchesId = 'DottedUnitsBranchesInconsistent';
  cMissingGuardId = 'MissingDottedUnitsGuard';
  cAliasId = 'DottedUnitAliasMismatch';
  cCaseMismatchId = 'UnitFileNameCaseMismatch';
  cMissingModeId = 'MissingModeDirective';
  cMissingCopyrightId = 'MissingCopyrightHeader';
  cDeprecatedId = 'DeprecatedSymbolUsed';
  cPlatformId = 'PlatformSymbolUsedInPortableUnit';
  cExperimentalId = 'ExperimentalSymbolUsed';
  cMethodDocId = 'PublicMethodUndocumented';
  cPropertyDocId = 'PublicPropertyUndocumented';
  cUsesTooBroadId = 'InterfaceUsesTooBroad';
  cIOResultId = 'IOResultNotChecked';
  cErrorId = 'RuleError';
  cScanErrorId = 'ScanError';

  // Embedded fixtures: line i+1 == [i].

  // The dotted branch lists 2 units, the non-dotted 3; the unpaired one is last.
  cBranchesNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes;',
    '{$ELSE}',
    '  SysUtils, Classes, StrUtils;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cBothBranchesCorrect: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes;',
    '{$ELSE}',
    '  SysUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // One unit carries the same name in both branches, one is renamed.
  cIdenticalNameInBothBranches: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, FpSonar.Types;',
    '{$ELSE}',
    '  SysUtils, FpSonar.Types;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cBranchesParseFailure: array[0..18] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes;',
    '{$ELSE}',
    '  SysUtils, Classes, StrUtils;',
    '{$ENDIF}',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  // The table maps fpjson to FpJson.Data, so the dotted branch contradicts it.
  cAliasNoncompliant: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  FpJson.Parser, System.Classes;',
    '{$ELSE}',
    '  fpjson, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cAliasParseFailure: array[0..18] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  FpJson.Parser, System.Classes;',
    '{$ELSE}',
    '  fpjson, Classes;',
    '{$ENDIF}',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  // Neither branch name occurs in the table, so no alias is on record.
  cAliasUnknownName: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  MyApp.Widgets, System.Classes;',
    '{$ELSE}',
    '  mywidgets, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // The wrong alias sits in a branch one unit longer than its counterpart.
  cAliasLengthMismatch: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  FpJson.Parser, System.Classes, System.StrUtils;',
    '{$ELSE}',
    '  fpjson, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // Both recorded aliases are present, in the order the other branch reverses.
  cAliasReordered: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  Pascal.Scanner, FpJson.Data;',
    '{$ELSE}',
    '  fpjson, pscanner;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // Reordered against the other branch and carrying one wrong alias.
  cAliasReorderedWrong: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.Classes, FpJson.Parser;',
    '{$ELSE}',
    '  fpjson, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // The wrong alias of cAliasNoncompliant, in a branch an include makes opaque.
  cAliasOpaqueGuard: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  FpJson.Parser, System.Classes,',
    '{$i dottedtail.inc}',
    '{$ELSE}',
    '  fpjson, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cMissingGuardNoncompliant: array[0..11] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'implementation',
    '',
    'end.');

  cMissingGuardParseFailure: array[0..14] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  // The only mention of the guard is the directive around the unit name.
  cGuardElsewhereOnly: array[0..15] of string = (
    '{$IFNDEF FPC_DOTTEDUNITS}',
    'unit Compliant;',
    '{$ELSE}',
    'unit Fpc.Compliant;',
    '{$ENDIF}',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'implementation',
    '',
    'end.');

  cNoUsesClause: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    '',
    'end.');

  // The symbol occurs in a comment and in a string literal, in no directive.
  cGuardOnlyInCommentAndString: array[0..16] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    '// FPC_DOTTEDUNITS is named here only in a comment',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils, Classes;',
    '',
    'const',
    '  cGuard = ''FPC_DOTTEDUNITS'';',
    '',
    'implementation',
    '',
    'end.');

  cNestedConditionalBothBranches: array[0..19] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils,',
    '{$ifdef NODEJS} Node.FS, {$endif}',
    '  System.Classes;',
    '{$ELSE}',
    '  SysUtils,',
    '{$ifdef NODEJS} nodefs, {$endif}',
    '  Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // A nested conditional in the dotted branch only, after an aligned prefix.
  cNestedArmInOneBranchOnly: array[0..19] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes,',
    '{$ifdef NODEJS}',
    '  Node.FS,',
    '{$endif}',
    '  System.StrUtils;',
    '{$ELSE}',
    '  SysUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cReorderedBranches: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.Classes, System.SysUtils;',
    '{$ELSE}',
    '  SysUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // Two independent differences, so no tail entry sits at an identifiable
  // position: the second pair already fails to correspond.
  cUnalignedPrefix: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes;',
    '{$ELSE}',
    '  SysUtils, StrUtils, Math;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cUnitInsertedMidList: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes;',
    '{$ELSE}',
    '  SysUtils, StrUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // A program clause whose dotted branch carries an "in" file reference.
  cProgramUsesIn: array[0..12] of string = (
    'program Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, Helper in ''helper.pas'';',
    '{$ELSE}',
    '  SysUtils, Helper;',
    '{$ENDIF}',
    '',
    'begin',
    'end.');

  cGuardWithoutElse: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils,',
    '{$ENDIF}',
    '  Classes;',
    '',
    'implementation',
    '',
    '',
    'end.');

  // Two units precede the guard, which carries one branch-local unit per side.
  cGuardInsideClause: array[0..16] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes, SysUtils,',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  Pascal.Scanner;',
    '{$ELSE}',
    '  PScanner;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  cInvertedGuard: array[0..16] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes,',
    '{$IFNDEF FPC_DOTTEDUNITS}',
    '  PScanner;',
    '{$ELSE}',
    '  Pascal.Scanner;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // The dominant tree shape: the guard wraps the whole clause and each branch
  // carries its own uses keyword; two dotted units have no counterpart.
  cCanonicalBranches: array[0..18] of string = (
    '{$IFNDEF FPC_DOTTEDUNITS}',
    'unit Noncompliant;',
    '{$ENDIF FPC_DOTTEDUNITS}',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '{$IFDEF FPC_DOTTEDUNITS}',
    'uses',
    '  System.SysUtils, System.Classes, System.StrUtils, System.Math;',
    '{$ELSE FPC_DOTTEDUNITS}',
    'uses',
    '  SysUtils, Classes;',
    '{$ENDIF FPC_DOTTEDUNITS}',
    '',
    'implementation',
    '',
    'end.');

  // The same placement, with the branches equally long and both aliases the
  // ones the table records.
  cCanonicalAlias: array[0..18] of string = (
    '{$IFNDEF FPC_DOTTEDUNITS}',
    'unit Compliant;',
    '{$ENDIF FPC_DOTTEDUNITS}',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '{$IFDEF FPC_DOTTEDUNITS}',
    'uses',
    '  Pascal.Scanner, FpJson.Data;',
    '{$ELSE FPC_DOTTEDUNITS}',
    'uses',
    '  PScanner, fpjson;',
    '{$ENDIF FPC_DOTTEDUNITS}',
    '',
    'implementation',
    '',
    'end.');

  // An interface clause that pairs and an implementation clause that does not.
  cTwoClausesInOneFile: array[0..22] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes;',
    '{$ELSE}',
    '  SysUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.StrUtils, System.Math;',
    '{$ELSE}',
    '  StrUtils;',
    '{$ENDIF}',
    '',
    'end.');

  // No ';' anywhere: the first guard would be an alias verdict and the second a
  // branch verdict, and the unterminated clause withdraws both.
  cUnterminatedClause: array[0..21] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes,',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  Pascal.Scanner,',
    '{$ELSE}',
    '  PScanner,',
    '{$ENDIF}',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.StrUtils',
    '{$ELSE}',
    '  SysUtils',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // The guard is spelled in the (*$ ... *) form throughout.
  cParenFormGuard: array[0..15] of string = (
    'unit Noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '(*$IFDEF FPC_DOTTEDUNITS*)',
    '  System.SysUtils, System.Classes, System.StrUtils;',
    '(*$ELSE*)',
    '  SysUtils, Classes;',
    '(*$ENDIF*)',
    '',
    'implementation',
    '',
    'end.');

  // A third arm the scan does not model, whose units would inflate one branch.
  cElseIfAtGuardLevel: array[0..17] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes,',
    '{$ELSEIF DEFINED(LEGACY)}',
    '  Legacy.Extra,',
    '{$ELSE}',
    '  SysUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // The dotted branch continues its list in another file.
  cIncludeInsideGuard: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes,',
    '{$i dottedtail.inc}',
    '{$ELSE}',
    '  SysUtils, Classes, StrUtils;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // The dotted branch lists one unit twice, so its tail entry is a duplicate.
  cDuplicateTailEntry: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils, System.Classes, System.SysUtils;',
    '{$ELSE}',
    '  SysUtils, Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // Both branches list Classes and Api.Classes, in opposite order.
  cExactNameBeforeLastComponent: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  Api.Classes, Classes;',
    '{$ELSE}',
    '  Classes, Api.Classes;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // Three comment forms naming units, and a list spread over several lines.
  cCommentsAndMultiLine: array[0..19] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '{$IFDEF FPC_DOTTEDUNITS}',
    '  System.SysUtils,',
    '  System.Classes { StrUtils } (* Contnrs *) // Math',
    '  ;',
    '{$ELSE}',
    '  SysUtils,',
    '  Classes',
    '  ;',
    '{$ENDIF}',
    '',
    'implementation',
    '',
    'end.');

  // Unit-hygiene fixtures. The file name each one is staged under is part of
  // the case.

  // Staged as Surfacething.pas, which is neither spelling the tree records.
  cHygieneCaseMismatch: array[0..11] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit SurfaceThing;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    'end.');

  cHygieneCaseParseFailure: array[0..12] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit SurfaceThing;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  // The generated namespaced mirror: the file name preserves the unit name.
  cHygieneNamespacedMirror: array[0..11] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit System.Macuuid;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    'end.');

  cHygieneLowercaseLayout: array[0..11] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit FpSonar.Rules.FpcStyle;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    'end.');

  // A mixed-case project name, no mode directive and no banner.
  cHygieneProgram: array[0..5] of string = (
    'program SurfaceProgram;',
    '',
    'var',
    '  GBanner: string;',
    'begin',
    'end.');

  // The 7-character module keyword, at line 1 column 1.
  cHygieneLibrary: array[0..5] of string = (
    'library SurfaceLib;',
    '',
    'var',
    '  GBanner: string;',
    'begin',
    'end.');

  // The banner sits below the program keyword and the file declares an
  // interface type.
  cHygieneProgramInterfaceType: array[0..9] of string = (
    'program SurfaceTyped;',
    '',
    '{ See the file COPYING.FPC, included in this distribution. }',
    '',
    'type',
    '  IShape = interface',
    '  end;',
    '',
    'begin',
    'end.');

  // The shape of packages/fcl-base/examples/dsocksvr.pp: the banner opens on
  // the line below the program keyword.
  cHygieneProgramBannerBelowKeyword: array[0..7] of string = (
    'Program server;',
    '{',
    '    This file is part of the Free Component Library (FCL)',
    '',
    '    See the file COPYING.FPC, included in this distribution.',
    '}',
    'begin',
    'end.');

  cHygieneNoMode: array[0..10] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit HygieneNoMode;',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    '',
    'end.');

  cHygieneNoModeParseFailure: array[0..11] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit HygieneNoMode;',
    '',
    'interface',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    '',
    'end.');

  cHygieneModeParenForm: array[0..10] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit HygieneParenMode;',
    '',
    '(*$MODE OBJFPC*)',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'end.');

  cHygieneModeInIfdefArm: array[0..13] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit HygieneArmMode;',
    '',
    '{$ifdef FPC}',
    '{$mode objfpc}{$H+}',
    '{$else}',
    '{$mode delphi}',
    '{$endif}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    'end.');

  // The shape 49 files under packages/*/src are written in.
  cHygieneModeAfterInterface: array[0..9] of string = (
    '{ See the file COPYING.FPC, included in this distribution. }',
    'unit HygieneLateMode;',
    '',
    'interface',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'const',
    '  cX = 1;',
    'end.');

  // The include sits above the interface keyword, so it precedes both windows.
  cHygieneIncludeAboveInterface: array[0..8] of string = (
    'unit HygieneInclude;',
    '',
    '{$i hygiene.inc}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  // The same include below the interface keyword cannot supply the banner,
  // while it may still carry the mode.
  cHygieneIncludeBelowInterface: array[0..8] of string = (
    'unit HygieneLateInclude;',
    '',
    'interface',
    '',
    'implementation',
    '',
    '{$i hygiene.inc}',
    '',
    'end.');

  cHygieneNoBanner: array[0..10] of string = (
    'unit HygieneNoBanner;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    '',
    'implementation',
    'end.');

  cHygieneNoBannerParseFailure: array[0..11] of string = (
    'unit HygieneNoBanner;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  // The fcl-json shape: the banner sits after a guarded module clause.
  cHygieneBannerAfterGuardedUnit: array[0..13] of string = (
    '{$IFNDEF FPC_DOTTEDUNITS}',
    'unit HygieneGuarded;',
    '{$ENDIF}',
    '{',
    '    This file is part of the Free Component Library (FCL)',
    '    See the file COPYING.FPC, included in this distribution.',
    '}',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    'end.');

  cHygieneBannerLineComment: array[0..9] of string = (
    '// See the file COPYING.FPC, included in this distribution.',
    'unit HygieneLineBanner;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    'end.');

  // The marker in the (* *) form and in lower case.
  cHygieneBannerParenComment: array[0..9] of string = (
    '(* see the file copying.fpc, included in this distribution *)',
    'unit HygieneParenBanner;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cX = 1;',
    'end.');

  cHygieneNoModuleKeyword: array[0..3] of string = (
    'const',
    '  cIncludedDepth = 4;',
    '',
    '');

  // The library hint modifier of an include fragment opens no module clause.
  cHygieneHintModifierOnly: array[0..4] of string = (
    'procedure Old; library;',
    '',
    'procedure Current;',
    '',
    '');

  cHygieneTruncatedModule: array[0..1] of string = (
    'unit',
    '');

  // interface stands where the module name would, so the clause has none.
  cHygieneBodyWordAsName: array[0..2] of string = (
    'unit',
    'interface',
    'end.');

  // The generated namespaced mirror: no interface word delimits the window.
  cHygieneNamespacedIncludeMirror: array[0..2] of string = (
    'unit System.Macuuid;',
    '{$DEFINE FPC_DOTTEDUNITS}',
    '{$i macuuid.pp}');

  cHygieneTwoClausesDottedFirst: array[0..11] of string = (
    '{$IFDEF FPC_DOTTEDUNITS}',
    'unit System.HygieneTwo;',
    '{$ELSE}',
    'unit hygienetwo;',
    '{$ENDIF}',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    'end.');

  cHygieneTwoClausesPlainFirst: array[0..11] of string = (
    '{$IFNDEF FPC_DOTTEDUNITS}',
    'unit hygienetwo;',
    '{$ELSE}',
    'unit System.HygieneTwo;',
    '{$ENDIF}',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    'end.');

  // DirectiveWord answers I for all three, and none of them is an include.
  cHygieneIoCheckDirectives: array[0..9] of string = (
    'unit HygieneIoCheck;',
    '',
    '{$I-}',
    '{$I+}',
    '{$I %FPCTARGETCPU%}',
    '',
    'interface',
    '',
    'implementation',
    'end.');

  // The scanner joins a directive split across two lines with a blank.
  cHygieneSplitIoCheck: array[0..6] of string = (
    'unit HygieneSplitIoCheck;',
    '',
    '{$',
    'I-}',
    '',
    'interface',
    'end.');

  // Hint-modifier fixtures: the deprecated call site is line 24.
  cHintDeprecatedUse: array[0..26] of string = (
    'unit hintdeprecated;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns the legacy value.',
    'function OldValue: Integer; deprecated;',
    '',
    '// Returns the current value.',
    'function CurrentValue: Integer;',
    '',
    'implementation',
    '',
    'function OldValue: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'function CurrentValue: Integer;',
    '',
    'begin',
    '  Result := OldValue;',
    'end;',
    '',
    'end.');

  // Nothing here carries a hint modifier, and the unit must resolve clean.
  cHintCompliant: array[0..26] of string = (
    'unit hintcompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns the base value.',
    'function BaseValue: Integer;',
    '',
    '// Returns twice the base value.',
    'function DoubleValue: Integer;',
    '',
    'implementation',
    '',
    'function BaseValue: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'function DoubleValue: Integer;',
    '',
    'begin',
    '  Result := BaseValue * 2;',
    'end;',
    '',
    'end.');

  // All three hints are declared; none of the three routines is ever called.
  cHintDeclaredNotUsed: array[0..34] of string = (
    'unit hintdeclared;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns the legacy value.',
    'function OldValue: Integer; deprecated;',
    '',
    '// Reports to the host console.',
    'procedure Report; platform;',
    '',
    '// Returns a provisional tag.',
    'function ProvisionalTag: Integer; experimental;',
    '',
    'implementation',
    '',
    'function OldValue: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'procedure Report;',
    '',
    'begin',
    'end;',
    '',
    'function ProvisionalTag: Integer;',
    '',
    'begin',
    '  Result := 2;',
    'end;',
    '',
    'end.');

  // The platform call site is line 23; the unit itself claims no hint.
  cHintPlatformUse: array[0..25] of string = (
    'unit hintplatform;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Reports aValue to the host console.',
    'procedure Report(aValue: Integer); platform;',
    '',
    '// Reports the default value.',
    'procedure ReportDefault;',
    '',
    'implementation',
    '',
    'procedure Report(aValue: Integer);',
    '',
    'begin',
    'end;',
    '',
    'procedure ReportDefault;',
    '',
    'begin',
    '  Report(1);',
    'end;',
    '',
    'end.');

  // The same call site inside a unit that declares itself platform.
  cHintPlatformUnit: array[0..25] of string = (
    'unit hintplatformunit platform;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Reports aValue to the host console.',
    'procedure Report(aValue: Integer); platform;',
    '',
    '// Reports the default value.',
    'procedure ReportDefault;',
    '',
    'implementation',
    '',
    'procedure Report(aValue: Integer);',
    '',
    'begin',
    'end;',
    '',
    'procedure ReportDefault;',
    '',
    'begin',
    '  Report(1);',
    'end;',
    '',
    'end.');

  // The experimental call site is line 24.
  cHintExperimentalUse: array[0..26] of string = (
    'unit hintexperimental;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns a tag whose numbering may still change.',
    'function ProvisionalTag: Integer; experimental;',
    '',
    '// Returns the tag in use.',
    'function CurrentTag: Integer;',
    '',
    'implementation',
    '',
    'function ProvisionalTag: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'function CurrentTag: Integer;',
    '',
    'begin',
    '  Result := ProvisionalTag;',
    'end;',
    '',
    'end.');

  // One declaration carrying two hints at once.
  cHintMultiHint: array[0..20] of string = (
    'unit hintmulti;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cVolumeId = 8 platform deprecated;',
    '',
    '// Returns the volume attribute.',
    'function VolumeAttribute: Integer;',
    '',
    'implementation',
    '',
    'function VolumeAttribute: Integer;',
    '',
    'begin',
    '  Result := cVolumeId;',
    'end;',
    '',
    'end.');

  { Each hinted type is named only in a variable's type annotation, which the
    reference pairing does not collect, so no hint reaches a rule. }
  cHintBlindSites: array[0..33] of string = (
    'unit hintblind;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TOldRec = record',
    '    Value: Integer;',
    '  end deprecated;',
    '  THostRec = record',
    '    Value: Integer;',
    '  end platform;',
    '  TDraftRec = record',
    '    Value: Integer;',
    '  end experimental;',
    '',
    '// Returns the sum of the three stored values.',
    'function StoredSum: Integer;',
    '',
    'implementation',
    '',
    'var',
    '  GOld: TOldRec;',
    '  GHost: THostRec;',
    '  GDraft: TDraftRec;',
    '',
    'function StoredSum: Integer;',
    '',
    'begin',
    '  Result := GOld.Value + GHost.Value + GDraft.Value;',
    'end;',
    '',
    'end.');

  // The three hinted declarations survive, but the bodies below never parse.
  cHintParseFailure: array[0..26] of string = (
    'unit hintbroken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns the legacy value.',
    'function OldValue: Integer; deprecated;',
    '',
    '// Reports aValue to the host console.',
    'procedure Report(aValue: Integer); platform;',
    '',
    '// Returns a tag whose numbering may still change.',
    'function ProvisionalTag: Integer; experimental;',
    '',
    '// Returns every hinted value at once.',
    'function Everything: Integer;',
    '',
    'implementation',
    '',
    'function Everything: Integer',
    '',
    'begin',
    '  Report(OldValue + ProvisionalTag)',
    'end',
    '',
    'end.');

  // The public 'procedure Undocumented;' of line 12 carries no comment
  // above it.
  cDocMethodPositions: array[0..16] of string = (
    'unit DocMethods;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    procedure Undocumented;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDocMethodParseFailure: array[0..18] of string = (
    'unit DocMethods;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    procedure Undocumented;',
    '  end;',
    '',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  // Two ways the association breaks: a blank line (line 12) and a directive as
  // the only thing above the declaration (line 14).
  cDocMethodBreaks: array[0..19] of string = (
    'unit DocBreaks;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  public',
    '    // Stores a value.',
    '',
    '    procedure BlankLineAbove;',
    '    {$region ''Housekeeping''}',
    '    procedure RegionOnlyAbove;',
    '    {$endregion}',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDocMethodSatisfied: array[0..32] of string = (
    'unit DocSatisfied;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '    procedure Hidden;',
    '  strict protected',
    '    procedure AlsoHidden;',
    '  public',
    '    // Stores a value.',
    '    procedure LineDocumented;',
    '    { Returns the value. }',
    '    function BlockDocumented: Integer;',
    '    { Returns twice the value,',
    '      across two comment lines. }',
    '    function MultiLineDocumented: Integer;',
    '    // Clears the value.',
    '    {$region ''Housekeeping''}',
    '    procedure RegionBetween;',
    '    {$endregion}',
    '  protected',
    '    // Reports the raw value.',
    '    function Guarded: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // Delphi mode: the comment sits above the attribute run for one member and
  // below it for the next two.
  cDocAttributes: array[0..30] of string = (
    'unit DocAttributes;',
    '',
    '{$mode delphi}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  MarkAttribute = class(TCustomAttribute)',
    '  public',
    '    // Marks a declaration.',
    '    constructor Create;',
    '  end;',
    '',
    '  TThing = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    // Documented above its attribute run.',
    '    [Mark]',
    '    procedure AttributeAbove;',
    '    [Mark]',
    '    // Documented below its attribute run.',
    '    procedure AttributeBelow;',
    '    [Mark]',
    '    // The value stored last.',
    '    property Value: Integer read FValue;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // Three bodies whose members are public by language rule but carry no
  // visibility specifier.
  cDocNoVisibility: array[0..39] of string = (
    'unit DocNoVisibility;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch advancedrecords}',
    '',
    'interface',
    '',
    'type',
    '  ILogger = interface',
    '    [''{4D3C2B1A-6E5F-4A3B-9C8D-1F0E9D8C7B6A}'']',
    '    function GetLevel: Integer;',
    '    procedure Log(const aLine: string);',
    '    property Level: Integer read GetLevel;',
    '  end;',
    '',
    '  TPlainRec = record',
    '    Value: Integer;',
    '    function Doubled: Integer;',
    '  end;',
    '',
    '  TEarly = class(TObject)',
    '    function GetEarly: Integer;',
    '    property Early: Integer read GetEarly;',
    '  end;',
    '',
    'implementation',
    '',
    'function TPlainRec.Doubled: Integer;',
    '',
    'begin',
    '  Result := Value * 2;',
    'end;',
    '',
    'function TEarly.GetEarly: Integer;',
    '',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'end.');

  // The published 'property X' of line 13 carries no comment above it.
  cDocPropertyPositions: array[0..17] of string = (
    'unit DocProps;',
    '',
    '{$mode objfpc}{$H+}',
    '{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FX: Integer;',
    '  published',
    '    property X: Integer read FX;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDocPropertyParseFailure: array[0..19] of string = (
    'unit DocProps;',
    '',
    '{$mode objfpc}{$H+}',
    '{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FX: Integer;',
    '  published',
    '    property X: Integer read FX;',
    '  end;',
    '',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  cDocPropertySatisfied: array[0..24] of string = (
    'unit DocPropsOk;',
    '',
    '{$mode objfpc}{$H+}',
    '{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FX: Integer;',
    '    property Hidden: Integer read FX;',
    '  protected',
    '    property Sheltered: Integer read FX;',
    '  published',
    '    // The stored value.',
    '    property Documented: Integer read FX;',
    '  public',
    '    { The stored value, once more. }',
    '    property AlsoDocumented: Integer read FX;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // One undocumented method (line 12) beside one undocumented property (13).
  cDocMixed: array[0..17] of string = (
    'unit DocMixed;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FX: Integer;',
    '  public',
    '    procedure Act;',
    '    property X: Integer read FX;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDocClassMembers: array[0..17] of string = (
    'unit DocClassMembers;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    class var FTotal: Integer;',
    '  public',
    '    class procedure Reset;',
    '    class property Total: Integer read FTotal;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDocIncludePayload: array[0..1] of string = (
    '    procedure UndocumentedInInclude;',
    '    property FromInclude: Integer read FValue;');

  cDocIncludeHost: array[0..18] of string = (
    'unit DocInclude;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    {$I docinc.inc}',
    '    procedure InTheHost;',
    '    property InTheHostToo: Integer read FValue;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The include host with its payload written out in place of the {$I}.
  cDocIncludeInlined: array[0..19] of string = (
    'unit DocInclude;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    procedure UndocumentedInInclude;',
    '    property FromInclude: Integer read FValue;',
    '    procedure InTheHost;',
    '    property InTheHostToo: Integer read FValue;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // Line 12 is never scanned, line 14 follows a conditional the doc chain
  // cannot cross, and line 16 follows the {$ENDIF} that closes it.
  cDocConditional: array[0..20] of string = (
    'unit DocConditional;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  public',
    '    // Logs a line.',
    '    {$IFDEF FPC_DOTTEDUNITS}',
    '    procedure LogDotted;',
    '    {$ELSE}',
    '    procedure LogPlain;',
    '    {$ENDIF}',
    '    procedure StillReported;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The comments of lines 10 and 12 trail a declaration instead of starting
  // their own line.
  cDocTrailingComment: array[0..17] of string = (
    'unit DocTrailing;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  public',
    '    FValue: Integer; // Holds the value.',
    '    procedure Foo;',
    '    FOther: Integer; // Holds another value.',
    '    property Bar: Integer read FOther;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The 'record' of line 11 is a constraint, not a body; lines 12 and 13 are
  // undocumented members of the same class.
  cDocGenerics: array[0..17] of string = (
    'unit DocGenerics;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBag = class(TObject)',
    '  public',
    '    // Adds an item.',
    '    generic procedure Add<T: record>(const aItem: T);',
    '    generic function Take<T>: T;',
    '    procedure Undocumented;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // Line 8 is a forward declaration, not an interface body.
  cDocForwardIntf: array[0..21] of string = (
    'unit DocForwardIntf;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface;',
    '',
    '  TAfter = class(TObject)',
    '  public',
    '    procedure Undocumented;',
    '  end;',
    '',
    '  IFoo = interface',
    '    // Returns a tag.',
    '    function Tag: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // A type helper opens a body of its own, so line 12 is a public member of it
  // and line 17 a member of the class that follows.
  cDocTypeHelper: array[0..21] of string = (
    'unit DocTypeHelper;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch typehelpers}',
    '',
    'interface',
    '',
    'type',
    '  TIntHelper = type helper for Integer',
    '    function First: Integer;',
    '  public',
    '    function Doubled: Integer;',
    '  end;',
    '',
    '  TAfter = class(TObject)',
    '  public',
    '    procedure Undocumented;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The visibility specifiers of lines 9 and 14 follow a class modifier.
  cDocClassModifiers: array[0..19] of string = (
    'unit DocClassModifiers;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class abstract',
    '  public',
    '    procedure FromAbstract;',
    '  end;',
    '',
    '  TLeaf = class sealed',
    '  public',
    '    procedure FromSealed;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // A packed class opens a body, so line 10 is its member and line 15 belongs
  // to the class that follows it.
  cDocPackedClass: array[0..19] of string = (
    'unit DocPackedClass;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPacked = packed class(TObject)',
    '  public',
    '    procedure InPacked;',
    '  end;',
    '',
    '  TAfter = class(TObject)',
    '  public',
    '    procedure AfterPacked;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The visibility specifier of line 10 is the first thing in the helper body.
  cDocHelperFirstVis: array[0..15] of string = (
    'unit DocHelperFirstVis;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch typehelpers}',
    '',
    'interface',
    '',
    'type',
    '  TIntHelper = type helper for Integer',
    '  public',
    '    function Doubled: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // Two directives share line 11, between the comment and the declaration.
  cDocSameRowDirectives: array[0..17] of string = (
    'unit DocSameRow;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  public',
    '    // Does a thing.',
    '    {$PUSH}{$WARN 5024 OFF}',
    '    procedure Documented;',
    '    {$POP}',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The undocumented method of line 13 and property of line 14, in Delphi mode.
  cDocDelphiMode: array[0..18] of string = (
    'unit DocDelphi;',
    '',
    '{$mode delphi}{$H+}',
    '{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class(TObject)',
    '  private',
    '    FX: Integer;',
    '  public',
    '    procedure Undocumented;',
    '    property X: Integer read FX;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  { The InterfaceUsesTooBroad project: of iub_user's four interface imports only
    iub_broad's line 9 entry is a finding; iub_implonly is imported below. }
  cIubUser: array[0..30] of string = (
    'unit iub_user;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  iub_needed,',
    '  iub_broad,',
    '  iub_operator,',
    '  iub_initfinal;',
    '',
    'type',
    '  TUser = record',
    '    Value: TNeededType;',
    '  end;',
    '',
    'implementation',
    '',
    'uses',
    '  iub_implonly;',
    '',
    'procedure Touch;',
    '',
    'begin',
    '  BroadTouch;',
    '  ImplOnlyTouch;',
    'end;',
    '',
    '',
    'end.');

  cIubBroken: array[0..15] of string = (
    'unit iub_broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  iub_needed,',
    '  iub_broad;',
    '',
    'type',
    '  TBroken = class(;',
    '',
    'implementation',
    '',
    'end.');

  cIubNeeded: array[0..11] of string = (
    'unit iub_needed;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TNeededType = Integer;',
    '',
    'implementation',
    '',
    'end.');

  cIubBroad: array[0..15] of string = (
    'unit iub_broad;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure BroadTouch;',
    '',
    'implementation',
    '',
    'procedure BroadTouch;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cIubImplOnly: array[0..15] of string = (
    'unit iub_implonly;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure ImplOnlyTouch;',
    '',
    'implementation',
    '',
    'procedure ImplOnlyTouch;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cIubOperator: array[0..19] of string = (
    'unit iub_operator;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TIubHelper = class helper for TObject',
    '    function IubTag: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TIubHelper.IubTag: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'end.');

  cIubInitFinal: array[0..13] of string = (
    'unit iub_initfinal;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GIubTicks: Integer;',
    '',
    'implementation',
    '',
    'initialization',
    '  GIubTicks := 0;',
    'end.');

  { The I/O fixtures declare their own Reset, Report and IOResult: the RTL
    spellings resolve nowhere, and an unresolved call takes the routine out.
    Line 21 is Run's begin, so every body below starts on line 22. }
  cIoUnchecked: array[0..26] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  Reset(g);',
    '  {$I+}',
    'end;',
    'end.');

  cIoLowercaseSwitch: array[0..26] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$i-}',
    '  Reset(f);',
    '  Reset(g);',
    '  {$i+}',
    'end;',
    'end.');

  cIoChecked: array[0..28] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  if IOResult <> 0 then',
    '    Report;',
    '  Reset(g);',
    '  {$I+}',
    'end;',
    'end.');

  cIoIndirectCheck: array[0..27] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  Report;',
    '  Reset(g);',
    '  {$I+}',
    'end;',
    'end.');

  cIoNoRegion: array[0..24] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  Reset(f);',
    '  Reset(g);',
    'end;',
    'end.');

  // The unchecked pair sits past the {$I+} that closed the region.
  cIoRegionClosed: array[0..29] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  if IOResult <> 0 then',
    '    Report;',
    '  {$I+}',
    '  Reset(g);',
    '  Reset(h);',
    'end;',
    'end.');

  // The check is an assignment, so nothing else between the two calls.
  cIoAssignedCheck: array[0..30] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    '  lCode: Word;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  lCode := IOResult;',
    '  Reset(g);',
    '  {$I+}',
    '  if lCode <> 0 then',
    '    Report;',
    'end;',
    'end.');

  // The region closes between the two calls, so the second one checks instead.
  cIoRegionEndsFirst: array[0..26] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'procedure Report;',
    'begin',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  {$I+}',
    '  Reset(g);',
    'end;',
    'end.');

  // OldValue is the live sibling: DeprecatedSymbolUsed fires on its one use.
  cIoInlineAssembler: array[0..37] of string = (
    'unit ioprobe;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    '// Returns the legacy value.',
    'function OldValue: Integer; deprecated;',
    'implementation',
    'procedure Reset(var aFile: Text);',
    'begin',
    'end;',
    'function OldValue: Integer;',
    'begin',
    '  Result := 1;',
    'end;',
    'procedure Report;',
    'begin',
    '  if OldValue > 0 then',
    '    Exit;',
    'end;',
    'function IOResult: Word;',
    'begin',
    '  Result := 0;',
    'end;',
    'procedure Run;',
    'var',
    '  f: Text;',
    '  g: Text;',
    '  h: Text;',
    'begin',
    '  {$I-}',
    '  Reset(f);',
    '  asm',
    '    nop',
    '  end;',
    '  Reset(g);',
    '  {$I+}',
    'end;',
    'end.');

function TRulesFpcStyleTest.EnabledConfig(
  const aRuleId: string): TFpSonarConfig;

begin
  Result := TFpSonarConfig.Default;
  SetLength(Result.Rules, 0);
  SetLength(Result.Rules, 1);
  Result.Rules[0].RuleId := aRuleId;
  Result.Rules[0].HasEnabled := True;
  Result.Rules[0].Enabled := True;
end;


procedure TRulesFpcStyleTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector; aWithhold: boolean = False);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lConfig: TFpSonarConfig;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lConfig := EnabledConfig(aRule.Metadata.RuleId);
    lEngine.Config := lConfig;
    if aWithhold then
      lEngine.Analyze(aFixture, cMode, cDefines, [], [], True, SizeOf(Pointer),
        aCollector)
    else
      lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TRulesFpcStyleTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
  const aSrc: array of string; const aCollector: TFpSonarIssueCollector;
  aWithhold: boolean = False);

var
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    RunRule(aRule, lFix.Add(aName, aSrc), aCollector, aWithhold);
  finally
    lFix.Free;
  end;
end;


function TRulesFpcStyleTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesFpcStyleTest.FirstById(const aCollector: TFpSonarIssueCollector;
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


function TRulesFpcStyleTest.NthById(const aCollector: TFpSonarIssueCollector;
  const aId: string; aNth: Integer): Integer;

var
  i, lSeen: Integer;

begin
  Result := -1;
  lSeen := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      begin
        if lSeen = aNth then
          Exit(i);
        Inc(lSeen);
      end;
end;


function TRulesFpcStyleTest.CountArg(const aCollector: TFpSonarIssueCollector;
  const aId, aArg: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if (aCollector.Issues[i].RuleId = aId)
      and (Length(aCollector.Issues[i].MessageArgs) > 0)
      and (aCollector.Issues[i].MessageArgs[0] = aArg) then
      Inc(Result);
end;


function TRulesFpcStyleTest.BuildIndex(
  const aFiles: array of string): TFpSonarProjectIndex;

begin
  Result := BuildProjectIndex(aFiles, cMode, cDefines, [], []);
end;


procedure TRulesFpcStyleTest.RunRuleWithIndex(aRule: TRuleBase;
  const aSubject: string; aIndex: TFpSonarProjectIndex;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.ProjectIndex := aIndex;
    lEngine.Config := EnabledConfig(aRule.Metadata.RuleId);
    lEngine.Analyze(aSubject, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesFpcStyleTest.WriteUsesProject(aTmp: TTempFixtures): TStringArray;

begin
  Result := [
    aTmp.Add('iub_user.pas', cIubUser),
    aTmp.Add('iub_needed.pas', cIubNeeded),
    aTmp.Add('iub_broad.pas', cIubBroad),
    aTmp.Add('iub_operator.pas', cIubOperator),
    aTmp.Add('iub_initfinal.pas', cIubInitFinal),
    aTmp.Add('iub_implonly.pas', cIubImplOnly)];
end;


procedure TRulesFpcStyleTest.CheckSilentWithLiveSibling(aRule,
  aSibling: TRuleBase; const aName, aId, aSiblingId: string;
  const aSrc: array of string);

begin
  AssertEquals('an unresolved fact is silent', 0,
    CountSrc(aRule, aName, aId, aSrc));
  AssertEquals('the resolver was live', 1,
    CountSrc(aSibling, aName, aSiblingId, aSrc));
end;


function TRulesFpcStyleTest.CountSrc(aRule: TRuleBase; const aName, aId: string;
  const aSrc: array of string; aWithhold: boolean = False): Integer;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(aRule, aName, aSrc, lc, aWithhold);
    Result := CountById(lc, aId);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.AssertResolvesClean(const aName: string;
  const aSrc: array of string);

var
  lFix: TTempFixtures;
  lRes: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lFix := TTempFixtures.Create;
  try
    lRes := TFpSonarResolver.Create;
    try
      AssertTrue(aName + ' must resolve clean (else the zero-issue assert is a '
        + 'vacuous silent-skip)',
        lRes.BuildFor(lFix.Add(aName, aSrc), cMode, cDefines, [], [], lDiag)
        and lRes.Succeeded);
    finally
      lRes.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesFpcStyleTest.NewBranches: TRuleBase;

begin
  Result := TRuleDottedUnitsBranchesInconsistent.Create(TRuleMetadata.Make(
    cBranchesId, rtTok, rfLineText, sevMajor, itBug, cfMedium, False, ''));
end;


function TRulesFpcStyleTest.NewMissingGuard: TRuleBase;

begin
  Result := TRuleMissingDottedUnitsGuard.Create(TRuleMetadata.Make(
    cMissingGuardId, rtTok, rfLineText, sevMinor, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewAliasMismatch: TRuleBase;

begin
  Result := TRuleDottedUnitAliasMismatch.Create(TRuleMetadata.Make(
    cAliasId, rtTok, rfLineText, sevMajor, itBug, cfMedium, False, ''));
end;


function TRulesFpcStyleTest.NewCaseMismatch: TRuleBase;

begin
  Result := TRuleUnitFileNameCaseMismatch.Create(TRuleMetadata.Make(
    cCaseMismatchId, rtTok, rfLineText, sevMinor, itCodeSmell, cfHigh,
    False, ''));
end;


function TRulesFpcStyleTest.NewMissingMode: TRuleBase;

begin
  Result := TRuleMissingModeDirective.Create(TRuleMetadata.Make(
    cMissingModeId, rtTok, rfLineText, sevMinor, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewMissingCopyright: TRuleBase;

begin
  Result := TRuleMissingCopyrightHeader.Create(TRuleMetadata.Make(
    cMissingCopyrightId, rtTok, rfLineText, sevInfo, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewDeprecatedSymbol: TRuleBase;

begin
  Result := TRuleDeprecatedSymbolUsed.Create(TRuleMetadata.Make(
    cDeprecatedId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    False, ''));
end;


function TRulesFpcStyleTest.NewPlatformSymbol: TRuleBase;

begin
  Result := TRulePlatformSymbolUsedInPortableUnit.Create(TRuleMetadata.Make(
    cPlatformId, rtSem, rfResolver, sevMinor, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewExperimentalSymbol: TRuleBase;

begin
  Result := TRuleExperimentalSymbolUsed.Create(TRuleMetadata.Make(
    cExperimentalId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    False, ''));
end;


function TRulesFpcStyleTest.NewMethodUndocumented: TRuleBase;

begin
  Result := TRulePublicMethodUndocumented.Create(TRuleMetadata.Make(
    cMethodDocId, rtTok, rfTokenStream, sevInfo, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewPropertyUndocumented: TRuleBase;

begin
  Result := TRulePublicPropertyUndocumented.Create(TRuleMetadata.Make(
    cPropertyDocId, rtTok, rfTokenStream, sevInfo, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewUsesTooBroad: TRuleBase;

begin
  Result := TRuleInterfaceUsesTooBroad.Create(TRuleMetadata.Make(
    cUsesTooBroadId, rtUse, rfAst, sevMinor, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesFpcStyleTest.NewIOResultNotChecked: TRuleBase;

begin
  Result := TRuleIOResultNotChecked.Create(TRuleMetadata.Make(
    cIOResultId, rtSem, rfResolver, sevMajor, itBug, cfMedium, False, ''));
end;


procedure TRulesFpcStyleTest.DottedUnitsBranchesInconsistentPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: StrUtils on line 11 is in the non-dotted branch only, at the
  // tail of a prefix whose two pairs both correspond => cols 22..29.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewBranches, 'noncompliant.pas', cBranchesNoncompliant, lc);
    AssertEquals('one branch-inconsistency issue', 1, CountById(lc, cBranchesId));
    k := FirstById(lc, cBranchesId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 22, lc.Issues[k].StartCol);
    AssertEquals('end line', 11, lc.Issues[k].EndLine);
    AssertEquals('end col', 29, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cBranchesId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the unpaired unit',
      'Unit StrUtils is listed in only one FPC_DOTTEDUNITS branch',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertEquals('both branches correct => zero', 0,
    CountSrc(NewBranches, 'compliant.pas', cBranchesId, cBothBranchesCorrect));
end;


procedure TRulesFpcStyleTest.DottedUnitsBranchesInconsistentSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewBranches, 'broken.pas', cBranchesParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cBranchesId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cBranchesId);
    AssertEquals('same start line', 11, lc.Issues[k].StartLine);
    AssertEquals('same start col', 22, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.DottedUnitsBranchesInconsistentSilentShapes;

begin
  // Every shape here reports at least once when its exclusion is removed; the
  // shapes that only ever reach this rule's length check live with the alias
  // rule instead.
  AssertEquals('unidentifiable unpaired position => zero', 0,
    CountSrc(NewBranches, 'unaligned.pas', cBranchesId, cUnalignedPrefix));
  AssertEquals('"in" is not a unit name => zero', 0,
    CountSrc(NewBranches, 'program.lpr', cBranchesId, cProgramUsesIn));
  AssertEquals('guard with no else branch => zero', 0,
    CountSrc(NewBranches, 'noelse.pas', cBranchesId, cGuardWithoutElse));
  AssertEquals('no uses clause => zero', 0,
    CountSrc(NewBranches, 'nouses.pas', cBranchesId, cNoUsesClause));
end;


procedure TRulesFpcStyleTest.MissingDottedUnitsGuardPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: nothing in the file names the guard; the uses keyword of
  // line 7 starts at column 1 and spans four bytes.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingGuard, 'noncompliant.pas', cMissingGuardNoncompliant, lc);
    AssertEquals('one missing-guard issue', 1, CountById(lc, cMissingGuardId));
    k := FirstById(lc, cMissingGuardId);
    AssertEquals('start line', 7, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 7, lc.Issues[k].EndLine);
    AssertEquals('end col', 4, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cMissingGuardId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message states the defect',
      'Uses clause has no FPC_DOTTEDUNITS guard',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertEquals('guarded clause => zero', 0,
    CountSrc(NewMissingGuard, 'compliant.pas', cMissingGuardId,
    cBothBranchesCorrect));
end;


procedure TRulesFpcStyleTest.MissingDottedUnitsGuardSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingGuard, 'broken.pas', cMissingGuardParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cMissingGuardId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cMissingGuardId);
    AssertEquals('same start line', 7, lc.Issues[k].StartLine);
    AssertEquals('same start col', 1, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.MissingDottedUnitsGuardIndirectSatisfaction;

begin
  AssertEquals('a directive elsewhere names the guard => zero', 0,
    CountSrc(NewMissingGuard, 'elsewhere.pas', cMissingGuardId,
    cGuardElsewhereOnly));
  AssertEquals('no uses clause => zero', 0,
    CountSrc(NewMissingGuard, 'nouses.pas', cMissingGuardId, cNoUsesClause));
end;


procedure TRulesFpcStyleTest.MissingDottedUnitsGuardIgnoresCommentAndStringMentions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The symbol occurs in a // comment and in a string literal only.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingGuard, 'mentions.pas', cGuardOnlyInCommentAndString, lc);
    AssertEquals('still reported', 1, CountById(lc, cMissingGuardId));
    k := FirstById(lc, cMissingGuardId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: the table records fpjson as FpJson.Data.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewAliasMismatch, 'noncompliant.pas', cAliasNoncompliant, lc);
    AssertEquals('one alias-mismatch issue', 1, CountById(lc, cAliasId));
    k := FirstById(lc, cAliasId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 3, lc.Issues[k].StartCol);
    AssertEquals('end line', 9, lc.Issues[k].EndLine);
    AssertEquals('end col', 15, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cAliasId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the non-dotted unit and its recorded alias',
      'Unit fpjson is not aliased as FpJson.Data in the FPC_DOTTEDUNITS branch',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertEquals('both aliases as recorded => zero', 0,
    CountSrc(NewAliasMismatch, 'compliant.pas', cAliasId, cCanonicalAlias));
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchSilentOnUnknownName;

begin
  AssertEquals('no table entry for the non-dotted name => zero', 0,
    CountSrc(NewAliasMismatch, 'unknown.pas', cAliasId, cAliasUnknownName));
  AssertEquals('RTL names the table does not carry => zero', 0,
    CountSrc(NewAliasMismatch, 'rtl.pas', cAliasId, cBothBranchesCorrect));
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchSilentOnLengthMismatch;

begin
  AssertEquals('branches of unequal length => zero', 0,
    CountSrc(NewAliasMismatch, 'lengths.pas', cAliasId, cAliasLengthMismatch));
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchSilentOnReorderedBranches;

begin
  AssertEquals('the recorded alias present elsewhere in the branch => zero', 0,
    CountSrc(NewAliasMismatch, 'reordered.pas', cAliasId, cAliasReordered));
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchReportsThroughAReorderedBranch;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // fpjson has no FpJson.Data anywhere in the branch.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewAliasMismatch, 'crossed.pas', cAliasReorderedWrong, lc);
    AssertEquals('one alias-mismatch issue', 1, CountById(lc, cAliasId));
    k := FirstById(lc, cAliasId);
    AssertEquals('message names the non-dotted unit and its recorded alias',
      'Unit fpjson is not aliased as FpJson.Data in the FPC_DOTTEDUNITS branch',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchSilentWithoutGuard;

begin
  AssertEquals('no FPC_DOTTEDUNITS directive => zero', 0,
    CountSrc(NewAliasMismatch, 'noguard.pas', cAliasId,
    cMissingGuardNoncompliant));
  AssertEquals('guard with no else branch => zero', 0,
    CountSrc(NewAliasMismatch, 'noelse.pas', cAliasId, cGuardWithoutElse));
  AssertEquals('an {$i} inside a branch => zero', 0,
    CountSrc(NewAliasMismatch, 'opaque.pas', cAliasId, cAliasOpaqueGuard));
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewAliasMismatch, 'broken.pas', cAliasParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cAliasId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cAliasId);
    AssertEquals('same start line', 9, lc.Issues[k].StartLine);
    AssertEquals('same start col', 3, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.DottedUnitAliasMismatchRegistersDisabled;

begin
  AssertTrue('DottedUnitAliasMismatch registered',
    RuleRegistry.FindById(cAliasId) <> nil);
  AssertFalse('DottedUnitAliasMismatch ships disabled',
    RuleRegistry.FindById(cAliasId).Metadata.DefaultEnabled);
end;


procedure TRulesFpcStyleTest.TwoClausesInOneFileArePairedApart;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The interface clause pairs; only the implementation clause's System.Math
  // (line 18, cols 20..30) is unpaired.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewBranches, 'twoclauses.pas', cTwoClausesInOneFile, lc);
    AssertEquals('only the second clause reports', 1,
      CountById(lc, cBranchesId));
    k := FirstById(lc, cBranchesId);
    AssertEquals('start line', 18, lc.Issues[k].StartLine);
    AssertEquals('start col', 20, lc.Issues[k].StartCol);
    AssertEquals('end col', 30, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.ParenFormDirectivesAreDirectives;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // (*$IFDEF*)/(*$ELSE*)/(*$ENDIF*) open, switch and close the guard.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewBranches, 'paren.pas', cParenFormGuard, lc);
    AssertEquals('one branch-inconsistency issue', 1,
      CountById(lc, cBranchesId));
    k := FirstById(lc, cBranchesId);
    AssertEquals('start line', 9, lc.Issues[k].StartLine);
    AssertEquals('start col', 36, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;

  AssertEquals('a (*$ ... *) guard exempts the file', 0,
    CountSrc(NewMissingGuard, 'paren.pas', cMissingGuardId, cParenFormGuard));
end;


procedure TRulesFpcStyleTest.ElseIfAtGuardLevelStopsTheComparison;

begin
  AssertEquals('an {$ELSEIF} arm at guard level => zero', 0,
    CountSrc(NewBranches, 'elseif.pas', cBranchesId, cElseIfAtGuardLevel));
end;


procedure TRulesFpcStyleTest.IncludeInsideGuardStopsTheComparison;

begin
  AssertEquals('an {$i} inside a branch => zero', 0,
    CountSrc(NewBranches, 'include.pas', cBranchesId, cIncludeInsideGuard));
end;


procedure TRulesFpcStyleTest.DuplicateTailEntryIsNoMissingUnit;

begin
  AssertEquals('a tail entry the other branch already holds => zero', 0,
    CountSrc(NewBranches, 'duplicate.pas', cBranchesId, cDuplicateTailEntry));
end;


procedure TRulesFpcStyleTest.UnitFileNameCaseMismatchPositions;

var
  lc: TFpSonarIssueCollector;
  lRule: TRuleBase;
  lCtx: TRuleContext;
  k: Integer;

begin
  // Noncompliant: Surfacething.pas is neither the lowercase form of
  // SurfaceThing nor that name verbatim; the identifier spans cols 6..17.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCaseMismatch, 'Surfacething.pas', cHygieneCaseMismatch, lc);
    AssertEquals('one case-mismatch issue', 1, CountById(lc, cCaseMismatchId));
    k := FirstById(lc, cCaseMismatchId);
    AssertEquals('start line', 2, lc.Issues[k].StartLine);
    AssertEquals('start col', 6, lc.Issues[k].StartCol);
    AssertEquals('end line', 2, lc.Issues[k].EndLine);
    AssertEquals('end col', 17, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cCaseMismatchId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the file and the unit',
      'File name Surfacething.pas is not the lowercase form of unit name '
      + 'SurfaceThing',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertEquals('the lowercase layout => zero', 0,
    CountSrc(NewCaseMismatch, 'fpsonar.rules.fpcstyle.pp', cCaseMismatchId,
    cHygieneLowercaseLayout));

  // No engine run can hand a rule an empty file name.
  lCtx := Default(TRuleContext);
  SetLength(lCtx.Lines, 1);
  lCtx.Lines[0] := 'unit SurfaceThing;';
  lRule := NewCaseMismatch;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      lRule.Apply(lCtx, lc);
      AssertEquals('an empty file name => zero', 0,
        CountById(lc, cCaseMismatchId));
    finally
      lc.Free;
    end;
  finally
    lRule.Free;
  end;
end;


procedure TRulesFpcStyleTest.UnitFileNameCaseMismatchSilentOnNamespacedMirror;

begin
  AssertEquals('the case-preserved dotted mirror => zero', 0,
    CountSrc(NewCaseMismatch, 'System.Macuuid.pp', cCaseMismatchId,
    cHygieneNamespacedMirror));
end;


procedure TRulesFpcStyleTest.UnitFileNameCaseMismatchSilentOnProgramAndLibrary;

begin
  AssertEquals('a program clause is never judged => zero', 0,
    CountSrc(NewCaseMismatch, 'MyProject.lpr', cCaseMismatchId,
    cHygieneProgram));
  AssertEquals('a library clause is never judged => zero', 0,
    CountSrc(NewCaseMismatch, 'MyLibrary.lpr', cCaseMismatchId,
    cHygieneLibrary));
end;


procedure TRulesFpcStyleTest.UnitFileNameCaseMismatchSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewCaseMismatch, 'Surfacething.pas', cHygieneCaseParseFailure,
      lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cCaseMismatchId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cCaseMismatchId);
    AssertEquals('same start line', 2, lc.Issues[k].StartLine);
    AssertEquals('same start col', 6, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.MissingModeDirectivePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: no directive in the file is a {$mode}; the unit keyword of
  // line 2 starts at column 1 and spans four bytes.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingMode, 'hygienenomode.pas', cHygieneNoMode, lc);
    AssertEquals('one missing-mode issue', 1, CountById(lc, cMissingModeId));
    k := FirstById(lc, cMissingModeId);
    AssertEquals('start line', 2, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 2, lc.Issues[k].EndLine);
    AssertEquals('end col', 4, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cMissingModeId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the module',
      'Module HygieneNoMode has no {$mode} directive',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // The library keyword spans seven bytes, and the range comes from the
  // spelling the scan kept at capture.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingMode, 'mylibrary.lpr', cHygieneLibrary, lc);
    AssertEquals('a library is judged too', 1, CountById(lc, cMissingModeId));
    k := FirstById(lc, cMissingModeId);
    AssertEquals('start line', 1, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end col', 7, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  AssertEquals('a program is judged too', 1,
    CountSrc(NewMissingMode, 'myproject.lpr', cMissingModeId, cHygieneProgram));
  AssertEquals('a literal {$mode} => zero', 0,
    CountSrc(NewMissingMode, 'hygienenobanner.pas', cMissingModeId,
    cHygieneNoBanner));
end;


procedure TRulesFpcStyleTest.MissingModeDirectiveIndirectSatisfaction;

begin
  AssertEquals('the (*$MODE*) form => zero', 0,
    CountSrc(NewMissingMode, 'hygieneparenmode.pas', cMissingModeId,
    cHygieneModeParenForm));
  AssertEquals('a {$mode} inside an {$ifdef} arm => zero', 0,
    CountSrc(NewMissingMode, 'hygienearmmode.pas', cMissingModeId,
    cHygieneModeInIfdefArm));
  AssertEquals('a {$mode} below the interface keyword => zero', 0,
    CountSrc(NewMissingMode, 'hygienelatemode.pas', cMissingModeId,
    cHygieneModeAfterInterface));
  AssertEquals('a real include above the interface keyword => zero', 0,
    CountSrc(NewMissingMode, 'hygieneinclude.pas', cMissingModeId,
    cHygieneIncludeAboveInterface));
  AssertEquals('a real include below it may carry a mode too => zero', 0,
    CountSrc(NewMissingMode, 'hygienelateinclude.pas', cMissingModeId,
    cHygieneIncludeBelowInterface));
end;


procedure TRulesFpcStyleTest.MissingModeDirectiveSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingMode, 'hygienenomode.pas', cHygieneNoModeParseFailure,
      lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cMissingModeId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cMissingModeId);
    AssertEquals('same start line', 2, lc.Issues[k].StartLine);
    AssertEquals('same start col', 1, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.MissingCopyrightHeaderPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: nothing at or above the interface line names COPYING.FPC.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingCopyright, 'hygienenobanner.pas', cHygieneNoBanner, lc);
    AssertEquals('one missing-banner issue', 1,
      CountById(lc, cMissingCopyrightId));
    k := FirstById(lc, cMissingCopyrightId);
    AssertEquals('start line', 1, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 1, lc.Issues[k].EndLine);
    AssertEquals('end col', 4, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cMissingCopyrightId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('message names the module',
      'Module HygieneNoBanner has no COPYING.FPC reference in its leading '
      + 'comment',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingCopyright, 'mylibrary.lpr', cHygieneLibrary, lc);
    AssertEquals('a library is judged too', 1,
      CountById(lc, cMissingCopyrightId));
    k := FirstById(lc, cMissingCopyrightId);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end col', 7, lc.Issues[k].EndCol);
  finally
    lc.Free;
  end;

  AssertEquals('a program with no banner at all', 1,
    CountSrc(NewMissingCopyright, 'myproject.lpr', cMissingCopyrightId,
    cHygieneProgram));
  // For a program the window ends at the first begin.
  AssertEquals('a banner below the program keyword => zero', 0,
    CountSrc(NewMissingCopyright, 'surfacetyped.lpr', cMissingCopyrightId,
    cHygieneProgramInterfaceType));
  AssertEquals('the dsocksvr.pp banner layout => zero', 0,
    CountSrc(NewMissingCopyright, 'dsocksvr.pp', cMissingCopyrightId,
    cHygieneProgramBannerBelowKeyword));
  AssertEquals('an include below the interface keyword still measures', 1,
    CountSrc(NewMissingCopyright, 'hygienelateinclude.pas',
    cMissingCopyrightId, cHygieneIncludeBelowInterface));
  AssertEquals('the banner in a { } comment => zero', 0,
    CountSrc(NewMissingCopyright, 'hygienenomode.pas', cMissingCopyrightId,
    cHygieneNoMode));
  AssertEquals('the banner below a guarded unit clause => zero', 0,
    CountSrc(NewMissingCopyright, 'hygieneguarded.pas', cMissingCopyrightId,
    cHygieneBannerAfterGuardedUnit));
end;


procedure TRulesFpcStyleTest.MissingCopyrightHeaderIndirectSatisfaction;

begin
  AssertEquals('the banner in a // comment => zero', 0,
    CountSrc(NewMissingCopyright, 'hygienelinebanner.pas',
    cMissingCopyrightId, cHygieneBannerLineComment));
  AssertEquals('the banner in a (* *) comment, lower case => zero', 0,
    CountSrc(NewMissingCopyright, 'hygieneparenbanner.pas',
    cMissingCopyrightId, cHygieneBannerParenComment));
  AssertEquals('a real include above the interface keyword => zero', 0,
    CountSrc(NewMissingCopyright, 'hygieneinclude.pas', cMissingCopyrightId,
    cHygieneIncludeAboveInterface));
end;


procedure TRulesFpcStyleTest.MissingCopyrightHeaderSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMissingCopyright, 'hygienenobanner.pas',
      cHygieneNoBannerParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cMissingCopyrightId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cMissingCopyrightId);
    AssertEquals('same start line', 1, lc.Issues[k].StartLine);
    AssertEquals('same start col', 1, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesSilentWithoutModuleKeyword;

begin
  AssertEquals('no module keyword => zero case mismatches', 0,
    CountSrc(NewCaseMismatch, 'hygienepayload.pas', cCaseMismatchId,
    cHygieneNoModuleKeyword));
  AssertEquals('no module keyword => zero missing modes', 0,
    CountSrc(NewMissingMode, 'hygienepayload.pas', cMissingModeId,
    cHygieneNoModuleKeyword));
  AssertEquals('no module keyword => zero missing banners', 0,
    CountSrc(NewMissingCopyright, 'hygienepayload.pas', cMissingCopyrightId,
    cHygieneNoModuleKeyword));
  AssertEquals('a library hint modifier => zero case mismatches', 0,
    CountSrc(NewCaseMismatch, 'hygienehints.pas', cCaseMismatchId,
    cHygieneHintModifierOnly));
  AssertEquals('a library hint modifier => zero missing modes', 0,
    CountSrc(NewMissingMode, 'hygienehints.pas', cMissingModeId,
    cHygieneHintModifierOnly));
  AssertEquals('a library hint modifier => zero missing banners', 0,
    CountSrc(NewMissingCopyright, 'hygienehints.pas', cMissingCopyrightId,
    cHygieneHintModifierOnly));
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesSilentOnTruncatedModuleClause;

begin
  AssertEquals('a nameless clause => zero case mismatches', 0,
    CountSrc(NewCaseMismatch, 'truncated.pas', cCaseMismatchId,
    cHygieneTruncatedModule));
  AssertEquals('a nameless clause => zero missing modes', 0,
    CountSrc(NewMissingMode, 'truncated.pas', cMissingModeId,
    cHygieneTruncatedModule));
  AssertEquals('a nameless clause => zero missing banners', 0,
    CountSrc(NewMissingCopyright, 'truncated.pas', cMissingCopyrightId,
    cHygieneTruncatedModule));
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesSilentOnBodyWordModuleName;

begin
  AssertEquals('a body word is no module name => zero case mismatches', 0,
    CountSrc(NewCaseMismatch, 'bodyword.pas', cCaseMismatchId,
    cHygieneBodyWordAsName));
  AssertEquals('a body word is no module name => zero missing modes', 0,
    CountSrc(NewMissingMode, 'bodyword.pas', cMissingModeId,
    cHygieneBodyWordAsName));
  AssertEquals('a body word is no module name => zero missing banners', 0,
    CountSrc(NewMissingCopyright, 'bodyword.pas', cMissingCopyrightId,
    cHygieneBodyWordAsName));
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesSilentOnUndelimitedWindow;

begin
  AssertEquals('the include mirror => zero case mismatches', 0,
    CountSrc(NewCaseMismatch, 'System.Macuuid.pp', cCaseMismatchId,
    cHygieneNamespacedIncludeMirror));
  AssertEquals('the include mirror => zero missing modes', 0,
    CountSrc(NewMissingMode, 'System.Macuuid.pp', cMissingModeId,
    cHygieneNamespacedIncludeMirror));
  // A unit with no interface word leaves the window undelimited, which is the
  // shape of the 2841 generated mirror files.
  AssertEquals('the include mirror => zero missing banners', 0,
    CountSrc(NewMissingCopyright, 'System.Macuuid.pp', cMissingCopyrightId,
    cHygieneNamespacedIncludeMirror));
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesEitherGuardedClauseSatisfies;

begin
  AssertEquals('the satisfying clause in the else arm => zero', 0,
    CountSrc(NewCaseMismatch, 'hygienetwo.pp', cCaseMismatchId,
    cHygieneTwoClausesDottedFirst));
  AssertEquals('the satisfying clause in the first arm => zero', 0,
    CountSrc(NewCaseMismatch, 'hygienetwo.pp', cCaseMismatchId,
    cHygieneTwoClausesPlainFirst));
  // Two clauses are one module, so the absence verdict is reported once.
  AssertEquals('two clauses, one missing-banner issue', 1,
    CountSrc(NewMissingCopyright, 'hygienetwo.pp', cMissingCopyrightId,
    cHygieneTwoClausesDottedFirst));
  AssertEquals('two clauses, one missing-banner issue', 1,
    CountSrc(NewMissingCopyright, 'hygienetwo.pp', cMissingCopyrightId,
    cHygieneTwoClausesPlainFirst));
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesIgnoreIoCheckDirectives;

begin
  AssertEquals('{$I-} and friends are no include, so the mode is measured', 1,
    CountSrc(NewMissingMode, 'hygieneiocheck.pas', cMissingModeId,
    cHygieneIoCheckDirectives));
  AssertEquals('{$I-} and friends are no include, so the banner is measured', 1,
    CountSrc(NewMissingCopyright, 'hygieneiocheck.pas', cMissingCopyrightId,
    cHygieneIoCheckDirectives));
  AssertEquals('a split {$I-} is no include either, mode measured', 1,
    CountSrc(NewMissingMode, 'hygienesplit.pas', cMissingModeId,
    cHygieneSplitIoCheck));
  AssertEquals('a split {$I-} is no include either, banner measured', 1,
    CountSrc(NewMissingCopyright, 'hygienesplit.pas', cMissingCopyrightId,
    cHygieneSplitIoCheck));
end;


procedure TRulesFpcStyleTest.UnitHygieneRulesRegisterDisabled;

var
  i: Integer;
  lIds: array[0..2] of string;

begin
  lIds[0] := cCaseMismatchId;
  lIds[1] := cMissingModeId;
  lIds[2] := cMissingCopyrightId;
  for i := Low(lIds) to High(lIds) do
  begin
    AssertTrue(lIds[i] + ' registered', RuleRegistry.FindById(lIds[i]) <> nil);
    AssertFalse(lIds[i] + ' ships disabled',
      RuleRegistry.FindById(lIds[i]).Metadata.DefaultEnabled);
    AssertTrue(lIds[i] + ' carries a description',
      RuleRegistry.FindById(lIds[i]).Metadata.Description <> '');
  end;
end;


procedure TRulesFpcStyleTest.DeprecatedSymbolUsedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeprecatedSymbol, 'hintdeprecated.pas', cHintDeprecatedUse, lc);
    AssertEquals('one deprecated-use issue', 1, CountById(lc, cDeprecatedId));
    k := FirstById(lc, cDeprecatedId);
    AssertEquals('start line', 24, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 24, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cDeprecatedId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('message names the declaration',
      'Symbol OldValue is deprecated',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertResolvesClean('hintcompliant.pas', cHintCompliant);
  AssertEquals('no hint modifier anywhere => zero', 0,
    CountSrc(NewDeprecatedSymbol, 'hintcompliant.pas', cDeprecatedId,
    cHintCompliant));
  AssertResolvesClean('hintdeclared.pas', cHintDeclaredNotUsed);
  AssertEquals('declared but never used => zero', 0,
    CountSrc(NewDeprecatedSymbol, 'hintdeclared.pas', cDeprecatedId,
    cHintDeclaredNotUsed));
  AssertEquals('a platform deprecated const is one deprecated use', 1,
    CountSrc(NewDeprecatedSymbol, 'hintmulti.pas', cDeprecatedId,
    cHintMultiHint));
end;


procedure TRulesFpcStyleTest.DeprecatedSymbolUsedDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  AssertEquals('resolved => one issue', 1,
    CountSrc(NewDeprecatedSymbol, 'hintdeprecated.pas', cDeprecatedId,
    cHintDeprecatedUse));
  AssertEquals('resolution withheld => silent', 0,
    CountSrc(NewDeprecatedSymbol, 'hintdeprecated.pas', cDeprecatedId,
    cHintDeprecatedUse, True));

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeprecatedSymbol, 'hintbroken.pas', cHintParseFailure, lc);
    AssertEquals('a failed parse is silent', 0, CountById(lc, cDeprecatedId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.DeprecatedSymbolUsedSilentOnUnresolvedOperand;

begin
  AssertResolvesClean('hintblind.pas', cHintBlindSites);
  AssertEquals('a hint the reference pairing never answers for => zero', 0,
    CountSrc(NewDeprecatedSymbol, 'hintblind.pas', cDeprecatedId,
    cHintBlindSites));
end;


procedure TRulesFpcStyleTest.DeprecatedSymbolUsedRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cDeprecatedId);
  AssertTrue(cDeprecatedId + ' registered', lRule <> nil);
  AssertFalse(cDeprecatedId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cDeprecatedId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.PlatformSymbolUsedInPortableUnitPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPlatformSymbol, 'hintplatform.pas', cHintPlatformUse, lc);
    AssertEquals('one platform-use issue', 1, CountById(lc, cPlatformId));
    k := FirstById(lc, cPlatformId);
    AssertEquals('start line', 23, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 23, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cPlatformId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('message names the declaration',
      'Symbol Report is platform-specific and this unit is not marked platform',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertResolvesClean('hintplatformunit.pas', cHintPlatformUnit);
  AssertEquals('the same use inside a platform unit => zero', 0,
    CountSrc(NewPlatformSymbol, 'hintplatformunit.pas', cPlatformId,
    cHintPlatformUnit));
  AssertResolvesClean('hintcompliant.pas', cHintCompliant);
  AssertEquals('no hint modifier anywhere => zero', 0,
    CountSrc(NewPlatformSymbol, 'hintcompliant.pas', cPlatformId,
    cHintCompliant));
  AssertResolvesClean('hintdeclared.pas', cHintDeclaredNotUsed);
  AssertEquals('declared but never used => zero', 0,
    CountSrc(NewPlatformSymbol, 'hintdeclared.pas', cPlatformId,
    cHintDeclaredNotUsed));
  AssertEquals('a platform deprecated const is one platform use', 1,
    CountSrc(NewPlatformSymbol, 'hintmulti.pas', cPlatformId, cHintMultiHint));
end;


procedure TRulesFpcStyleTest.PlatformSymbolUsedInPortableUnitDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  AssertEquals('resolved => one issue', 1,
    CountSrc(NewPlatformSymbol, 'hintplatform.pas', cPlatformId,
    cHintPlatformUse));
  AssertEquals('resolution withheld => silent', 0,
    CountSrc(NewPlatformSymbol, 'hintplatform.pas', cPlatformId,
    cHintPlatformUse, True));

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPlatformSymbol, 'hintbroken.pas', cHintParseFailure, lc);
    AssertEquals('a failed parse is silent', 0, CountById(lc, cPlatformId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PlatformSymbolUsedInPortableUnitSilentOnUnresolvedOperand;

begin
  AssertResolvesClean('hintblind.pas', cHintBlindSites);
  AssertEquals('a hint the reference pairing never answers for => zero', 0,
    CountSrc(NewPlatformSymbol, 'hintblind.pas', cPlatformId, cHintBlindSites));
end;


procedure TRulesFpcStyleTest.PlatformSymbolUsedInPortableUnitRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cPlatformId);
  AssertTrue(cPlatformId + ' registered', lRule <> nil);
  AssertFalse(cPlatformId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cPlatformId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.ExperimentalSymbolUsedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewExperimentalSymbol, 'hintexperimental.pas',
      cHintExperimentalUse, lc);
    AssertEquals('one experimental-use issue', 1,
      CountById(lc, cExperimentalId));
    k := FirstById(lc, cExperimentalId);
    AssertEquals('start line', 24, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 24, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cExperimentalId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('message names the declaration',
      'Symbol ProvisionalTag is experimental',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  AssertResolvesClean('hintcompliant.pas', cHintCompliant);
  AssertEquals('no hint modifier anywhere => zero', 0,
    CountSrc(NewExperimentalSymbol, 'hintcompliant.pas', cExperimentalId,
    cHintCompliant));
  AssertResolvesClean('hintdeclared.pas', cHintDeclaredNotUsed);
  AssertEquals('declared but never used => zero', 0,
    CountSrc(NewExperimentalSymbol, 'hintdeclared.pas', cExperimentalId,
    cHintDeclaredNotUsed));
end;


procedure TRulesFpcStyleTest.ExperimentalSymbolUsedDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  AssertEquals('resolved => one issue', 1,
    CountSrc(NewExperimentalSymbol, 'hintexperimental.pas', cExperimentalId,
    cHintExperimentalUse));
  AssertEquals('resolution withheld => silent', 0,
    CountSrc(NewExperimentalSymbol, 'hintexperimental.pas', cExperimentalId,
    cHintExperimentalUse, True));

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewExperimentalSymbol, 'hintbroken.pas', cHintParseFailure, lc);
    AssertEquals('a failed parse is silent', 0, CountById(lc, cExperimentalId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.ExperimentalSymbolUsedSilentOnUnresolvedOperand;

begin
  AssertResolvesClean('hintblind.pas', cHintBlindSites);
  AssertEquals('a hint the reference pairing never answers for => zero', 0,
    CountSrc(NewExperimentalSymbol, 'hintblind.pas', cExperimentalId,
    cHintBlindSites));
end;


procedure TRulesFpcStyleTest.ExperimentalSymbolUsedRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cExperimentalId);
  AssertTrue(cExperimentalId + ' registered', lRule <> nil);
  AssertFalse(cExperimentalId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cExperimentalId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.PublicMethodUndocumentedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: the public 'procedure Undocumented;' of line 12 starts at
  // column 5 and its keyword spans nine bytes.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docmethods.pas', cDocMethodPositions,
      lc);
    AssertEquals('one undocumented-method issue', 1,
      CountById(lc, cMethodDocId));
    k := FirstById(lc, cMethodDocId);
    AssertEquals('start line', 12, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('end line', 12, lc.Issues[k].EndLine);
    AssertEquals('end col', 13, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cMethodDocId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('message names the method',
      'Method Undocumented has no preceding documentation comment',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docbreaks.pas', cDocMethodBreaks, lc);
    AssertEquals('two undocumented-method issues', 2,
      CountById(lc, cMethodDocId));
    k := NthById(lc, cMethodDocId, 0);
    AssertEquals('a blank line breaks the association', 12,
      lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    k := NthById(lc, cMethodDocId, 1);
    AssertEquals('a directive alone is no documentation', 14,
      lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;

  // A class method is reported at the 'class' keyword, which spans five bytes.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docclassmembers.pas', cDocClassMembers,
      lc);
    AssertEquals('one undocumented class method', 1,
      CountById(lc, cMethodDocId));
    k := FirstById(lc, cMethodDocId);
    AssertEquals('start line', 12, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('end col', 9, lc.Issues[k].EndCol);
    AssertEquals('message names the class method',
      'Method Reset has no preceding documentation comment',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'doctrailing.pas', cDocTrailingComment,
      lc);
    AssertEquals('one undocumented-method issue', 1,
      CountById(lc, cMethodDocId));
    k := FirstById(lc, cMethodDocId);
    AssertEquals('a trailing comment documents nothing', 11,
      lc.Issues[k].StartLine);
    AssertEquals('arg 0 is the method', 'Foo', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicMethodUndocumentedIndirectSatisfaction;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lHost: string;
  i: Integer;

begin
  AssertEquals('documented, private and strict-protected shapes => zero', 0,
    CountSrc(NewMethodUndocumented, 'docsatisfied.pas', cMethodDocId,
    cDocMethodSatisfied));
  AssertEquals('a comment on either side of an attribute run => zero', 0,
    CountSrc(NewMethodUndocumented, 'docattributes.pas', cMethodDocId,
    cDocAttributes));
  AssertEquals('an implicit visibility carries no specifier => zero', 0,
    CountSrc(NewMethodUndocumented, 'docnovisibility.pas', cMethodDocId,
    cDocNoVisibility));
  AssertEquals('two directives sharing one row stay transparent => zero', 0,
    CountSrc(NewMethodUndocumented, 'docsamerow.pas', cMethodDocId,
    cDocSameRowDirectives));

  AssertEquals('the payload written out is reported', 2,
    CountSrc(NewMethodUndocumented, 'docinclude.pas', cMethodDocId,
    cDocIncludeInlined));
  lFix := TTempFixtures.Create;
  try
    lFix.Add('docinc.inc', cDocIncludePayload);
    lHost := lFix.Add('docinclude.pas', cDocIncludeHost);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewMethodUndocumented, lHost, lc);
      AssertEquals('the include was read', 0, CountById(lc, cScanErrorId));
      AssertEquals('only the host member is reported', 1,
        CountById(lc, cMethodDocId));
      AssertEquals('arg 0 is the host member', 'InTheHost',
        lc.Issues[FirstById(lc, cMethodDocId)].MessageArgs[0]);
      for i := 0 to lc.Count - 1 do
        if lc.Issues[i].RuleId = cMethodDocId then
          AssertTrue('no row outside the 19-line host file',
            (lc.Issues[i].StartLine >= 1) and (lc.Issues[i].StartLine <= 19));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docconditional.pas', cDocConditional,
      lc);
    AssertEquals('only the member past the {$ENDIF} is reported', 1,
      CountById(lc, cMethodDocId));
    AssertEquals('arg 0 is that member', 'StillReported',
      lc.Issues[FirstById(lc, cMethodDocId)].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicMethodUndocumentedWalkStaysAligned;

var
  lc: TFpSonarIssueCollector;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docgenerics.pas', cDocGenerics, lc);
    AssertEquals('a generic member and the member after it', 2,
      CountById(lc, cMethodDocId));
    AssertEquals('the generic function', 12,
      lc.Issues[NthById(lc, cMethodDocId, 0)].StartLine);
    AssertEquals('arg 0 is the generic function', 'Take',
      lc.Issues[NthById(lc, cMethodDocId, 0)].MessageArgs[0]);
    AssertEquals('a record constraint opens no body', 13,
      lc.Issues[NthById(lc, cMethodDocId, 1)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docforwardintf.pas', cDocForwardIntf,
      lc);
    AssertEquals('a forward interface opens no body', 1,
      CountById(lc, cMethodDocId));
    AssertEquals('start line', 12,
      lc.Issues[FirstById(lc, cMethodDocId)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'doctypehelper.pas', cDocTypeHelper, lc);
    AssertEquals('the helper member and the one after its end', 2,
      CountById(lc, cMethodDocId));
    AssertEquals('the type helper member', 12,
      lc.Issues[NthById(lc, cMethodDocId, 0)].StartLine);
    AssertEquals('the class member after it', 17,
      lc.Issues[NthById(lc, cMethodDocId, 1)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docclassmodifiers.pas',
      cDocClassModifiers, lc);
    AssertEquals('abstract and sealed keep the section', 2,
      CountById(lc, cMethodDocId));
    AssertEquals('under class abstract', 10,
      lc.Issues[NthById(lc, cMethodDocId, 0)].StartLine);
    AssertEquals('under class sealed', 15,
      lc.Issues[NthById(lc, cMethodDocId, 1)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docpackedclass.pas', cDocPackedClass,
      lc);
    AssertEquals('the packed member and the one after its end', 2,
      CountById(lc, cMethodDocId));
    AssertEquals('the packed class member', 10,
      lc.Issues[NthById(lc, cMethodDocId, 0)].StartLine);
    AssertEquals('the class member after it', 15,
      lc.Issues[NthById(lc, cMethodDocId, 1)].StartLine);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'dochelperfirstvis.pas',
      cDocHelperFirstVis, lc);
    AssertEquals('a specifier opening a helper body is recognised', 1,
      CountById(lc, cMethodDocId));
    AssertEquals('start line', 11,
      lc.Issues[FirstById(lc, cMethodDocId)].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicMethodUndocumentedReportsInDelphiMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docdelphi.pas', cDocDelphiMode, lc);
    AssertEquals('one undocumented-method issue', 1,
      CountById(lc, cMethodDocId));
    k := FirstById(lc, cMethodDocId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('arg 0 is the method', 'Undocumented',
      lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicMethodUndocumentedSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docmethods.pas', cDocMethodParseFailure,
      lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cMethodDocId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cMethodDocId);
    AssertEquals('same start line', 12, lc.Issues[k].StartLine);
    AssertEquals('same start col', 5, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicMethodUndocumentedRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cMethodDocId);
  AssertTrue(cMethodDocId + ' registered', lRule <> nil);
  AssertFalse(cMethodDocId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cMethodDocId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfTokenStream', Ord(rfTokenStream),
    Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.PublicPropertyUndocumentedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: the published 'property X' of line 13 starts at column 5 and
  // its keyword spans eight bytes.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPropertyUndocumented, 'docprops.pas', cDocPropertyPositions,
      lc);
    AssertEquals('one undocumented-property issue', 1,
      CountById(lc, cPropertyDocId));
    k := FirstById(lc, cPropertyDocId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('end line', 13, lc.Issues[k].EndLine);
    AssertEquals('end col', 12, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cPropertyDocId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('message names the property',
      'Property X has no preceding documentation comment',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // A class property is reported at the 'class' keyword.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPropertyUndocumented, 'docclassmembers.pas',
      cDocClassMembers, lc);
    AssertEquals('one undocumented class property', 1,
      CountById(lc, cPropertyDocId));
    k := FirstById(lc, cPropertyDocId);
    AssertEquals('start line', 13, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('end col', 9, lc.Issues[k].EndCol);
    AssertEquals('message names the class property',
      'Property Total has no preceding documentation comment',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;

  // Neither rule answers for the other's member kind.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPropertyUndocumented, 'docmixed.pas', cDocMixed, lc);
    AssertEquals('exactly one property issue', 1,
      CountById(lc, cPropertyDocId));
    AssertEquals('no method issue from the property rule', 0,
      CountById(lc, cMethodDocId));
    k := FirstById(lc, cPropertyDocId);
    AssertEquals('the property line', 13, lc.Issues[k].StartLine);
    AssertEquals('arg 0 is the property', 'X', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewMethodUndocumented, 'docmixed.pas', cDocMixed, lc);
    AssertEquals('exactly one method issue', 1, CountById(lc, cMethodDocId));
    AssertEquals('no property issue from the method rule', 0,
      CountById(lc, cPropertyDocId));
    k := FirstById(lc, cMethodDocId);
    AssertEquals('the method line', 12, lc.Issues[k].StartLine);
    AssertEquals('arg 0 is the method', 'Act', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPropertyUndocumented, 'doctrailing.pas',
      cDocTrailingComment, lc);
    AssertEquals('one undocumented-property issue', 1,
      CountById(lc, cPropertyDocId));
    k := FirstById(lc, cPropertyDocId);
    AssertEquals('a trailing comment documents nothing', 13,
      lc.Issues[k].StartLine);
    AssertEquals('arg 0 is the property', 'Bar', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicPropertyUndocumentedReportsInDelphiMode;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPropertyUndocumented, 'docdelphi.pas', cDocDelphiMode, lc);
    AssertEquals('one undocumented-property issue', 1,
      CountById(lc, cPropertyDocId));
    k := FirstById(lc, cPropertyDocId);
    AssertEquals('start line', 14, lc.Issues[k].StartLine);
    AssertEquals('start col', 5, lc.Issues[k].StartCol);
    AssertEquals('arg 0 is the property', 'X', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicPropertyUndocumentedIndirectSatisfaction;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lHost: string;
  i: Integer;

begin
  AssertEquals('documented, private and protected properties => zero', 0,
    CountSrc(NewPropertyUndocumented, 'docpropsok.pas', cPropertyDocId,
    cDocPropertySatisfied));
  AssertEquals('a comment below an attribute run => zero', 0,
    CountSrc(NewPropertyUndocumented, 'docattributes.pas', cPropertyDocId,
    cDocAttributes));
  AssertEquals('an implicit visibility carries no specifier => zero', 0,
    CountSrc(NewPropertyUndocumented, 'docnovisibility.pas', cPropertyDocId,
    cDocNoVisibility));

  AssertEquals('the payload written out is reported', 2,
    CountSrc(NewPropertyUndocumented, 'docinclude.pas', cPropertyDocId,
    cDocIncludeInlined));
  lFix := TTempFixtures.Create;
  try
    lFix.Add('docinc.inc', cDocIncludePayload);
    lHost := lFix.Add('docinclude.pas', cDocIncludeHost);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewPropertyUndocumented, lHost, lc);
      AssertEquals('the include was read', 0, CountById(lc, cScanErrorId));
      AssertEquals('only the host member is reported', 1,
        CountById(lc, cPropertyDocId));
      AssertEquals('arg 0 is the host member', 'InTheHostToo',
        lc.Issues[FirstById(lc, cPropertyDocId)].MessageArgs[0]);
      for i := 0 to lc.Count - 1 do
        if lc.Issues[i].RuleId = cPropertyDocId then
          AssertTrue('no row outside the 19-line host file',
            (lc.Issues[i].StartLine >= 1) and (lc.Issues[i].StartLine <= 19));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicPropertyUndocumentedSurvivesParseFailure;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewPropertyUndocumented, 'docprops.pas',
      cDocPropertyParseFailure, lc);
    AssertEquals('count unchanged by the parse failure', 1,
      CountById(lc, cPropertyDocId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
    k := FirstById(lc, cPropertyDocId);
    AssertEquals('same start line', 13, lc.Issues[k].StartLine);
    AssertEquals('same start col', 5, lc.Issues[k].StartCol);
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.PublicPropertyUndocumentedRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cPropertyDocId);
  AssertTrue(cPropertyDocId + ' registered', lRule <> nil);
  AssertFalse(cPropertyDocId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cPropertyDocId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtTok', Ord(rtTok), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfTokenStream', Ord(rfTokenStream),
    Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.InterfaceUsesTooBroadPositions;

var
  lTmp: TTempFixtures;
  lFiles: TStringArray;
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lTmp := TTempFixtures.Create;
  try
    lFiles := WriteUsesProject(lTmp);
    lIndex := BuildIndex(lFiles);
    try
      lc := TFpSonarIssueCollector.Create;
      try
        RunRuleWithIndex(NewUsesTooBroad, lFiles[0], lIndex, lc);
        AssertEquals('one too-broad import', 1, CountById(lc, cUsesTooBroadId));
        k := FirstById(lc, cUsesTooBroadId);
        AssertEquals('start line', 9, lc.Issues[k].StartLine);
        AssertEquals('start col', 1, lc.Issues[k].StartCol);
        AssertEquals('end line', 9, lc.Issues[k].EndLine);
        AssertEquals('end col', 1, lc.Issues[k].EndCol);
        AssertEquals('message key', 'rule.' + cUsesTooBroadId + '.message',
          lc.Issues[k].MessageKey);
        AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
        AssertEquals('message names the import',
          'Interface uses clause names iub_broad, which no interface '
          + 'declaration references',
          FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
      finally
        lc.Free;
      end;
    finally
      lIndex.Free;
    end;
  finally
    lTmp.Free;
  end;
end;


procedure TRulesFpcStyleTest.InterfaceUsesTooBroadIndirectSatisfaction;

var
  lTmp: TTempFixtures;
  lFiles: TStringArray;
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;

begin
  lTmp := TTempFixtures.Create;
  try
    lFiles := WriteUsesProject(lTmp);
    lIndex := BuildIndex(lFiles);
    try
      AssertTrue('iub_broad is in the index',
        lIndex.InterfaceNames('iub_broad') <> nil);
      AssertTrue('iub_needed is in the index',
        lIndex.InterfaceNames('iub_needed') <> nil);
      AssertTrue('iub_implonly is in the index',
        lIndex.InterfaceNames('iub_implonly') <> nil);
      AssertTrue('iub_operator declares a helper',
        lIndex.UnitHasOperatorOrHelper('iub_operator'));
      AssertTrue('iub_initfinal carries an initialization section',
        lIndex.UnitHasInitFinal('iub_initfinal'));

      lc := TFpSonarIssueCollector.Create;
      try
        RunRuleWithIndex(NewUsesTooBroad, lFiles[0], lIndex, lc);
        AssertEquals('the interface names a type iub_needed exports', 0,
          CountArg(lc, cUsesTooBroadId, 'iub_needed'));
        AssertEquals('an operator or helper import is never reported', 0,
          CountArg(lc, cUsesTooBroadId, 'iub_operator'));
        AssertEquals('an init/final import is never reported', 0,
          CountArg(lc, cUsesTooBroadId, 'iub_initfinal'));
        AssertEquals('the implementation clause is not read', 0,
          CountArg(lc, cUsesTooBroadId, 'iub_implonly'));
        AssertEquals('only the unnamed interface import is reported', 1,
          CountArg(lc, cUsesTooBroadId, 'iub_broad'));
      finally
        lc.Free;
      end;
    finally
      lIndex.Free;
    end;
  finally
    lTmp.Free;
  end;
end;


procedure TRulesFpcStyleTest.InterfaceUsesTooBroadDegradesWithoutIndex;

var
  lTmp: TTempFixtures;
  lFiles: TStringArray;
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;
  lBroken: string;

begin
  lTmp := TTempFixtures.Create;
  try
    lFiles := WriteUsesProject(lTmp);
    lBroken := lTmp.Add('iub_broken.pas', cIubBroken);
    lIndex := BuildIndex(lFiles);
    try
      lc := TFpSonarIssueCollector.Create;
      try
        RunRuleWithIndex(NewUsesTooBroad, lFiles[0], lIndex, lc);
        AssertEquals('with an index => one issue', 1,
          CountById(lc, cUsesTooBroadId));
      finally
        lc.Free;
      end;

      lc := TFpSonarIssueCollector.Create;
      try
        RunRule(NewUsesTooBroad, lFiles[0], lc);
        AssertEquals('no project index => silent', 0,
          CountById(lc, cUsesTooBroadId));
        AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
      finally
        lc.Free;
      end;

      lc := TFpSonarIssueCollector.Create;
      try
        RunRuleWithIndex(NewUsesTooBroad, lBroken, lIndex, lc);
        AssertEquals('a failed parse is silent', 0,
          CountById(lc, cUsesTooBroadId));
        AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
      finally
        lc.Free;
      end;
    finally
      lIndex.Free;
    end;
  finally
    lTmp.Free;
  end;
end;


procedure TRulesFpcStyleTest.InterfaceUsesTooBroadRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cUsesTooBroadId);
  AssertTrue(cUsesTooBroadId + ' registered', lRule <> nil);
  AssertFalse(cUsesTooBroadId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cUsesTooBroadId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtUse', Ord(rtUse), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfAst', Ord(rfAst), Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewIOResultNotChecked, 'ioprobe.pas', cIoUnchecked, lc);
    // The trailing Reset leaves its result to the caller, so only line 23.
    AssertEquals('one unchecked I/O call', 1, CountById(lc, cIOResultId));
    k := FirstById(lc, cIOResultId);
    AssertEquals('start line', 23, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 23, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('message key', 'rule.' + cIOResultId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('message names the called routine',
      'Call to Reset under {$I-} is not followed by an IOResult check',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedReadsALowercaseSwitch;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewIOResultNotChecked, 'ioprobe.pas', cIoLowercaseSwitch, lc);
    AssertEquals('{$i-} opens the region as {$I-} does', 1,
      CountById(lc, cIOResultId));
    k := FirstById(lc, cIOResultId);
    AssertEquals('start line', 23, lc.Issues[k].StartLine);
    AssertEquals('end line', 23, lc.Issues[k].EndLine);
    AssertEquals('message names the called routine',
      'Call to Reset under {$I-} is not followed by an IOResult check',
      FormatMessage(lc.Issues[k].MessageKey, lc.Issues[k].MessageArgs));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentOnCheckedCall;

begin
  AssertResolvesClean('ioprobe.pas', cIoChecked);
  AssertEquals('an IOResult read between the two calls satisfies it', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId, cIoChecked));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentOnAnAssignedCheck;

begin
  AssertResolvesClean('ioprobe.pas', cIoAssignedCheck);
  AssertEquals('a check assigned to a local satisfies the site', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId,
    cIoAssignedCheck));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentOnPossibleIndirectCheck;

begin
  AssertResolvesClean('ioprobe.pas', cIoIndirectCheck);
  AssertEquals('a call between the two may read IOResult itself', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId,
    cIoIndirectCheck));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentOutsideTheRegion;

begin
  AssertResolvesClean('ioprobe.pas', cIoNoRegion);
  AssertEquals('with the checks on, the call raises instead', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId, cIoNoRegion));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentAfterTheRegionCloses;

begin
  AssertResolvesClean('ioprobe.pas', cIoRegionClosed);
  AssertEquals('the unchecked pair sits past the {$I+}', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId,
    cIoRegionClosed));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentWhenTheRegionEndsFirst;

begin
  AssertResolvesClean('ioprobe.pas', cIoRegionEndsFirst);
  AssertEquals('the next call is outside the region, so it checks', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId,
    cIoRegionEndsFirst));
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;

begin
  AssertEquals('resolution withheld => silent', 0,
    CountSrc(NewIOResultNotChecked, 'ioprobe.pas', cIOResultId, cIoUnchecked,
    True));

  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewIOResultNotChecked, 'iobroken.pas', cHintParseFailure, lc);
    AssertEquals('a failed parse is silent', 0, CountById(lc, cIOResultId));
    AssertEquals('no rule fault', 0, CountById(lc, cErrorId));
  finally
    lc.Free;
  end;
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedSilentOnInlineAssembler;

begin
  AssertResolvesClean('ioprobe.pas', cIoInlineAssembler);
  // A statement the classifier cannot answer for takes the routine out.
  CheckSilentWithLiveSibling(NewIOResultNotChecked, NewDeprecatedSymbol,
    'ioprobe.pas', cIOResultId, cDeprecatedId, cIoInlineAssembler);
end;


procedure TRulesFpcStyleTest.IOResultNotCheckedRegistersDisabled;

var
  lRule: TRuleBase;

begin
  lRule := RuleRegistry.FindById(cIOResultId);
  AssertTrue(cIOResultId + ' registered', lRule <> nil);
  AssertFalse(cIOResultId + ' ships disabled', lRule.Metadata.DefaultEnabled);
  AssertTrue(cIOResultId + ' carries a description',
    lRule.Metadata.Description <> '');
  AssertEquals('tier rtSem', Ord(rtSem), Ord(lRule.Metadata.Tier));
  AssertEquals('feed rfResolver', Ord(rfResolver), Ord(lRule.Metadata.Feed));
end;


procedure TRulesFpcStyleTest.FpcStyleRulesSelfRegisterGlobally;

begin
  AssertTrue('DottedUnitsBranchesInconsistent registered',
    RuleRegistry.FindById(cBranchesId) <> nil);
  AssertTrue('MissingDottedUnitsGuard registered',
    RuleRegistry.FindById(cMissingGuardId) <> nil);
  AssertTrue('UnitFileNameCaseMismatch registered',
    RuleRegistry.FindById(cCaseMismatchId) <> nil);
  AssertTrue('MissingModeDirective registered',
    RuleRegistry.FindById(cMissingModeId) <> nil);
  AssertTrue('MissingCopyrightHeader registered',
    RuleRegistry.FindById(cMissingCopyrightId) <> nil);
  AssertTrue('DeprecatedSymbolUsed registered',
    RuleRegistry.FindById(cDeprecatedId) <> nil);
  AssertTrue('PlatformSymbolUsedInPortableUnit registered',
    RuleRegistry.FindById(cPlatformId) <> nil);
  AssertTrue('ExperimentalSymbolUsed registered',
    RuleRegistry.FindById(cExperimentalId) <> nil);
  AssertTrue('PublicMethodUndocumented registered',
    RuleRegistry.FindById(cMethodDocId) <> nil);
  AssertTrue('PublicPropertyUndocumented registered',
    RuleRegistry.FindById(cPropertyDocId) <> nil);
  AssertTrue('InterfaceUsesTooBroad registered',
    RuleRegistry.FindById(cUsesTooBroadId) <> nil);
  AssertTrue('IOResultNotChecked registered',
    RuleRegistry.FindById(cIOResultId) <> nil);
  AssertFalse('DottedUnitsBranchesInconsistent ships disabled',
    RuleRegistry.FindById(cBranchesId).Metadata.DefaultEnabled);
  AssertFalse('MissingDottedUnitsGuard ships disabled',
    RuleRegistry.FindById(cMissingGuardId).Metadata.DefaultEnabled);
end;


initialization
  RegisterTest(TRulesFpcStyleTest);

end.
