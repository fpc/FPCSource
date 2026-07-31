{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the concurrency (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesConcurrency;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Resolver, FpSonar.Rules.Concurrency, UtstFixtures;

type
  { Concurrency rule position, silence, degradation and registration tests. }
  TRulesConcurrencyTest = class(TTestCase)
  private
    { Runs aRule (taken into a fresh local registry, freed here) over aFixture
      with the rule explicitly enabled, collecting into aCollector. aWithhold
      withholds resolution the way the silence sweep's degraded pass does. }
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector; aWithhold: boolean = False);
    // As RunRule, but the fixture source is supplied inline, one array element
    // per source line, and materialised to a temp dir for the run.
    procedure RunRuleSrc(aRule: TRuleBase; const aName: string;
      const aSrc: array of string; const aCollector: TFpSonarIssueCollector;
      aWithhold: boolean = False);
    // A config enabling aRuleId and nothing else.
    function EnabledConfig(const aRuleId: string): TFpSonarConfig;
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // How often aId fires when aRule runs over the inline source aSrc.
    function CountSrc(aRule: TRuleBase; const aName, aId: string;
      const aSrc: array of string; aWithhold: boolean = False): Integer;
    // Asserts aSrc resolves clean, so a zero-issue assert over it is a
    // measurement and not a vacuous silent-skip.
    procedure AssertResolvesClean(const aName: string;
      const aSrc: array of string);
    { Asserts aRule fires over aSrc exactly once, at aRow, column 1, with key
      rule.<aId>.message and message args aArgs. }
    procedure CheckIssueAt(aRule: TRuleBase; const aId, aName: string;
      const aSrc: array of string; aRow: Integer;
      const aArgs: array of string);
    // As CheckIssueAt, but the fixture is not required to resolve.
    procedure CheckAstIssueAt(aRule: TRuleBase; const aId, aName: string;
      const aSrc: array of string; aRow: Integer;
      const aArgs: array of string);
    // Asserts aRule reports nothing over aSrc, which resolves clean.
    procedure CheckSilent(aRule: TRuleBase; const aId, aName: string;
      const aSrc: array of string);
    { Fresh, separately-owned rule instances (metadata mirrors the unit's
      self-registration; empty key defaults to rule.<RuleId>.message). }
    function NewGlobalWrittenFromThreadRoutine: TRuleBase;
    function NewSynchronizeWithLockHeld: TRuleBase;
    function NewCriticalSectionNotInitialized: TRuleBase;
    function NewVclAccessOffMainThread: TRuleBase;
    function NewThreadvarInitialization: TRuleBase;
  published
    procedure GlobalWrittenFromThreadRoutinePositions;
    procedure GlobalWrittenFromThreadRoutineReportsWriteAfterLeave;
    procedure GlobalWrittenFromThreadRoutineSilentOutsideThreadRoutine;
    procedure GlobalWrittenFromThreadRoutineSilentOnLocalWrite;
    procedure GlobalWrittenFromThreadRoutineDegradesWithoutResolver;
    procedure GlobalWrittenFromThreadRoutineSilentOnUnresolvedOperand;
    procedure GlobalWrittenFromThreadRoutineSilentOnSectionOperand;
    procedure SynchronizeWithLockHeldPositions;
    procedure SynchronizeWithLockHeldReportsFieldSection;
    procedure SynchronizeWithLockHeldReportsQueueOnOneBranch;
    procedure SynchronizeWithLockHeldSilentWithoutEnter;
    procedure SynchronizeWithLockHeldDegradesWithoutResolver;
    procedure SynchronizeWithLockHeldSilentOnUnresolvedOperand;
    procedure CriticalSectionNotInitializedPositions;
    procedure CriticalSectionNotInitializedReportsSingleBranchInit;
    procedure CriticalSectionNotInitializedSilentOnFieldAndGlobalSection;
    procedure CriticalSectionNotInitializedSilentOnEscapedSection;
    procedure CriticalSectionNotInitializedSilentOnHelperInit;
    procedure CriticalSectionNotInitializedSilentOnAbsoluteAlias;
    procedure CriticalSectionNotInitializedDegradesWithoutResolver;
    procedure CriticalSectionNotInitializedSilentOnUnresolvedOperand;
    procedure VclAccessOffMainThreadPositions;
    procedure VclAccessOffMainThreadSilentInsideSynchronize;
    procedure VclAccessOffMainThreadSilentOnNonThreadClass;
    procedure VclAccessOffMainThreadReportsIndirectDescendant;
    procedure VclAccessOffMainThreadReportsUndeclaredAncestor;
    procedure VclAccessOffMainThreadReportsChainedAccessOnce;
    procedure VclAccessOffMainThreadReportsCuratedQualifier;
    procedure VclAccessOffMainThreadSilentOnNonUiMember;
    procedure VclAccessOffMainThreadDegradesOnParseFailure;
    procedure ThreadvarInitializationPositions;
    procedure ThreadvarInitializationReportsInitializationOnlyWrite;
    procedure ThreadvarInitializationSilentOnWriteByReference;
    procedure ThreadvarInitializationSilentOnIncDec;
    procedure ThreadvarInitializationSilentOnInitializationOnlyRead;
    procedure ThreadvarInitializationSilentOnRecordMemberWrite;
    procedure ThreadvarInitializationSilentOnWithMemberWrite;
    procedure ThreadvarInitializationSilentOnAsmRoutineBody;
    procedure ThreadvarInitializationSilentOnAddressTaken;
    procedure ThreadvarInitializationSilentOnInterfaceThreadvar;
    procedure ThreadvarInitializationDegradesWithoutResolver;
    procedure ThreadvarInitializationSilentOnUnresolvedOperand;
    procedure ConcurrencyRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cGlobalWrittenFromThreadRoutineId = 'GlobalWrittenFromThreadRoutine';
  cSynchronizeWithLockHeldId = 'SynchronizeWithLockHeld';
  cCriticalSectionNotInitializedId = 'CriticalSectionNotInitialized';
  cVclAccessOffMainThreadId = 'VclAccessOffMainThread';
  cThreadvarInitializationId = 'ThreadvarInitialization';

  cThreadWriteNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    'var',
    '  gCounter: Integer;',
    'implementation',
    'procedure TWorker.Execute;',
    'begin',
    '  gCounter := 1;',
    'end;',
    'end.');

  cThreadWriteGuarded: array[0..31] of string = (
    'unit guarded;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'var',
    '  gCounter: Integer;',
    '  gSection: TRTLCriticalSection;',
    'implementation',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  EnterCriticalSection(gSection);',
    '  gCounter := 1;',
    '  LeaveCriticalSection(gSection);',
    'end;',
    'end.');

  cThreadWriteAfterLeave: array[0..31] of string = (
    'unit afterleave;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'var',
    '  gCounter: Integer;',
    '  gSection: TRTLCriticalSection;',
    'implementation',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  EnterCriticalSection(gSection);',
    '  LeaveCriticalSection(gSection);',
    '  gCounter := 1;',
    'end;',
    'end.');

  cThreadWriteOutsideExecute: array[0..22] of string = (
    'unit outside;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Update;',
    '    procedure Execute;',
    '  end;',
    'var',
    '  gCounter: Integer;',
    'implementation',
    'procedure TWorker.Update;',
    'begin',
    '  gCounter := 1;',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  gCounter := 2;',
    'end;',
    'end.');

  cThreadWriteLocal: array[0..20] of string = (
    'unit locals;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    'var',
    '  gCounter: Integer;',
    'implementation',
    'procedure TWorker.Execute;',
    'var',
    '  lLocal: Integer;',
    'begin',
    '  lLocal := 1;',
    '  gCounter := lLocal;',
    'end;',
    'end.');

  cThreadWriteUnanswerable: array[0..28] of string = (
    'unit unanswerable;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TThread = class(TObject)',
    '  end;',
    '  TQuiet = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    'var',
    '  gCounter: Integer;',
    'implementation',
    'procedure TQuiet.Execute;',
    'begin',
    '  gCounter := 1;',
    '  asm',
    '    nop',
    '  end;',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  gCounter := 2;',
    'end;',
    'end.');

  cSyncWhileHeld: array[0..49] of string = (
    'unit synchronize;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThreadMethod = procedure of object;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aMethod: TThreadMethod);',
    '    procedure Queue(aMethod: TThreadMethod);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Report;',
    '    procedure Run;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'implementation',
    'procedure TThread.Synchronize(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure TThread.Queue(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Report;',
    'begin',
    'end;',
    'procedure TWorker.Run;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  EnterCriticalSection(lSection);',
    '  Synchronize(@Report);',
    '  LeaveCriticalSection(lSection);',
    'end;',
    'end.');

  cSyncAfterLeave: array[0..49] of string = (
    'unit afterleave;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThreadMethod = procedure of object;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aMethod: TThreadMethod);',
    '    procedure Queue(aMethod: TThreadMethod);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Report;',
    '    procedure Run;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'implementation',
    'procedure TThread.Synchronize(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure TThread.Queue(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Report;',
    'begin',
    'end;',
    'procedure TWorker.Run;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  EnterCriticalSection(lSection);',
    '  LeaveCriticalSection(lSection);',
    '  Synchronize(@Report);',
    'end;',
    'end.');

  cSyncWithoutEnter: array[0..44] of string = (
    'unit noenter;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThreadMethod = procedure of object;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aMethod: TThreadMethod);',
    '    procedure Queue(aMethod: TThreadMethod);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Report;',
    '    procedure Run;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'implementation',
    'procedure TThread.Synchronize(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure TThread.Queue(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Report;',
    'begin',
    'end;',
    'procedure TWorker.Run;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  Synchronize(@Report);',
    '  LeaveCriticalSection(lSection);',
    'end;',
    'end.');

  cQueueOnOneBranch: array[0..49] of string = (
    'unit onebranch;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThreadMethod = procedure of object;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aMethod: TThreadMethod);',
    '    procedure Queue(aMethod: TThreadMethod);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Report;',
    '    procedure Run(b: Boolean);',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'implementation',
    'procedure TThread.Synchronize(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure TThread.Queue(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Report;',
    'begin',
    'end;',
    'procedure TWorker.Run(b: Boolean);',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  if b then',
    '    EnterCriticalSection(lSection);',
    '  Queue(@Report);',
    'end;',
    'end.');

  cSyncUnanswerable: array[0..56] of string = (
    'unit syncunanswerable;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThreadMethod = procedure of object;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aMethod: TThreadMethod);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Report;',
    '    procedure Quiet;',
    '    procedure Run;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'implementation',
    'procedure TThread.Synchronize(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Report;',
    'begin',
    'end;',
    'procedure TWorker.Quiet;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  EnterCriticalSection(lSection);',
    '  Synchronize(@Report);',
    '  asm',
    '    nop',
    '  end;',
    'end;',
    'procedure TWorker.Run;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  EnterCriticalSection(lSection);',
    '  Synchronize(@Report);',
    'end;',
    'end.');

  cSectionNeverInitialized: array[0..23] of string = (
    'unit sections;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Run;',
    'implementation',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lSection);',
    'end;',
    'end.');

  cSectionInitializedFirst: array[0..24] of string = (
    'unit initialized;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Run;',
    'implementation',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  EnterCriticalSection(lSection);',
    'end;',
    'end.');

  cSectionInitializedOneBranch: array[0..25] of string = (
    'unit onebranchinit;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Run(b: Boolean);',
    'implementation',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Run(b: Boolean);',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  if b then',
    '    InitCriticalSection(lSection);',
    '  EnterCriticalSection(lSection);',
    'end;',
    'end.');

  cSectionFieldAndGlobal: array[0..31] of string = (
    'unit fields;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  THolder = class(TObject)',
    '  public',
    '    FSection: TRTLCriticalSection;',
    '    procedure Lock;',
    '  end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Loose;',
    'var',
    '  gSection: TRTLCriticalSection;',
    'implementation',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure THolder.Lock;',
    'begin',
    '  EnterCriticalSection(FSection);',
    '  EnterCriticalSection(gSection);',
    'end;',
    'procedure Loose;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lSection);',
    'end;',
    'end.');

  cSectionEscapes: array[0..32] of string = (
    'unit escapes;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  PRTLCriticalSection = ^TRTLCriticalSection;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Take(aSection: PRTLCriticalSection);',
    'procedure Escaped;',
    'procedure Loose;',
    'implementation',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Take(aSection: PRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Escaped;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  Take(@lSection);',
    '  EnterCriticalSection(lSection);',
    'end;',
    'procedure Loose;',
    'var',
    '  lOther: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lOther);',
    'end;',
    'end.');

  cSectionUnanswerable: array[0..29] of string = (
    'unit sectionunanswerable;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Quiet;',
    'procedure Loose;',
    'implementation',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Quiet;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lSection);',
    '  asm',
    '    nop',
    '  end;',
    'end;',
    'procedure Loose;',
    'var',
    '  lOther: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lOther);',
    'end;',
    'end.');

  cSyncFieldSection: array[0..39] of string = (
    'unit fieldsection;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TThreadMethod = procedure of object;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aMethod: TThreadMethod);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FSection: TRTLCriticalSection;',
    '    procedure Report;',
    '    procedure Run;',
    '  end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'implementation',
    'procedure TThread.Synchronize(aMethod: TThreadMethod);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure LeaveCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Report;',
    'begin',
    'end;',
    'procedure TWorker.Run;',
    'begin',
    '  EnterCriticalSection(FSection);',
    '  Synchronize(@Report);',
    '  LeaveCriticalSection(FSection);',
    'end;',
    'end.');

  cThreadWriteIndexedSection: array[0..36] of string = (
    'unit indexed;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    '  TSections = array[0..1] of TRTLCriticalSection;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    '  TPlain = class(TThread)',
    '  public',
    '    procedure Execute;',
    '  end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'var',
    '  gCounter: Integer;',
    '  gOther: Integer;',
    '  gSections: TSections;',
    'implementation',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  EnterCriticalSection(gSections[0]);',
    '  gCounter := 1;',
    'end;',
    'procedure TPlain.Execute;',
    'begin',
    '  gOther := 1;',
    'end;',
    'end.');

  cSectionHelperInit: array[0..36] of string = (
    'unit helperinit;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Setup(var aSection: TRTLCriticalSection);',
    'procedure Indirect;',
    'procedure Loose;',
    'implementation',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Setup(var aSection: TRTLCriticalSection);',
    'begin',
    '  InitCriticalSection(aSection);',
    'end;',
    'procedure Indirect;',
    'var',
    '  lSection: TRTLCriticalSection;',
    'begin',
    '  Setup(lSection);',
    '  EnterCriticalSection(lSection);',
    'end;',
    'procedure Loose;',
    'var',
    '  lOther: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lOther);',
    'end;',
    'end.');

  cSectionAbsoluteAlias: array[0..32] of string = (
    'unit alias;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRTLCriticalSection = record',
    '    Handle: Pointer;',
    '  end;',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'procedure Aliased;',
    'procedure Loose;',
    'implementation',
    'procedure InitCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure EnterCriticalSection(var aSection: TRTLCriticalSection);',
    'begin',
    'end;',
    'procedure Aliased;',
    'var',
    '  lSection: TRTLCriticalSection;',
    '  lAlias: TRTLCriticalSection absolute lSection;',
    'begin',
    '  InitCriticalSection(lSection);',
    '  EnterCriticalSection(lAlias);',
    'end;',
    'procedure Loose;',
    'var',
    '  lOther: TRTLCriticalSection;',
    'begin',
    '  EnterCriticalSection(lOther);',
    'end;',
    'end.');

  cVclAccessNoncompliant: array[0..20] of string = (
    'unit vclaccess;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TWorker.Execute;',
    'begin',
    '  FLabel.Caption := ''busy'';',
    'end;',
    'end.');

  cVclAccessSynchronized: array[0..30] of string = (
    'unit vclsync;',
    '{$mode objfpc}{$H+}',
    '{$modeswitch functionreferences}',
    '{$modeswitch anonymousfunctions}',
    'interface',
    'type',
    '  TNotify = reference to procedure;',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '    Hint: string;',
    '  end;',
    '  TThread = class(TObject)',
    '  public',
    '    procedure Synchronize(aProc: TNotify);',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TThread.Synchronize(aProc: TNotify);',
    'begin',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  Synchronize(procedure begin FLabel.Caption := ''busy''; end);',
    '  FLabel.Hint := ''wait'';',
    'end;',
    'end.');

  cVclAccessNonThreadClass: array[0..29] of string = (
    'unit vclplain;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TPlain = class(TObject)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TPlain.Execute;',
    'begin',
    '  FLabel.Caption := ''idle'';',
    'end;',
    'procedure TWorker.Execute;',
    'begin',
    '  FLabel.Caption := ''busy'';',
    'end;',
    'end.');

  cVclAccessIndirectDescendant: array[0..22] of string = (
    'unit vclindirect;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TBase = class(TThread)',
    '  end;',
    '  TWorker = class(TBase)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TWorker.Execute;',
    'begin',
    '  FLabel.Caption := ''busy'';',
    'end;',
    'end.');

  cVclAccessChainedMember: array[0..25] of string = (
    'unit vclchain;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFont = class(TObject)',
    '  public',
    '    Color: Integer;',
    '  end;',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '    Font: TFont;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TWorker.Execute;',
    'begin',
    '  FLabel.Font.Color := 1;',
    'end;',
    'end.');

  cVclAccessChainedNonUiTail: array[0..25] of string = (
    'unit vcltail;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFont = class(TObject)',
    '  public',
    '    Size: Integer;',
    '  end;',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '    Font: TFont;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TWorker.Execute;',
    'begin',
    '  FLabel.Font.Size := 1;',
    'end;',
    'end.');

  cVclAccessUndeclaredAncestor: array[0..18] of string = (
    'unit vclforeign;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TWorker.Execute;',
    'begin',
    '  FLabel.Caption := ''busy'';',
    'end;',
    'end.');

  cVclAccessNonUiMember: array[0..34] of string = (
    'unit vclother;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TLabel = class(TObject)',
    '  public',
    '    Caption: string;',
    '  end;',
    '  TItemList = class(TObject)',
    '  public',
    '    Items: array[0..1] of Integer;',
    '  end;',
    '  TByteStream = class(TObject)',
    '  public',
    '    Position: Int64;',
    '  end;',
    '  TThread = class(TObject)',
    '  end;',
    '  TWorker = class(TThread)',
    '  public',
    '    FLabel: TLabel;',
    '    FList: TItemList;',
    '    FStream: TByteStream;',
    '    procedure Execute;',
    '  end;',
    'implementation',
    'procedure TWorker.Execute;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  lValue := FList.Items[0];',
    '  FStream.Position := lValue;',
    '  FLabel.Caption := ''busy'';',
    'end;',
    'end.');

  cVclAccessUnparseable: array[0..11] of string = (
    'unit vclbroken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TWorker = class(TObject',
    '  end;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'begin',
    'end;',
    'end.');

  cThreadvarNeverAssigned: array[0..13] of string = (
    'unit tvnever;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  lValue := gDepth;',
    'end;',
    'end.');

  cThreadvarAssignedInRoutine: array[0..20] of string = (
    'unit tvassigned;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    '  gNested: Integer;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    '  procedure Bump;',
    '  begin',
    '    gNested := 2;',
    '  end;',
    'begin',
    '  gDepth := 1;',
    '  Bump;',
    '  lValue := gDepth + gNested;',
    'end;',
    'end.');

  cThreadvarInitializationOnly: array[0..15] of string = (
    'unit tvinit;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  lValue := gDepth;',
    'end;',
    'initialization',
    '  gDepth := 1;',
    'end.');

  cThreadvarByReference: array[0..18] of string = (
    'unit tvbyref;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Bump(var aValue: Integer);',
    'begin',
    '  aValue := 1;',
    'end;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  Bump(gDepth);',
    '  lValue := gDepth;',
    'end;',
    'end.');

  cThreadvarIncDec: array[0..17] of string = (
    'unit tvincdec;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    '  gOther: Integer;',
    '  gPlain: Integer;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  Inc(gDepth);',
    '  Dec(gOther);',
    '  lValue := gDepth + gOther + gPlain;',
    'end;',
    'end.');

  cThreadvarInitializationRead: array[0..14] of string = (
    'unit tvinitread;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Run;',
    'begin',
    'end;',
    'initialization',
    '  gDepth := 1;',
    '  if gDepth = 1 then',
    '    Run;',
    'end.');

  cThreadvarRecordMember: array[0..18] of string = (
    'unit tvrecord;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRec = record',
    '    N: Integer;',
    '  end;',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gRec: TRec;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  gRec.N := 1;',
    '  lValue := gRec.N;',
    'end;',
    'end.');

  cThreadvarWithMemberWrite: array[0..20] of string = (
    'unit tvwith;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRec = record',
    '    N: Integer;',
    '  end;',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gRec: TRec;',
    '  gPlain: Integer;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  with gRec do',
    '    N := 1;',
    '  lValue := gPlain;',
    'end;',
    'end.');

  cThreadvarAsmBody: array[0..16] of string = (
    'unit tvasmbody;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Quiet; assembler;',
    'asm',
    'end;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  lValue := gDepth;',
    'end;',
    'end.');

  cThreadvarAddressTaken: array[0..19] of string = (
    'unit tvaddr;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  PInt = ^Integer;',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Take(aPtr: PInt);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  Take(@gDepth);',
    '  lValue := gDepth;',
    'end;',
    'end.');

  cThreadvarInInterface: array[0..15] of string = (
    'unit tvintf;',
    '{$mode objfpc}{$H+}',
    'interface',
    'threadvar',
    '  gShared: Integer;',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gLocal: Integer;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  lValue := gShared + gLocal;',
    'end;',
    'end.');

  cThreadvarUnanswerable: array[0..19] of string = (
    'unit tvasm;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'threadvar',
    '  gDepth: Integer;',
    'procedure Quiet;',
    'begin',
    '  asm',
    '    nop',
    '  end;',
    'end;',
    'procedure Run;',
    'var',
    '  lValue: Integer;',
    'begin',
    '  lValue := gDepth;',
    'end;',
    'end.');


function TRulesConcurrencyTest.EnabledConfig(
  const aRuleId: string): TFpSonarConfig;

begin
  Result := TFpSonarConfig.Default;
  SetLength(Result.Rules, 0);
  SetLength(Result.Rules, 1);
  Result.Rules[0].RuleId := aRuleId;
  Result.Rules[0].HasEnabled := True;
  Result.Rules[0].Enabled := True;
end;


procedure TRulesConcurrencyTest.RunRule(aRule: TRuleBase;
  const aFixture: string; const aCollector: TFpSonarIssueCollector;
  aWithhold: boolean = False);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Config := EnabledConfig(aRule.Metadata.RuleId);
    // aRealRtl puts objpas in every implicit uses chain; it is absent from the
    // synthetic registry.
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


procedure TRulesConcurrencyTest.RunRuleSrc(aRule: TRuleBase;
  const aName: string; const aSrc: array of string;
  const aCollector: TFpSonarIssueCollector; aWithhold: boolean = False);

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


function TRulesConcurrencyTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesConcurrencyTest.FirstById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Exit(i);
end;


function TRulesConcurrencyTest.CountSrc(aRule: TRuleBase;
  const aName, aId: string; const aSrc: array of string;
  aWithhold: boolean = False): Integer;

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


procedure TRulesConcurrencyTest.AssertResolvesClean(const aName: string;
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


procedure TRulesConcurrencyTest.CheckIssueAt(aRule: TRuleBase;
  const aId, aName: string; const aSrc: array of string; aRow: Integer;
  const aArgs: array of string);

begin
  AssertResolvesClean(aName, aSrc);
  CheckAstIssueAt(aRule, aId, aName, aSrc, aRow, aArgs);
end;


procedure TRulesConcurrencyTest.CheckAstIssueAt(aRule: TRuleBase;
  const aId, aName: string; const aSrc: array of string; aRow: Integer;
  const aArgs: array of string);

var
  lc: TFpSonarIssueCollector;
  k, m: Integer;

begin
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(aRule, aName, aSrc, lc);
    AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
    k := FirstById(lc, aId);
    AssertEquals('start line', aRow, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', aRow, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('arg count', Length(aArgs), Length(lc.Issues[k].MessageArgs));
    for m := 0 to High(aArgs) do
      AssertEquals('arg ' + IntToStr(m), aArgs[m], lc.Issues[k].MessageArgs[m]);
  finally
    lc.Free;
  end;
end;


procedure TRulesConcurrencyTest.CheckSilent(aRule: TRuleBase;
  const aId, aName: string; const aSrc: array of string);

begin
  AssertResolvesClean(aName, aSrc);
  AssertEquals(aName + ' => zero', 0, CountSrc(aRule, aName, aId, aSrc));
end;


function TRulesConcurrencyTest.NewGlobalWrittenFromThreadRoutine: TRuleBase;

begin
  Result := TRuleGlobalWrittenFromThreadRoutine.Create(TRuleMetadata.Make(
    cGlobalWrittenFromThreadRoutineId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, ''));
end;


function TRulesConcurrencyTest.NewSynchronizeWithLockHeld: TRuleBase;

begin
  Result := TRuleSynchronizeWithLockHeld.Create(TRuleMetadata.Make(
    cSynchronizeWithLockHeldId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, ''));
end;


function TRulesConcurrencyTest.NewCriticalSectionNotInitialized: TRuleBase;

begin
  Result := TRuleCriticalSectionNotInitialized.Create(TRuleMetadata.Make(
    cCriticalSectionNotInitializedId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, ''));
end;


function TRulesConcurrencyTest.NewVclAccessOffMainThread: TRuleBase;

begin
  Result := TRuleVclAccessOffMainThread.Create(TRuleMetadata.Make(
    cVclAccessOffMainThreadId, rtAst, rfAst, sevMajor, itBug, cfLow, False,
    ''));
end;


function TRulesConcurrencyTest.NewThreadvarInitialization: TRuleBase;

begin
  Result := TRuleThreadvarInitialization.Create(TRuleMetadata.Make(
    cThreadvarInitializationId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, ''));
end;


procedure TRulesConcurrencyTest.GlobalWrittenFromThreadRoutinePositions;

begin
  // Noncompliant: TWorker.Execute writes the unit-level gCounter with no
  // section anywhere in the routine.
  CheckIssueAt(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'noncompliant.pas',
    cThreadWriteNoncompliant, 16, ['gCounter']);
  // Compliant: the same write, between an Enter and a Leave of gSection.
  CheckSilent(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'guarded.pas', cThreadWriteGuarded);
end;


procedure TRulesConcurrencyTest.
  GlobalWrittenFromThreadRoutineReportsWriteAfterLeave;

begin
  // The section is free again on every path reaching the write.
  CheckIssueAt(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'afterleave.pas',
    cThreadWriteAfterLeave, 30, ['gCounter']);
end;


procedure TRulesConcurrencyTest.
  GlobalWrittenFromThreadRoutineSilentOutsideThreadRoutine;

begin
  // TWorker.Update carries the same write as TWorker.Execute; only Execute is a
  // thread routine.
  CheckIssueAt(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'outside.pas',
    cThreadWriteOutsideExecute, 21, ['gCounter']);
end;


procedure TRulesConcurrencyTest.GlobalWrittenFromThreadRoutineSilentOnLocalWrite;

begin
  // The routine local written at row 18 is not unit-level storage; the global
  // write at row 19 is the live control.
  CheckIssueAt(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'locals.pas', cThreadWriteLocal, 19,
    ['gCounter']);
end;


procedure TRulesConcurrencyTest.
  GlobalWrittenFromThreadRoutineDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewGlobalWrittenFromThreadRoutine, 'noncompliant.pas',
      cGlobalWrittenFromThreadRoutineId, cThreadWriteNoncompliant, True));
end;


procedure TRulesConcurrencyTest.
  GlobalWrittenFromThreadRoutineSilentOnUnresolvedOperand;

begin
  // TQuiet.Execute carries an asm statement no statement-access answer covers.
  CheckIssueAt(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'unanswerable.pas',
    cThreadWriteUnanswerable, 27, ['gCounter']);
end;


procedure TRulesConcurrencyTest.
  GlobalWrittenFromThreadRoutineSilentOnSectionOperand;

begin
  // The Enter at row 30 names gSections[0], which no slot stands for.
  CheckIssueAt(NewGlobalWrittenFromThreadRoutine,
    cGlobalWrittenFromThreadRoutineId, 'indexed.pas',
    cThreadWriteIndexedSection, 35, ['gOther']);
end;


procedure TRulesConcurrencyTest.SynchronizeWithLockHeldPositions;

begin
  // Noncompliant: Synchronize between the Enter and the Leave of lSection.
  CheckIssueAt(NewSynchronizeWithLockHeld, cSynchronizeWithLockHeldId,
    'synchronize.pas', cSyncWhileHeld, 47, ['Synchronize', 'lSection']);
  // Compliant: the same call, after the Leave.
  CheckSilent(NewSynchronizeWithLockHeld, cSynchronizeWithLockHeldId,
    'afterleave.pas', cSyncAfterLeave);
end;


procedure TRulesConcurrencyTest.SynchronizeWithLockHeldReportsFieldSection;

begin
  // A field is silent for the init verdict but still tracked for held state.
  CheckIssueAt(NewSynchronizeWithLockHeld, cSynchronizeWithLockHeldId,
    'fieldsection.pas', cSyncFieldSection, 37, ['Synchronize', 'FSection']);
end;


procedure TRulesConcurrencyTest.SynchronizeWithLockHeldReportsQueueOnOneBranch;

begin
  // The section is entered on one branch only, so it is held on some path.
  CheckIssueAt(NewSynchronizeWithLockHeld, cSynchronizeWithLockHeldId,
    'onebranch.pas', cQueueOnOneBranch, 48, ['Queue', 'lSection']);
end;


procedure TRulesConcurrencyTest.SynchronizeWithLockHeldSilentWithoutEnter;

begin
  // The section is initialised and left, never entered.
  CheckSilent(NewSynchronizeWithLockHeld, cSynchronizeWithLockHeldId,
    'noenter.pas', cSyncWithoutEnter);
end;


procedure TRulesConcurrencyTest.SynchronizeWithLockHeldDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewSynchronizeWithLockHeld, 'synchronize.pas',
      cSynchronizeWithLockHeldId, cSyncWhileHeld, True));
end;


procedure TRulesConcurrencyTest.SynchronizeWithLockHeldSilentOnUnresolvedOperand;

begin
  // TWorker.Quiet carries an asm statement no statement-access answer covers.
  CheckIssueAt(NewSynchronizeWithLockHeld, cSynchronizeWithLockHeldId,
    'syncunanswerable.pas', cSyncUnanswerable, 55,
    ['Synchronize', 'lSection']);
end;


procedure TRulesConcurrencyTest.CriticalSectionNotInitializedPositions;

begin
  // Noncompliant: the routine local lSection is entered with no
  // InitCriticalSection anywhere before it.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'sections.pas', cSectionNeverInitialized,
    22, ['lSection']);
  // Compliant: the initialisation dominates the Enter.
  CheckSilent(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'initialized.pas',
    cSectionInitializedFirst);
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedReportsSingleBranchInit;

begin
  // One path reaches the Enter with the section still uninitialised.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'onebranchinit.pas',
    cSectionInitializedOneBranch, 24, ['lSection']);
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedSilentOnFieldAndGlobalSection;

begin
  // A field and a unit-level section can be initialised in any routine of the
  // program.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'fields.pas', cSectionFieldAndGlobal, 30,
    ['lSection']);
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedSilentOnEscapedSection;

begin
  // The address of lSection is taken, so its slot is dropped whole; the
  // untouched local at row 31 is the live control.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'escapes.pas', cSectionEscapes, 31,
    ['lOther']);
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedSilentOnHelperInit;

begin
  // Setup takes lSection by var and initialises it there.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'helperinit.pas', cSectionHelperInit, 35,
    ['lOther']);
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedSilentOnAbsoluteAlias;

begin
  // lAlias is not its own section, and the target it aliases is dropped whole;
  // the untouched local at row 31 is the live control.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'alias.pas', cSectionAbsoluteAlias, 31,
    ['lOther']);
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewCriticalSectionNotInitialized, 'sections.pas',
      cCriticalSectionNotInitializedId, cSectionNeverInitialized, True));
end;


procedure TRulesConcurrencyTest.
  CriticalSectionNotInitializedSilentOnUnresolvedOperand;

begin
  // Quiet carries an asm statement no statement-access answer covers.
  CheckIssueAt(NewCriticalSectionNotInitialized,
    cCriticalSectionNotInitializedId, 'sectionunanswerable.pas',
    cSectionUnanswerable, 28, ['lOther']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadPositions;

begin
  // Noncompliant: TWorker.Execute touches the Caption of a field directly.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclaccess.pas', cVclAccessNoncompliant, 19,
    ['Caption', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadSilentInsideSynchronize;

begin
  // The row 28 Caption access is an anonymous-procedure body; the unwrapped
  // Hint access at row 29 is the live control.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclsync.pas', cVclAccessSynchronized, 29, ['Hint', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadSilentOnNonThreadClass;

begin
  // TPlain reaches no TThread, so the row 28 hit measures the row 24 silence.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclplain.pas', cVclAccessNonThreadClass, 28,
    ['Caption', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadReportsIndirectDescendant;

begin
  // TWorker reaches TThread through TBase, both written in the module.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclindirect.pas', cVclAccessIndirectDescendant, 21,
    ['Caption', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadReportsUndeclaredAncestor;

begin
  // TWorker's ancestor is a bare reference no module type stands for.
  CheckAstIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclforeign.pas', cVclAccessUndeclaredAncestor, 17,
    ['Caption', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadReportsChainedAccessOnce;

begin
  // FLabel.Font.Color selects two curated members; only the outermost counts.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclchain.pas', cVclAccessChainedMember, 24,
    ['Color', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadReportsCuratedQualifier;

begin
  // FLabel.Font.Size ends outside the list, so the curated Font it qualifies
  // is what stands for the access.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vcltail.pas', cVclAccessChainedNonUiTail, 24,
    ['Font', 'TWorker.Execute']);
end;


procedure TRulesConcurrencyTest.VclAccessOffMainThreadSilentOnNonUiMember;

begin
  // The row 33 Caption hit measures the silence on rows 31 and 32.
  CheckIssueAt(NewVclAccessOffMainThread, cVclAccessOffMainThreadId,
    'vclother.pas', cVclAccessNonUiMember, 33,
    ['Caption', 'TWorker.Execute']);
end;


// SilentOnUnresolvedOperand is n/a for VclAccessOffMainThread: it is
// rtAst/rfAst and consults no resolved fact.
procedure TRulesConcurrencyTest.VclAccessOffMainThreadDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => zero', 0,
    CountSrc(NewVclAccessOffMainThread, 'vclbroken.pas',
      cVclAccessOffMainThreadId, cVclAccessUnparseable));
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationPositions;

begin
  // Noncompliant: gDepth is read at row 12 and assigned nowhere.
  CheckIssueAt(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvnever.pas', cThreadvarNeverAssigned, 7, ['gDepth']);
  // Compliant: gDepth assigned in the body, gNested in a nested routine.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvassigned.pas', cThreadvarAssignedInRoutine);
end;


procedure TRulesConcurrencyTest.
  ThreadvarInitializationReportsInitializationOnlyWrite;

begin
  // The only write is in the initialization section; the read is in Run.
  CheckIssueAt(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvinit.pas', cThreadvarInitializationOnly, 7, ['gDepth']);
end;


procedure TRulesConcurrencyTest.
  ThreadvarInitializationSilentOnInitializationOnlyRead;

begin
  // Both the write and the read are in the initialization section.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvinitread.pas', cThreadvarInitializationRead);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnRecordMemberWrite;

begin
  // gRec.N := 1 writes the field and reads gRec, so the candidate is withdrawn.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvrecord.pas', cThreadvarRecordMember);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnWithMemberWrite;

begin
  // The with body writes a member of gRec; the row 12 gPlain hit proves the
  // module was not withdrawn instead.
  CheckIssueAt(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvwith.pas', cThreadvarWithMemberWrite, 12, ['gPlain']);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnAsmRoutineBody;

begin
  // Quiet's body is the asm block itself, so it carries no child statement to
  // classify; cThreadvarNeverAssigned is the same shape without it.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvasmbody.pas', cThreadvarAsmBody);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnWriteByReference;

begin
  // gDepth is passed to a var parameter, which counts as a write.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvbyref.pas', cThreadvarByReference);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnIncDec;

begin
  // Inc and Dec write their argument; the row 9 gPlain hit proves the module
  // was not withdrawn instead.
  CheckIssueAt(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvincdec.pas', cThreadvarIncDec, 9, ['gPlain']);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnAddressTaken;

begin
  // @gDepth classifies as a read, so the write channel is invisible.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvaddr.pas', cThreadvarAddressTaken);
end;


procedure TRulesConcurrencyTest.
  ThreadvarInitializationSilentOnInterfaceThreadvar;

begin
  // The row 9 hit on the implementation-section gLocal measures the silence on
  // the interface-section gShared.
  CheckIssueAt(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvintf.pas', cThreadvarInInterface, 9, ['gLocal']);
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewThreadvarInitialization, 'tvnever.pas',
      cThreadvarInitializationId, cThreadvarNeverAssigned, True));
end;


procedure TRulesConcurrencyTest.ThreadvarInitializationSilentOnUnresolvedOperand;

begin
  // Quiet's asm withdraws the whole module; cThreadvarNeverAssigned is the same
  // shape without it and reports one.
  CheckSilent(NewThreadvarInitialization, cThreadvarInitializationId,
    'tvasm.pas', cThreadvarUnanswerable);
end;


procedure TRulesConcurrencyTest.ConcurrencyRulesSelfRegisterGlobally;

const
  cIds: array[0..2] of string = (cGlobalWrittenFromThreadRoutineId,
    cSynchronizeWithLockHeldId, cCriticalSectionNotInitializedId);

var
  lRule: TRuleBase;
  i: Integer;

begin
  // The production initialization registered the rules into the global
  // registry.
  for i := 0 to High(cIds) do
  begin
    lRule := RuleRegistry.FindById(cIds[i]);
    AssertTrue(cIds[i] + ' registered', lRule <> nil);
    AssertFalse(cIds[i] + ' ships disabled', lRule.Metadata.DefaultEnabled);
    AssertTrue(cIds[i] + ' has a description',
      lRule.Metadata.Description <> '');
    AssertTrue(cIds[i] + ' tier rtSem', lRule.Metadata.Tier = rtSem);
    AssertTrue(cIds[i] + ' feed rfResolver',
      lRule.Metadata.Feed = rfResolver);
  end;
  lRule := RuleRegistry.FindById(cVclAccessOffMainThreadId);
  AssertTrue(cVclAccessOffMainThreadId + ' registered', lRule <> nil);
  AssertFalse(cVclAccessOffMainThreadId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cVclAccessOffMainThreadId + ' has a description',
    lRule.Metadata.Description <> '');
  AssertTrue(cVclAccessOffMainThreadId + ' tier rtAst',
    lRule.Metadata.Tier = rtAst);
  AssertTrue(cVclAccessOffMainThreadId + ' feed rfAst',
    lRule.Metadata.Feed = rfAst);
  lRule := RuleRegistry.FindById(cThreadvarInitializationId);
  AssertTrue(cThreadvarInitializationId + ' registered', lRule <> nil);
  AssertFalse(cThreadvarInitializationId + ' ships disabled',
    lRule.Metadata.DefaultEnabled);
  AssertTrue(cThreadvarInitializationId + ' has a description',
    lRule.Metadata.Description <> '');
  AssertTrue(cThreadvarInitializationId + ' tier rtSem',
    lRule.Metadata.Tier = rtSem);
  AssertTrue(cThreadvarInitializationId + ' feed rfResolver',
    lRule.Metadata.Feed = rfResolver);
end;


initialization
  RegisterTest(TRulesConcurrencyTest);
end.
