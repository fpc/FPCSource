{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the data-flow (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesDataFlow;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Config, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Resolver,
  FpSonar.Rules.DataFlow, FpSonar.Rules.Structure, UtstFixtures;

type
  { DataFlow rule position, silence, degradation and registration tests. }
  TRulesDataFlowTest = class(TTestCase)
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
    { A fresh, separately-owned rule instance (metadata mirrors the unit's
      self-registration; empty key defaults to rule.<RuleId>.message). }
    function NewUninitializedVariable: TRuleBase;
    function NewDeadStore: TRuleBase;
    function NewUninitializedVariableStrict: TRuleBase;
    function NewSelfAssignedNeverUsed: TRuleBase;
    function NewResultOverwrittenBeforeExit: TRuleBase;
    function NewNoSelfAssignment: TRuleBase;
  published
    procedure UninitializedVariablePositions;
    procedure UninitializedVariableOptimisticImprecision;
    procedure UninitializedVariableDegradesWithoutResolver;
    procedure UninitializedVariableSilentOnUnresolvedOperand;
    procedure DeadStorePositions;
    procedure DeadStoreSilentOnVarAndOutParameter;
    procedure DeadStoreSilentOnNestedRoutineReference;
    procedure DeadStoreSilentOnInlineAssembler;
    procedure DeadStoreSilentOnNestedAssemblerRoutine;
    procedure DeadStoreSilentOnAnonymousMethodCapture;
    procedure DeadStoreSilentOnNestedAbsoluteAlias;
    procedure DeadStoreSilentOnFieldWidthRead;
    procedure DeadStoreSilentOnPointerDereferenceStore;
    procedure DeadStoreSilentOnRaiseOperand;
    procedure DeadStoreReportsForControlVariableOverwrite;
    procedure DeadStoreSilentOnNonScalarLocal;
    procedure DeadStoreSilentOnInitializedOrAbsoluteLocal;
    procedure DeadStoreSilentOnUnitLevelVariable;
    procedure DeadStoreSilentOnUnreachableStore;
    procedure DeadStoreDegradesWithoutResolver;
    procedure DeadStoreSilentOnUnresolvedOperand;
    procedure UninitializedVariableStrictPositions;
    procedure UninitializedVariableStrictSilentOnSameStatementWrite;
    procedure UninitializedVariableStrictSilentOnTryProtectedStore;
    procedure UninitializedVariableStrictDegradesWithoutResolver;
    procedure UninitializedVariableStrictSilentOnUnresolvedOperand;
    procedure SelfAssignedNeverUsedPositions;
    procedure SelfAssignedNeverUsedDegradesWithoutResolver;
    procedure SelfAssignedNeverUsedSilentOnUnresolvedOperand;
    procedure ResultOverwrittenBeforeExitPositions;
    procedure ResultOverwrittenBeforeExitSilentOnTryProtectedStore;
    procedure ResultOverwrittenBeforeExitSilentOnNestedRoutineResult;
    procedure ResultOverwrittenBeforeExitDegradesWithoutResolver;
    procedure ResultOverwrittenBeforeExitSilentOnUnresolvedOperand;
    procedure FlowVerdictsAreMutuallyExclusive;
    procedure DataFlowRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cUninitializedVariableId = 'UninitializedVariable';
  cDeadStoreId = 'DeadStore';
  cUninitializedVariableStrictId = 'UninitializedVariableStrict';
  cSelfAssignedNeverUsedId = 'SelfAssignedNeverUsed';
  cResultOverwrittenBeforeExitId = 'ResultOverwrittenBeforeExit';
  cNoSelfAssignmentId = 'NoSelfAssignment';

  // UninitializedVariablePositionsPerMode is n/a: the verdict reads the access
  // kind the resolver records on each reference, which no mode directive alters.

  cUninitNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  i: integer;',
    '  j: integer;',
    'begin',
    '  j := i; // UninitializedVariable  (i is read before any definition)',
    'end;',
    'end.');

  cUninitCompliant: array[0..35] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '{$COPERATORS ON}',
    'interface',
    'procedure Run;',
    'implementation',
    'type',
    '  TRec = record',
    '    Value: integer;',
    '  end;',
    'procedure SetIt(var aValue: integer);',
    'begin',
    '  aValue := 1;',
    'end;',
    'procedure Run;',
    'var',
    '  a: integer;',
    '  b: integer;',
    '  c: integer;',
    '  d: integer;',
    '  t: integer;',
    '  r: TRec;',
    'begin',
    '  a := 1;      // plain assignment defines a',
    '  t := a;',
    '  b += 1;      // compound assignment defines b',
    '  t := b;',
    '  SetIt(c);    // a var argument defines c',
    '  t := c;',
    '  asm',
    '    mov eax, d',
    '  end;',
    '  t := d;      // the asm mention above defines d',
    '  t := r.Value; // a structured local is never tracked',
    'end;',
    'end.');

  cUninitOptimistic: array[0..15] of string = (
    'unit optimistic;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run(aFirst: boolean; aSecond: boolean);',
    'implementation',
    'procedure Run(aFirst: boolean; aSecond: boolean);',
    'var',
    '  i: integer;',
    '  j: integer;',
    'begin',
    '  if aFirst then',
    '    i := 1;',
    '  if aSecond then',
    '    j := i;   // reached with i undefined, and deliberately NOT reported',
    'end;',
    'end.');

  cUninitTemplateType: array[0..15] of string = (
    'unit gtparam;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBox<T> = class',
    '    procedure Run;',
    '  end;',
    'implementation',
    'procedure TBox.Run;',
    'var',
    '  v: T;',
    '  w: T;',
    'begin',
    '  w := v;',
    'end;',
    'end.');

  cUninitTemplateMixed: array[0..18] of string = (
    'unit gtmixed;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBox<T> = class',
    '    procedure Run;',
    '  end;',
    'implementation',
    'procedure TBox.Run;',
    'var',
    '  v: T;',
    '  w: T;',
    '  i: integer;',
    '  j: integer;',
    'begin',
    '  w := v;   // the template-typed local is not a tracked type',
    '  j := i;   // the identical shape on a concrete local IS reported',
    'end;',
    'end.');

  cUninitWithShadow: array[0..19] of string = (
    'unit withshadow;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'type',
    '  TRec = record',
    '    i: integer;',
    '  end;',
    'procedure Run;',
    'var',
    '  i: integer;',
    '  j: integer;',
    '  r: TRec;',
    'begin',
    '  r.i := 0;',
    '  with r do',
    '    j := i;   // reads the field, not the like-named local',
    'end;',
    'end.');

  cDeadStoreNoncompliant: array[0..28] of string = (
    'unit dsnoncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Plain;',
    'procedure Branched(aFlag: boolean);',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Plain;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore: overwritten on the next line',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'procedure Branched(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore: overwritten on every branch',
    '  if aFlag then',
    '    lI := 2',
    '  else',
    '    lI := 3;',
    '  Use(lI);',
    'end;',
    'end.');

  cDeadStoreCompliant: array[0..45] of string = (
    'unit dscompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure ReadFirst;',
    'procedure OnePath(aFlag: boolean);',
    'procedure ReadOnePath(aFlag: boolean);',
    'procedure LoopStore(aFlag: boolean);',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure ReadFirst;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  Use(lI);   // the value is read, so the store is not dead',
    '  lI := 2;',
    'end;',
    'procedure OnePath(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  if aFlag then',
    '    lI := 2;   // one path reaches the exit still pending',
    '  Use(lI);',
    'end;',
    'procedure ReadOnePath(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  if aFlag then',
    '    Use(lI);   // a read on any path is a read',
    '  lI := 2;',
    'end;',
    'procedure LoopStore(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  while aFlag do',
    '    lI := 2;   // the pending exit path silences both stores',
    'end;',
    'end.');

  cDeadStoreParams: array[0..29] of string = (
    'unit dsparams;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure VarArg;',
    'procedure OutArg;',
    'implementation',
    'procedure Fill(var aI: integer);',
    'begin',
    '  aI := 0;',
    'end;',
    'procedure GetIt(out aI: integer);',
    'begin',
    '  aI := 0;',
    'end;',
    'procedure VarArg;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // a var argument counts as a read',
    '  Fill(lI);',
    '  lI := 2;',
    'end;',
    'procedure OutArg;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // an out argument is a write AND a read',
    '  GetIt(lI);',
    'end;',
    'end.');

  cFlowNested: array[0..20] of string = (
    'unit dsnested;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lI: integer;',
    '  procedure Bump;',
    '  begin',
    '    Use(lI);',
    '  end;',
    'begin',
    '  lI := 1;',
    '  Bump;',
    '  lI := 2;',
    'end;',
    'end.');

  cFlowAsm: array[0..28] of string = (
    'unit dsasm;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Live;',
    'procedure WithAsm;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Live;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, in the routine beside the asm one',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'procedure WithAsm;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  asm',
    '    nop',
    '  end;',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'end.');

  cFlowNestedAsm: array[0..30] of string = (
    'unit dsnestedasm;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Live;',
    'procedure Outer;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Live;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, beside the routine nesting the asm-bodied one',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'procedure Outer;',
    'var',
    '  lI: integer;',
    '  procedure Inner; assembler;',
    '  asm',
    '    nop',
    '  end;',
    'begin',
    '  lI := 1;   // the assembler above may name it',
    '  Inner;',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'end.');

  cFlowAnonymous: array[0..22] of string = (
    'unit dsanon;',
    '{$mode objfpc}{$H+}',
    '{$modeswitch functionreferences}',
    '{$modeswitch anonymousfunctions}',
    'interface',
    'procedure Run;',
    'implementation',
    'type',
    '  TRunner = reference to procedure;',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lI: integer;',
    '  lProc: TRunner;',
    'begin',
    '  lI := 1;   // the captured read below is invisible to the walker',
    '  lProc := procedure begin Use(lI); end;',
    '  lI := 2;',
    '  lProc();',
    'end;',
    'end.');

  cFlowNestedAbsolute: array[0..22] of string = (
    'unit dsnestabs;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lI: integer;',
    '  procedure Peek;',
    '  var',
    '    lA: integer absolute lI;',
    '  begin',
    '    Use(lA);',
    '  end;',
    'begin',
    '  lI := 1;   // Peek reads it through the alias below',
    '  Peek;',
    '  lI := 2;',
    'end;',
    'end.');

  cFlowFieldWidth: array[0..17] of string = (
    'unit dsformat;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lV: integer;',
    '  lW: integer;',
    '  lS: string;',
    'begin',
    '  lV := 3;',
    '  lW := 5;   // read only as the field width below',
    '  Str(lV:lW, lS);',
    '  lW := 8;',
    '  Str(lV:lW, lS);',
    'end;',
    'end.');

  cFlowPointerDeref: array[0..16] of string = (
    'unit dsderef;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'type',
    '  PInt = ^integer;',
    'procedure Run;',
    'var',
    '  lI: integer;',
    '  lP: PInt;',
    'begin',
    '  lI := 0;',
    '  lP := @lI;   // the store below reads the pointer',
    '  lP^ := 5;',
    'end;',
    'end.');

  cFlowTryResult: array[0..17] of string = (
    'unit dstryresult;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function Parse(aI: integer): integer;',
    'implementation',
    'function Convert(aI: integer): integer;',
    'begin',
    '  Result := aI;',
    'end;',
    'function Parse(aI: integer): integer;',
    'begin',
    '  Result := -1;   // live on the exception path',
    '  try',
    '    Result := Convert(aI);',
    '  except',
    '  end;',
    'end;',
    'end.');

  cFlowTryFinally: array[0..23] of string = (
    'unit dstryfinally;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Cleanup;',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lI: integer;',
    'begin',
    '  try',
    '    Cleanup;',
    '    lI := 1;',
    '  finally',
    '    Cleanup;',
    '  end;',
    '  Use(lI);   // every normal path stored lI first',
    'end;',
    'end.');

  cFlowNestedResult: array[0..22] of string = (
    'unit dsnestedresult;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function Outer: integer;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'function Outer: integer;',
    '  procedure Log;',
    '  begin',
    '    Use(Result);   // the enclosing function''s result, read here',
    '  end;',
    '  procedure SetIt;',
    '  begin',
    '    Result := 1;   // Log reads it before the next store',
    '    Log;',
    '    Result := 2;',
    '  end;',
    'begin',
    '  SetIt;',
    'end;',
    'end.');

  cFlowRaise: array[0..23] of string = (
    'unit dsraise;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses',
    '  SysUtils;',
    'type',
    '  { A failure. }',
    '  EFoo = class(Exception);',
    'procedure Run(aFlag: boolean);',
    'implementation',
    'function Describe(aI: integer): string;',
    'begin',
    '  Result := '''';',
    'end;',
    'procedure Run(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  if aFlag then',
    '    raise EFoo.Create(Describe(lI));   // the operand reads lI',
    '  lI := 2;',
    'end;',
    'end.');

  cFlowForLoop: array[0..16] of string = (
    'unit dsfor;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 0;   // DeadStore: the loop header overwrites it unread',
    '  for lI := 1 to 3 do',
    '    Use(lI);   // the loop header is a definition',
    'end;',
    'end.');

  cFlowNonScalar: array[0..28] of string = (
    'unit dsstring;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'procedure Live;',
    'implementation',
    'procedure TakeS(const aValue: string);',
    'begin',
    'end;',
    'procedure TakeI(aI: integer);',
    'begin',
    'end;',
    'procedure Run;',
    'var',
    '  lS: string;',
    'begin',
    '  lS := ''a'';   // outside the tracked population',
    '  lS := ''b'';',
    '  TakeS(lS);',
    'end;',
    'procedure Live;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, proving the analysis ran',
    '  lI := 2;',
    '  TakeI(lI);',
    'end;',
    'end.');

  cFlowInitAbsolute: array[0..36] of string = (
    'unit dsinit;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Initialized;',
    'procedure Aliased;',
    'procedure Live;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Initialized;',
    'var',
    '  lI: integer = 1;',
    'begin',
    '  lI := 2;   // an initialized local is not tracked',
    '  lI := 3;',
    '  Use(lI);',
    'end;',
    'procedure Aliased;',
    'var',
    '  lBase: integer;',
    '  lI: integer absolute lBase;',
    'begin',
    '  lBase := 1;   // the alias below reads it, so the store is not dead',
    '  Use(lI);',
    '  lBase := 2;',
    '  Use(lBase);',
    'end;',
    'procedure Live;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, proving the analysis ran',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'end.');

  cFlowUnitVar: array[0..25] of string = (
    'unit dsglobal;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'procedure Live;',
    'implementation',
    'var',
    '  GI: integer;',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Run;',
    'begin',
    '  GI := 1;   // not a routine local',
    '  GI := 2;',
    '  Use(GI);',
    'end;',
    'procedure Live;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, proving the analysis ran',
    '  lI := 2;',
    '  Use(lI);',
    'end;',
    'end.');

  cFlowUnreachable: array[0..41] of string = (
    'unit dsunreachable;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Dead;',
    'procedure DeadSelf;',
    'procedure DeadJoin(aFlag: boolean);',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Dead;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, on the live half of the same routine',
    '  lI := 2;',
    '  Use(lI);',
    '  Exit;',
    '  lI := 3;   // no control path reaches these two stores',
    '  lI := 4;',
    '  Use(lI);',
    'end;',
    'procedure DeadSelf;',
    'var',
    '  lI: integer;',
    'begin',
    '  Exit;',
    '  lI := 1;',
    '  lI := lI + 1;',
    'end;',
    'procedure DeadJoin(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  if aFlag then',
    '  begin',
    '    Exit;',
    '    lI := 1;   // the dead tail rejoins the live path in the graph',
    '  end;',
    '  Use(lI);',
    'end;',
    'end.');

  cStrictNoncompliant: array[0..16] of string = (
    'unit uvsnoncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run(aFlag: boolean);',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Run(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  if aFlag then',
    '    lI := 1;',
    '  Use(lI);   // Strict: lI is unassigned on the else path',
    'end;',
    'end.');

  cStrictCompliant: array[0..25] of string = (
    'unit uvscompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Never;',
    'procedure Both(aFlag: boolean);',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Never;',
    'var',
    '  lI: integer;',
    'begin',
    '  Use(lI);   // no path assigns it: UninitializedVariable owns the row',
    'end;',
    'procedure Both(aFlag: boolean);',
    'var',
    '  lI: integer;',
    'begin',
    '  if aFlag then',
    '    lI := 1',
    '  else',
    '    lI := 2;',
    '  Use(lI);',
    'end;',
    'end.');

  cStrictGuardedOut: array[0..22] of string = (
    'unit uvsguard;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run(aCount: integer);',
    'implementation',
    'function TryGet(out aKind: integer): boolean;',
    'begin',
    '  aKind := 0;',
    '  Result := True;',
    'end;',
    'procedure Take(aI: integer);',
    'begin',
    'end;',
    'procedure Run(aCount: integer);',
    'var',
    '  lI: integer;',
    '  lKind: integer;',
    'begin',
    '  for lI := 1 to aCount do',
    '    if TryGet(lKind) and (lKind = 0) then',
    '      Take(lKind);',
    'end;',
    'end.');

  cSelfNoncompliant: array[0..12] of string = (
    'unit sanunoncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;',
    '  lI := lI + 1;   // SelfAssignedNeverUsed: never read afterwards',
    'end;',
    'end.');

  cSelfCompliant: array[0..22] of string = (
    'unit sanucompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure Bare;',
    'procedure ThenUsed;',
    'implementation',
    'procedure Use(aI: integer);',
    'begin',
    'end;',
    'procedure Bare;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := lI;   // NoSelfAssignment owns that row',
    'end;',
    'procedure ThenUsed;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := lI + 1;',
    '  Use(lI);',
    'end;',
    'end.');

  cResultNoncompliant: array[0..10] of string = (
    'unit robenoncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function F: integer;',
    'implementation',
    'function F: integer;',
    'begin',
    '  Result := 0;   // ResultOverwrittenBeforeExit',
    '  Result := 5;',
    'end;',
    'end.');

  cResultCompliant: array[0..17] of string = (
    'unit robecompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function ReadBetween: integer;',
    'function OnePath(aFlag: boolean): integer;',
    'implementation',
    'function ReadBetween: integer;',
    'begin',
    '  Result := 0;',
    '  Result := Result + 1;   // the first store is read',
    'end;',
    'function OnePath(aFlag: boolean): integer;',
    'begin',
    '  Result := 0;',
    '  if aFlag then',
    '    Result := 5;   // one path returns the first value',
    'end;',
    'end.');

  cFlowUnresolved: array[0..48] of string = (
    'unit gtunresolved;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  { A generic box. }',
    '  generic TBox<T> = class',
    '    function Same(const aItem: T): T;',
    '    procedure Run(aFlag: boolean);',
    '    function Calc: T;',
    '  end;',
    'procedure Live;',
    'implementation',
    'procedure Take(aI: integer);',
    'begin',
    'end;',
    'function TBox.Same(const aItem: T): T;',
    'begin',
    '  Result := aItem;',
    'end;',
    'procedure TBox.Run(aFlag: boolean);',
    'var',
    '  lA: T;',
    '  lB: T;',
    '  lC: T;',
    'begin',
    '  lA := lB;',
    '  lA := lB;   // a dead store, were T a type the verdict could classify',
    '  lC := lA;',
    '  lA := Same(lA);   // a self-derived store nothing reads',
    '  if aFlag then',
    '    lB := lC;',
    '  lC := lB;   // a read of a maybe-unassigned local',
    'end;',
    'function TBox.Calc: T;',
    'var',
    '  lA: T;',
    'begin',
    '  Result := lA;   // an overwritten result',
    '  Result := lA;',
    'end;',
    'procedure Live;',
    'var',
    '  lI: integer;',
    'begin',
    '  lI := 1;   // DeadStore, proving the analysis ran',
    '  lI := 2;',
    '  Take(lI);',
    'end;',
    'end.');

function TRulesDataFlowTest.EnabledConfig(
  const aRuleId: string): TFpSonarConfig;

begin
  Result := TFpSonarConfig.Default;
  SetLength(Result.Rules, 0);
  SetLength(Result.Rules, 1);
  Result.Rules[0].RuleId := aRuleId;
  Result.Rules[0].HasEnabled := True;
  Result.Rules[0].Enabled := True;
end;


procedure TRulesDataFlowTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector; aWithhold: boolean = False);

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


procedure TRulesDataFlowTest.RunRuleSrc(aRule: TRuleBase; const aName: string;
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


function TRulesDataFlowTest.CountById(const aCollector: TFpSonarIssueCollector;
  const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesDataFlowTest.FirstById(const aCollector: TFpSonarIssueCollector;
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


function TRulesDataFlowTest.CountSrc(aRule: TRuleBase; const aName, aId: string;
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


procedure TRulesDataFlowTest.AssertResolvesClean(const aName: string;
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


function TRulesDataFlowTest.NewUninitializedVariable: TRuleBase;

begin
  Result := TRuleUninitializedVariable.Create(TRuleMetadata.Make(
    cUninitializedVariableId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    False, ''));
end;


function TRulesDataFlowTest.NewDeadStore: TRuleBase;

begin
  Result := TRuleDeadStore.Create(TRuleMetadata.Make(
    cDeadStoreId, rtSem, rfResolver, sevMinor, itCodeSmell, cfMedium,
    False, ''));
end;


function TRulesDataFlowTest.NewUninitializedVariableStrict: TRuleBase;

begin
  Result := TRuleUninitializedVariableStrict.Create(TRuleMetadata.Make(
    cUninitializedVariableStrictId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, ''));
end;


function TRulesDataFlowTest.NewSelfAssignedNeverUsed: TRuleBase;

begin
  Result := TRuleSelfAssignedNeverUsed.Create(TRuleMetadata.Make(
    cSelfAssignedNeverUsedId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfMedium, False, ''));
end;


function TRulesDataFlowTest.NewResultOverwrittenBeforeExit: TRuleBase;

begin
  Result := TRuleResultOverwrittenBeforeExit.Create(TRuleMetadata.Make(
    cResultOverwrittenBeforeExitId, rtSem, rfResolver, sevMajor, itBug,
    cfMedium, False, ''));
end;


function TRulesDataFlowTest.NewNoSelfAssignment: TRuleBase;

begin
  Result := TRuleNoSelfAssignment.Create(TRuleMetadata.Make(
    cNoSelfAssignmentId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


procedure TRulesDataFlowTest.UninitializedVariablePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: 'i' is read on row 11 with no earlier definition.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewUninitializedVariable, 'noncompliant.pas',
      cUninitNoncompliant, lc);
    AssertEquals('one issue', 1, CountById(lc, cUninitializedVariableId));
    k := FirstById(lc, cUninitializedVariableId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 11, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.' + cUninitializedVariableId + '.message', lc.Issues[k].MessageKey);
    AssertEquals('arg count', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('the variable name', 'i', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  // Compliant: every indirect way the absence polarity is satisfied — a plain
  // assignment, a compound assignment, a var argument and an asm mention — plus
  // a structured local, which the analysis never tracks at all.
  AssertResolvesClean('compliant.pas', cUninitCompliant);
  AssertEquals('compliant => zero', 0,
    CountSrc(NewUninitializedVariable, 'compliant.pas',
      cUninitializedVariableId, cUninitCompliant));
end;


procedure TRulesDataFlowTest.UninitializedVariableOptimisticImprecision;

begin
  // The documented imprecision, pinned: 'i' is defined only inside the first if
  // branch and read under a later independent if, and a textually earlier
  // definition suppresses every later use.
  AssertResolvesClean('optimistic.pas', cUninitOptimistic);
  AssertEquals('an earlier one-branch definition silences the later read', 0,
    CountSrc(NewUninitializedVariable, 'optimistic.pas',
      cUninitializedVariableId, cUninitOptimistic));
end;


procedure TRulesDataFlowTest.UninitializedVariableDegradesWithoutResolver;

begin
  // The resolver goes dark on the noncompliant fixture.
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewUninitializedVariable, 'noncompliant.pas',
      cUninitializedVariableId, cUninitNoncompliant, True));
end;


procedure TRulesDataFlowTest.UninitializedVariableSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { First: a local typed by a generic template parameter. The template body
    resolves in full, but the type is not one the verdict can classify and the
    local is never tracked. }
  AssertResolvesClean('gtparam.pas', cUninitTemplateType);
  AssertEquals('template-typed local => zero', 0,
    CountSrc(NewUninitializedVariable, 'gtparam.pas',
      cUninitializedVariableId, cUninitTemplateType));

  { The same routine carrying both shapes: the concrete local beside it is
    still reported. }
  AssertResolvesClean('gtmixed.pas', cUninitTemplateMixed);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewUninitializedVariable, 'gtmixed.pas', cUninitTemplateMixed, lc);
    AssertEquals('only the concrete local is reported', 1,
      CountById(lc, cUninitializedVariableId));
    k := FirstById(lc, cUninitializedVariableId);
    AssertEquals('at the concrete read', 17, lc.Issues[k].StartLine);
    AssertEquals('the concrete variable name', 'i', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  { Second: the use site resolves to a different declaration. A with-scope
    binds the read to a like-named record field and the result set is empty. }
  AssertResolvesClean('withshadow.pas', cUninitWithShadow);
  AssertEquals('use site bound elsewhere => zero', 0,
    CountSrc(NewUninitializedVariable, 'withshadow.pas',
      cUninitializedVariableId, cUninitWithShadow));
end;


procedure TRulesDataFlowTest.DeadStorePositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { Noncompliant: the plain overwrite on row 14 and the shape pure syntax
    cannot see -- a store overwritten on every branch of an if -- on row 22. }
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsnoncompliant.pas', cDeadStoreNoncompliant, lc);
    AssertEquals('two issues', 2, CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('start line', 14, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 14, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.' + cDeadStoreId + '.message', lc.Issues[k].MessageKey);
    AssertEquals('arg count', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('the variable name', 'lI', lc.Issues[k].MessageArgs[0]);
    AssertEquals('the second issue is the branched store', 22,
      lc.Issues[k + 1].StartLine);
  finally
    lc.Free;
  end;

  { Compliant: a read before the overwrite, an overwrite on one path only, a
    read on one path only, and a loop re-store whose exit path stays pending. }
  AssertResolvesClean('dscompliant.pas', cDeadStoreCompliant);
  AssertEquals('compliant => zero', 0,
    CountSrc(NewDeadStore, 'dscompliant.pas', cDeadStoreId,
      cDeadStoreCompliant));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnVarAndOutParameter;

begin
  { Both parameter forms count as a read: FPC does not initialise a non-managed
    out parameter, so pre-setting a local before a caller-supplied callback
    writes it back is deliberate, not a dead store. }
  AssertResolvesClean('dsparams.pas', cDeadStoreParams);
  AssertEquals('a var or out argument => zero', 0,
    CountSrc(NewDeadStore, 'dsparams.pas', cDeadStoreId, cDeadStoreParams));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnNestedAssemblerRoutine;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { A nested routine whose whole body is an asm block yields no sub-statements
    at all, so the outer locals it names would otherwise go unseen; the outer
    routine is dropped and the routine beside it still reports. }
  AssertResolvesClean('dsnestedasm.pas', cFlowNestedAsm);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsnestedasm.pas', cFlowNestedAsm, lc);
    AssertEquals('only the routine nesting no assembler', 1,
      CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('in the live sibling', 14, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnAnonymousMethodCapture;

begin
  { An anonymous method is an expression operand rather than a declaration-list
    entry, so the nested-routine scan cannot reach the read it captures. }
  AssertResolvesClean('dsanon.pas', cFlowAnonymous);
  AssertEquals('a captured local => zero', 0,
    CountSrc(NewDeadStore, 'dsanon.pas', cDeadStoreId, cFlowAnonymous));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnNestedAbsoluteAlias;

begin
  // The alias is declared in the nested routine.
  AssertResolvesClean('dsnestabs.pas', cFlowNestedAbsolute);
  AssertEquals('a nested absolute alias => zero', 0,
    CountSrc(NewDeadStore, 'dsnestabs.pas', cDeadStoreId, cFlowNestedAbsolute));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnFieldWidthRead;

begin
  // A field width is a resolved read held beside the value rather than under
  // it.
  AssertResolvesClean('dsformat.pas', cFlowFieldWidth);
  AssertEquals('a field-width read => zero', 0,
    CountSrc(NewDeadStore, 'dsformat.pas', cDeadStoreId, cFlowFieldWidth));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnPointerDereferenceStore;

begin
  // The resolver files the pointer of a dereferenced store as written, but
  // the store lands in the memory it addresses and reads the pointer itself.
  AssertResolvesClean('dsderef.pas', cFlowPointerDeref);
  AssertEquals('a store through a dereference => zero', 0,
    CountSrc(NewDeadStore, 'dsderef.pas', cDeadStoreId, cFlowPointerDeref));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnNestedRoutineReference;

begin
  // The nested routine's body is not in this graph.
  AssertResolvesClean('dsnested.pas', cFlowNested);
  AssertEquals('a nested-routine reference => zero', 0,
    CountSrc(NewDeadStore, 'dsnested.pas', cDeadStoreId, cFlowNested));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnInlineAssembler;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // The asm block cannot be classified, so its whole routine is skipped while
  // the identical shape in the routine beside it still reports.
  AssertResolvesClean('dsasm.pas', cFlowAsm);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsasm.pas', cFlowAsm, lc);
    AssertEquals('only the asm-free routine', 1, CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('in the live sibling', 14, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnRaiseOperand;

begin
  // The raise leaves the routine without reaching the exit node.
  AssertResolvesClean('dsraise.pas', cFlowRaise);
  AssertEquals('a read in a raise operand => zero', 0,
    CountSrc(NewDeadStore, 'dsraise.pas', cDeadStoreId, cFlowRaise));
end;


procedure TRulesDataFlowTest.DeadStoreReportsForControlVariableOverwrite;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { The loop header is a write of the control variable, which is observable
    only through a store it overwrites: without that classification the store
    before the loop stays pending and nothing is reported. }
  AssertResolvesClean('dsfor.pas', cFlowForLoop);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsfor.pas', cFlowForLoop, lc);
    AssertEquals('the header overwrites the store before it', 1,
      CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('at the store before the loop', 13, lc.Issues[k].StartLine);
    AssertEquals('the variable name', 'lI', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;
  AssertEquals('the header definition leaves no unassigned path', 0,
    CountSrc(NewUninitializedVariableStrict, 'dsfor.pas',
      cUninitializedVariableStrictId, cFlowForLoop));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnNonScalarLocal;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A partial write to a compound type is undecidable from the graph.
  AssertResolvesClean('dsstring.pas', cFlowNonScalar);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsstring.pas', cFlowNonScalar, lc);
    AssertEquals('only the scalar local', 1, CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('in the live sibling', 25, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnInitializedOrAbsoluteLocal;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // An initializer already defines the local and an absolute clause aliases
  // another one; neither is tracked, while a plain local beside them is.
  AssertResolvesClean('dsinit.pas', cFlowInitAbsolute);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsinit.pas', cFlowInitAbsolute, lc);
    AssertEquals('only the plain local', 1, CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('in the live sibling', 33, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnUnitLevelVariable;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // A unit-level variable outlives the routine.
  AssertResolvesClean('dsglobal.pas', cFlowUnitVar);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsglobal.pas', cFlowUnitVar, lc);
    AssertEquals('only the routine local', 1, CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('in the live sibling', 22, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnUnreachableStore;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { A store no control path reaches gets no verdict: UnreachableCode owns that
    row. All three store-bearing verdicts are asserted, each resting on a
    different guard. }
  AssertResolvesClean('dsunreachable.pas', cFlowUnreachable);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'dsunreachable.pas', cFlowUnreachable, lc);
    AssertEquals('only the reachable store', 1, CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('on the live half of the routine', 15,
      lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
  AssertEquals('an unreachable self-derived store => zero', 0,
    CountSrc(NewSelfAssignedNeverUsed, 'dsunreachable.pas',
      cSelfAssignedNeverUsedId, cFlowUnreachable));
  AssertEquals('an unreachable definition assigns no live path', 0,
    CountSrc(NewUninitializedVariableStrict, 'dsunreachable.pas',
      cUninitializedVariableStrictId, cFlowUnreachable));
end;


procedure TRulesDataFlowTest.DeadStoreDegradesWithoutResolver;

begin
  // The resolver goes dark, so the query is False and the rule reports nothing.
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewDeadStore, 'dsnoncompliant.pas', cDeadStoreId,
      cDeadStoreNoncompliant, True));
end;


procedure TRulesDataFlowTest.DeadStoreSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { Degradation mode 2: the generic body resolves in full, but its declarations
    are typed by the template parameter and the verdict cannot classify that
    type. The concrete routine beside it still reports. }
  AssertResolvesClean('gtunresolved.pas', cFlowUnresolved);
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'gtunresolved.pas', cFlowUnresolved, lc);
    AssertEquals('only the concretely typed routine', 1,
      CountById(lc, cDeadStoreId));
    k := FirstById(lc, cDeadStoreId);
    AssertEquals('in the live sibling', 45, lc.Issues[k].StartLine);
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.UninitializedVariableStrictPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { Noncompliant: lI is defined in one if arm only and read after the join, so
    both an assigned and an unassigned path reach row 15. }
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewUninitializedVariableStrict, 'uvsnoncompliant.pas',
      cStrictNoncompliant, lc);
    AssertEquals('one issue', 1,
      CountById(lc, cUninitializedVariableStrictId));
    k := FirstById(lc, cUninitializedVariableStrictId);
    AssertEquals('start line', 15, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 15, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.' + cUninitializedVariableStrictId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('arg count', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('the variable name', 'lI', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  { The shipped optimistic rule is silent on that same fixture -- the shape it
    cannot see is exactly what this id covers. }
  AssertEquals('UninitializedVariable stays silent there', 0,
    CountSrc(NewUninitializedVariable, 'uvsnoncompliant.pas',
      cUninitializedVariableId, cStrictNoncompliant));

  { Compliant: a read no path assigns before (UninitializedVariable's row) and
    a read every branch assigns before. The sibling that owns the row is
    asserted live. }
  AssertResolvesClean('uvscompliant.pas', cStrictCompliant);
  AssertEquals('compliant => zero', 0,
    CountSrc(NewUninitializedVariableStrict, 'uvscompliant.pas',
      cUninitializedVariableStrictId, cStrictCompliant));
  AssertEquals('UninitializedVariable owns the definitely-unassigned read', 1,
    CountSrc(NewUninitializedVariable, 'uvscompliant.pas',
      cUninitializedVariableId, cStrictCompliant));
end;


procedure TRulesDataFlowTest.UninitializedVariableStrictSilentOnSameStatementWrite;

begin
  { The out argument writes lKind and the short-circuited right operand reads
    it, both in one condition; the loop back-edge then carries an unassigned
    path into the same node. The lattice has no order inside a statement. }
  AssertResolvesClean('uvsguard.pas', cStrictGuardedOut);
  AssertEquals('a read the same statement also writes => zero', 0,
    CountSrc(NewUninitializedVariableStrict, 'uvsguard.pas',
      cUninitializedVariableStrictId, cStrictGuardedOut));
end;


procedure TRulesDataFlowTest.UninitializedVariableStrictSilentOnTryProtectedStore;

begin
  { pascfg continues an exception through the finally into the code after the
    try, so a read there sees both an assigned and an unassigned path; every
    declaration written inside a protected region is dropped instead. }
  AssertResolvesClean('dstryfinally.pas', cFlowTryFinally);
  AssertEquals('a store inside a protected region => zero', 0,
    CountSrc(NewUninitializedVariableStrict, 'dstryfinally.pas',
      cUninitializedVariableStrictId, cFlowTryFinally));
end;


procedure TRulesDataFlowTest.UninitializedVariableStrictDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewUninitializedVariableStrict, 'uvsnoncompliant.pas',
      cUninitializedVariableStrictId, cStrictNoncompliant, True));
end;


procedure TRulesDataFlowTest.UninitializedVariableStrictSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  // The generic body also carries a one-branch definition read after the join;
  // the skipped routine yields nothing while DeadStore fires on the sibling.
  AssertResolvesClean('gtunresolved.pas', cFlowUnresolved);
  AssertEquals('unresolved operand => zero', 0,
    CountSrc(NewUninitializedVariableStrict, 'gtunresolved.pas',
      cUninitializedVariableStrictId, cFlowUnresolved));
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'gtunresolved.pas', cFlowUnresolved, lc);
    AssertEquals('the sibling rule still fires', 1,
      CountById(lc, cDeadStoreId));
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.SelfAssignedNeverUsedPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  { Noncompliant: row 11 computes a new value out of the old one and no path
    ever reads it, so the whole computation is wasted. }
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewSelfAssignedNeverUsed, 'sanunoncompliant.pas',
      cSelfNoncompliant, lc);
    AssertEquals('one issue', 1, CountById(lc, cSelfAssignedNeverUsedId));
    k := FirstById(lc, cSelfAssignedNeverUsedId);
    AssertEquals('start line', 11, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 11, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.' + cSelfAssignedNeverUsedId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('arg count', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('the variable name', 'lI', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  { Compliant: a bare self-assignment, which NoSelfAssignment owns, and a
    self-derived store the routine goes on to read. The owning rule is asserted
    live. }
  AssertResolvesClean('sanucompliant.pas', cSelfCompliant);
  AssertEquals('compliant => zero', 0,
    CountSrc(NewSelfAssignedNeverUsed, 'sanucompliant.pas',
      cSelfAssignedNeverUsedId, cSelfCompliant));
  AssertEquals('NoSelfAssignment owns the bare self-assignment', 1,
    CountSrc(NewNoSelfAssignment, 'sanucompliant.pas', cNoSelfAssignmentId,
      cSelfCompliant));
end;


procedure TRulesDataFlowTest.SelfAssignedNeverUsedDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewSelfAssignedNeverUsed, 'sanunoncompliant.pas',
      cSelfAssignedNeverUsedId, cSelfNoncompliant, True));
end;


procedure TRulesDataFlowTest.SelfAssignedNeverUsedSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  // The generic body also carries an unread self-derived store.
  AssertResolvesClean('gtunresolved.pas', cFlowUnresolved);
  AssertEquals('unresolved operand => zero', 0,
    CountSrc(NewSelfAssignedNeverUsed, 'gtunresolved.pas',
      cSelfAssignedNeverUsedId, cFlowUnresolved));
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'gtunresolved.pas', cFlowUnresolved, lc);
    AssertEquals('the sibling rule still fires', 1,
      CountById(lc, cDeadStoreId));
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.ResultOverwrittenBeforeExitPositions;

var
  lc: TFpSonarIssueCollector;
  k: Integer;

begin
  // Noncompliant: the computed result on row 8 never reaches the caller.
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewResultOverwrittenBeforeExit, 'robenoncompliant.pas',
      cResultNoncompliant, lc);
    AssertEquals('one issue', 1,
      CountById(lc, cResultOverwrittenBeforeExitId));
    k := FirstById(lc, cResultOverwrittenBeforeExitId);
    AssertEquals('start line', 8, lc.Issues[k].StartLine);
    AssertEquals('start col', 1, lc.Issues[k].StartCol);
    AssertEquals('end line', 8, lc.Issues[k].EndLine);
    AssertEquals('end col', 1, lc.Issues[k].EndCol);
    AssertEquals('key is the dotted rule key',
      'rule.' + cResultOverwrittenBeforeExitId + '.message',
      lc.Issues[k].MessageKey);
    AssertEquals('arg count', 1, Length(lc.Issues[k].MessageArgs));
    AssertEquals('the result name', 'Result', lc.Issues[k].MessageArgs[0]);
  finally
    lc.Free;
  end;

  // Compliant: the first store read by the second, and a store one path leaves
  // standing.
  AssertResolvesClean('robecompliant.pas', cResultCompliant);
  AssertEquals('compliant => zero', 0,
    CountSrc(NewResultOverwrittenBeforeExit, 'robecompliant.pas',
      cResultOverwrittenBeforeExitId, cResultCompliant));
end;


procedure TRulesDataFlowTest.ResultOverwrittenBeforeExitSilentOnTryProtectedStore;

begin
  { The engine propagates a node's post-transfer state along its exceptional
    successors, so the store inside the try looks complete on the handler path
    and the store before it looks overwritten; it is live on that path. }
  AssertResolvesClean('dstryresult.pas', cFlowTryResult);
  AssertEquals('a store inside a protected region => zero', 0,
    CountSrc(NewResultOverwrittenBeforeExit, 'dstryresult.pas',
      cResultOverwrittenBeforeExitId, cFlowTryResult));
  AssertEquals('and no dead store either', 0,
    CountSrc(NewDeadStore, 'dstryresult.pas', cDeadStoreId, cFlowTryResult));
end;


procedure TRulesDataFlowTest.ResultOverwrittenBeforeExitSilentOnNestedRoutineResult;

begin
  { A nested routine's own graph cannot see the enclosing function's reads of
    Result, so a store it overwrites there is judged against the wrong exit;
    the result slot belongs to the routine under analysis alone. }
  AssertResolvesClean('dsnestedresult.pas', cFlowNestedResult);
  AssertEquals('an enclosing routine''s result => zero', 0,
    CountSrc(NewResultOverwrittenBeforeExit, 'dsnestedresult.pas',
      cResultOverwrittenBeforeExitId, cFlowNestedResult));
end;


procedure TRulesDataFlowTest.ResultOverwrittenBeforeExitDegradesWithoutResolver;

begin
  AssertEquals('no resolver => zero', 0,
    CountSrc(NewResultOverwrittenBeforeExit, 'robenoncompliant.pas',
      cResultOverwrittenBeforeExitId, cResultNoncompliant, True));
end;


procedure TRulesDataFlowTest.ResultOverwrittenBeforeExitSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;

begin
  // The generic class's function carries the overwritten-result shape.
  AssertResolvesClean('gtunresolved.pas', cFlowUnresolved);
  AssertEquals('unresolved operand => zero', 0,
    CountSrc(NewResultOverwrittenBeforeExit, 'gtunresolved.pas',
      cResultOverwrittenBeforeExitId, cFlowUnresolved));
  lc := TFpSonarIssueCollector.Create;
  try
    RunRuleSrc(NewDeadStore, 'gtunresolved.pas', cFlowUnresolved, lc);
    AssertEquals('the sibling rule still fires', 1,
      CountById(lc, cDeadStoreId));
  finally
    lc.Free;
  end;
end;


procedure TRulesDataFlowTest.FlowVerdictsAreMutuallyExclusive;

begin
  { One site yields at most one verdict, so each noncompliant fixture is also
    asserted zero for every id that does not own its rows. }
  AssertResolvesClean('dsnoncompliant.pas', cDeadStoreNoncompliant);
  AssertEquals('a dead store is no strict read', 0,
    CountSrc(NewUninitializedVariableStrict, 'dsnoncompliant.pas',
      cUninitializedVariableStrictId, cDeadStoreNoncompliant));
  AssertEquals('a dead store is not self-derived', 0,
    CountSrc(NewSelfAssignedNeverUsed, 'dsnoncompliant.pas',
      cSelfAssignedNeverUsedId, cDeadStoreNoncompliant));
  AssertEquals('a dead store is no result store', 0,
    CountSrc(NewResultOverwrittenBeforeExit, 'dsnoncompliant.pas',
      cResultOverwrittenBeforeExitId, cDeadStoreNoncompliant));

  // The self-derived store owns its own row and the store it reads is read.
  AssertResolvesClean('sanunoncompliant.pas', cSelfNoncompliant);
  AssertEquals('neither store is dead', 0,
    CountSrc(NewDeadStore, 'sanunoncompliant.pas', cDeadStoreId,
      cSelfNoncompliant));

  // The result slot yields the result verdict alone.
  AssertResolvesClean('robenoncompliant.pas', cResultNoncompliant);
  AssertEquals('a result store is no dead store', 0,
    CountSrc(NewDeadStore, 'robenoncompliant.pas', cDeadStoreId,
      cResultNoncompliant));
  AssertResolvesClean('robecompliant.pas', cResultCompliant);
  AssertEquals('a self-derived result store yields nothing', 0,
    CountSrc(NewSelfAssignedNeverUsed, 'robecompliant.pas',
      cSelfAssignedNeverUsedId, cResultCompliant));

  // The read that carries the strict verdict also silences the store it reads.
  AssertResolvesClean('uvsnoncompliant.pas', cStrictNoncompliant);
  AssertEquals('the read silences the store', 0,
    CountSrc(NewDeadStore, 'uvsnoncompliant.pas', cDeadStoreId,
      cStrictNoncompliant));
end;


procedure TRulesDataFlowTest.DataFlowRulesSelfRegisterGlobally;

const
  cIds: array[0..4] of string = (cUninitializedVariableId, cDeadStoreId,
    cUninitializedVariableStrictId, cSelfAssignedNeverUsedId,
    cResultOverwrittenBeforeExitId);

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
end;


initialization
  RegisterTest(TRulesDataFlowTest);

end.
