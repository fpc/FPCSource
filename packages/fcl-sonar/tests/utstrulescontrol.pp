{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the control-flow (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesControl;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework, FpSonar.SourceFile,
  FpSonar.Rules.Control, UtstFixtures;

type
  { SEM-tier control-flow-rule position + registration tests. }
  TRulesControlTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires once at aDeclLine, column 1, with key
    // rule.<aId>.message and message args aArgs, and zero on the compliant
    // fixture.
    procedure CheckControlRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // The token-tier sibling of CheckControlRuleSrc: identical, except that the
    // issue must carry aDeclCol as both its start and its end column.
    procedure CheckControlTokenRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine, aDeclCol: Integer;
      const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewExhaustiveCaseStatement: TRuleBase;
    function NewExceptionRaised: TRuleBase;
    function NewSingleIterationLoop: TRuleBase;
    function NewNoPascalStyleResultAssignment: TRuleBase;
    function NewRedundantAssignedCheckBeforeFree: TRuleBase;
    function NewLoopBeyondCollectionEnd: TRuleBase;
    function NewRoutineResultAssigned: TRuleBase;
    function NewNoCatchRawException: TRuleBase;
    function NewNoRaiseRawException: TRuleBase;
    function NewIdenticalBranches: TRuleBase;
    function NewDuplicateConditionInChain: TRuleBase;
    function NewDuplicateCaseLabel: TRuleBase;
    function NewSelfComparison: TRuleBase;
    function NewEmptyThenWithFollowingStatement: TRuleBase;
    function NewMixedBooleanAndRelational: TRuleBase;
    function NewBitwiseOnBooleanOperands: TRuleBase;
    function NewAssignmentInsteadOfComparison: TRuleBase;
    function NewConditionWithSideEffect: TRuleBase;
    function NewRedundantElseAfterExit: TRuleBase;
    function NewCollapsibleNestedIf: TRuleBase;
    function NewNegatedConditionWithElse: TRuleBase;
    function NewSwitchOnBooleanExpression: TRuleBase;
    function NewLoopConditionNeverChanges: TRuleBase;
    function NewUnreachableCode: TRuleBase;
    // Asserts aPath parses while its resolution fails, which is what puts an
    // rfResolver rule out of reach of the shape and forces the AST tier.
    procedure AssertFixtureParsesWithoutResolving(const aPath: string);
    // The positive control for the above: aPath parses AND resolves.
    procedure AssertFixtureResolves(const aPath: string);
    // Asserts aPath fails to parse, so no module reaches the AST feed.
    procedure AssertFixtureDoesNotParse(const aPath: string);
    // Asserts aPath parses, so a zero finding count is not a staging failure.
    procedure AssertFixtureParses(const aPath: string);
    { Asserts RoutineResultAssigned fires over the inline source aSrc exactly
      once, at aRow; every other function of aSrc is thereby measured silent. }
    procedure CheckResultAssignedSoleRow(const aName: string;
      const aSrc: array of string; aRow: Integer);
  published
    procedure ExhaustiveCaseStatementPositions;
    procedure ExceptionRaisedPositions;
    procedure SingleIterationLoopPositions;
    procedure NoPascalStyleResultAssignmentPositions;
    procedure NoPascalStyleResultAssignmentMethodFires;
    procedure RedundantAssignedCheckBeforeFreePositions;
    procedure LoopBeyondCollectionEndPositions;
    procedure RoutineResultAssignedPositions;
    procedure RoutineResultAssignedReportsCaseWithoutElse;
    procedure RoutineResultAssignedSilentOnReRaisingExcept;
    procedure RoutineResultAssignedSilentOnTerminatingPath;
    procedure RoutineResultAssignedSilentOnManagedResult;
    procedure RoutineResultAssignedSilentOnNestedRoutineWrite;
    procedure RoutineResultAssignedSilentOnExitValueInLoop;
    procedure RoutineResultAssignedSilentOnOperatorAndAssembler;
    procedure RoutineResultAssignedDegradesWithoutResolver;
    procedure RoutineResultAssignedSilentOnUnresolvedOperand;
    procedure NoCatchRawExceptionPositions;
    procedure NoRaiseRawExceptionPositions;
    procedure IdenticalBranchesPositions;
    procedure IdenticalBranchesDegradesOnParseFailure;
    procedure DuplicateConditionInChainPositions;
    procedure DuplicateConditionInChainDegradesOnParseFailure;
    procedure DuplicateCaseLabelPositions;
    procedure DuplicateCaseLabelDegradesOnParseFailure;
    procedure SelfComparisonPositions;
    procedure SelfComparisonDegradesOnParseFailure;
    procedure EmptyThenWithFollowingStatementPositions;
    procedure EmptyThenWithFollowingStatementDegradesOnParseFailure;
    procedure MixedBooleanAndRelationalPositions;
    procedure MixedBooleanAndRelationalRunsWithoutParse;
    procedure BitwiseOnBooleanOperandsPositions;
    procedure BitwiseOnBooleanOperandsDegradesOnParseFailure;
    procedure AssignmentInsteadOfComparisonPositions;
    procedure AssignmentInsteadOfComparisonDegradesOnParseFailure;
    procedure ConditionWithSideEffectPositions;
    procedure ConditionWithSideEffectDegradesWithoutResolver;
    procedure ConditionWithSideEffectSilentOnUnresolvedOperand;
    procedure RedundantElseAfterExitPositions;
    procedure RedundantElseAfterExitDegradesOnParseFailure;
    procedure CollapsibleNestedIfPositions;
    procedure CollapsibleNestedIfDegradesOnParseFailure;
    procedure NegatedConditionWithElsePositions;
    procedure NegatedConditionWithElseDegradesOnParseFailure;
    procedure SwitchOnBooleanExpressionPositions;
    procedure SwitchOnBooleanExpressionDegradesWithoutResolver;
    procedure SwitchOnBooleanExpressionSilentOnUnresolvedOperand;
    procedure LoopConditionNeverChangesPositions;
    procedure LoopConditionNeverChangesDegradesWithoutResolver;
    procedure LoopConditionNeverChangesSilentOnUnresolvedOperand;
    procedure UnreachableCodePositions;
    procedure UnreachableCodeReportsFlowDeadCode;
    procedure UnreachableCodeSilentOnLiveContainers;
    procedure UnreachableCodeDegradesOnParseFailure;
    procedure IfShapeRulesDoNotOverlapLoopAndFreeRules;
    procedure ControlRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');
  cExhaustiveCaseStatementId = 'ExhaustiveCaseStatement';
  cExceptionRaisedId = 'ExceptionRaised';
  cSingleIterationLoopId = 'SingleIterationLoop';
  cNoPascalStyleResultAssignmentId = 'NoPascalStyleResultAssignment';
  cRedundantAssignedCheckBeforeFreeId = 'RedundantAssignedCheckBeforeFree';
  cLoopBeyondCollectionEndId = 'LoopBeyondCollectionEnd';
  cRoutineResultAssignedId = 'RoutineResultAssigned';
  cNoCatchRawExceptionId = 'NoCatchRawException';
  cNoRaiseRawExceptionId = 'NoRaiseRawException';
  cIdenticalBranchesId = 'IdenticalBranches';
  cDuplicateConditionInChainId = 'DuplicateConditionInChain';
  cDuplicateCaseLabelId = 'DuplicateCaseLabel';
  cSelfComparisonId = 'SelfComparison';
  cEmptyThenWithFollowingStatementId = 'EmptyThenWithFollowingStatement';
  cMixedBooleanAndRelationalId = 'MixedBooleanAndRelational';
  cBitwiseOnBooleanOperandsId = 'BitwiseOnBooleanOperands';
  cAssignmentInsteadOfComparisonId = 'AssignmentInsteadOfComparison';
  cConditionWithSideEffectId = 'ConditionWithSideEffect';
  cRedundantElseAfterExitId = 'RedundantElseAfterExit';
  cCollapsibleNestedIfId = 'CollapsibleNestedIf';
  cNegatedConditionWithElseId = 'NegatedConditionWithElse';
  cSwitchOnBooleanExpressionId = 'SwitchOnBooleanExpression';
  cLoopConditionNeverChangesId = 'LoopConditionNeverChanges';
  cUnreachableCodeId = 'UnreachableCode';

  // Embedded control-flow-rule fixtures: line i+1 == [i].

  cExhaustiveCaseStatementNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TColor = (cRed, cGreen, cBlue);',
    'procedure P(c: TColor);',
    'implementation',
    'procedure P(c: TColor);',
    'var',
    '  n: Integer;',
    'begin',
    '  case c of',
    '    cRed:   n := 1;',
    '    cGreen: n := 2;',
    '  end;',
    'end;',
    'end.');

  cExhaustiveCaseStatementCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TColor = (cRed, cGreen, cBlue);',
    'procedure P(c: TColor);',
    'implementation',
    'procedure P(c: TColor);',
    'var',
    '  n: Integer;',
    'begin',
    '  case c of',
    '    cRed:   n := 1;',
    '    cGreen: n := 2;',
    '    cBlue:  n := 3;',
    '  end;',
    'end;',
    'end.');

  cExceptionRaisedNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  EBadValue = class(Exception) end;',
    'procedure P;',
    'implementation',
    'procedure P;',
    'begin',
    '  EBadValue.Create(''bad value'');',
    'end;',
    'end.');

  cExceptionRaisedCompliant: array[0..16] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  EBadValue = class(Exception) end;',
    'procedure P;',
    'implementation',
    'procedure P;',
    'var',
    '  e: EBadValue;',
    'begin',
    '  e := EBadValue.Create(''x'');',
    '  TObject.Create();',
    '  raise EBadValue.Create(''bad value'');',
    'end;',
    'end.');

  cSingleIterationLoopNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P;',
    'implementation',
    'procedure P;',
    'var',
    '  done: Boolean;',
    '  n: Integer;',
    'begin',
    '  done := False;',
    '  n := 0;',
    '  while not done do',
    '  begin',
    '    n := n + 1;',
    '    break;',
    '  end;',
    'end;',
    'end.');

  cSingleIterationLoopCompliant: array[0..31] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P;',
    'implementation',
    'procedure P;',
    'var',
    '  done: Boolean;',
    '  n: Integer;',
    'begin',
    '  done := False;',
    '  n := 0;',
    '  while not done do',
    '  begin',
    '    n := n + 1;',
    '    if n > 10 then',
    '      break;',
    '  end;',
    '  while not done do',
    '  begin',
    '    if n > 100 then',
    '      continue;',
    '    n := n + 1;',
    '  end;',
    '  while not done do',
    '  begin',
    '    if n > 5 then',
    '      continue;',
    '    break;',
    '  end;',
    'end;',
    'end.');

  cNoPascalStyleResultAssignmentNoncompliant: array[0..9] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function Add(a, b: Integer): Integer;',
    'implementation',
    'function Add(a, b: Integer): Integer;',
    'begin',
    '  Add := a + b;',
    'end;',
    'end.');

  cNoPascalStyleResultAssignmentCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function Add(a, b: Integer): Integer;',
    'function Calc(a: Integer): Integer;',
    'implementation',
    'function Add(a, b: Integer): Integer;',
    'begin',
    '  Result := a + b;',
    'end;',
    'function Calc(a: Integer): Integer;',
    'var',
    '  Calc: Integer;',
    'begin',
    '  Calc := a + 1;',
    '  Result := Calc;',
    'end;',
    'end.');

  cRedundantAssignedCheckBeforeFreeNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure P(o: TObject);',
    'implementation',
    'procedure P(o: TObject);',
    'begin',
    '  if Assigned(o) then',
    '    o.Free;',
    'end;',
    'end.');

  cRedundantAssignedCheckBeforeFreeCompliant: array[0..28] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure P(o: TObject);',
    'procedure Q(o: TObject);',
    'procedure R(a, b: TObject);',
    'implementation',
    'procedure DoSomething;',
    'begin',
    'end;',
    'procedure P(o: TObject);',
    'begin',
    '  o.Free;',
    'end;',
    'procedure Q(o: TObject);',
    'begin',
    '  if Assigned(o) then',
    '  begin',
    '    DoSomething;',
    '    o.Free;',
    '  end;',
    'end;',
    'procedure R(a, b: TObject);',
    'begin',
    '  if Assigned(a) then',
    '    b.Free;',
    'end;',
    'end.');

  cLoopBeyondCollectionEndNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P;',
    'implementation',
    'procedure P;',
    'var',
    '  A: array[0..9] of Integer;',
    '  i: Integer;',
    'begin',
    '  for i := 0 to Length(A) do',
    '    A[i] := 0;',
    'end;',
    'end.');

  cLoopBeyondCollectionEndCompliant: array[0..21] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(n: Integer);',
    'implementation',
    'procedure P(n: Integer);',
    'var',
    '  A: array[0..9] of Integer;',
    '  D: array of Integer;',
    '  i: Integer;',
    'begin',
    '  for i := Low(A) to High(A) do',
    '    A[i] := 0;',
    '  SetLength(D, 10);',
    '  for i := 0 to High(D) do',
    '    D[i] := 0;',
    '  for i := 0 to n do',
    '    A[i] := 0;',
    '  for i := 5 to 2 do',
    '    A[i+10] := 0;',
    'end;',
    'end.');

  cRoutineResultAssignedNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Pick(b: Boolean): Integer;',
    '',
    'implementation',
    '',
    'function Pick(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;          // no else => the b=False path falls off end with no',
    '                          // result write -> RoutineResultAssigned',
    'end;',
    '',
    'end.');

  cRoutineResultAssignedCompliant: array[0..104] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses SysUtils;',
    '',
    'type',
    '  EMyError = class(Exception) end;',
    '  TRec = record',
    '    V: Integer;',
    '  end;',
    '',
    'function Sole(a, b: Integer): Integer;',
    'function IfElse(b: Boolean): Integer;',
    'function CaseAll(b: Integer): Integer;',
    'function NameForm: Integer;',
    'function ExitVal: Integer;',
    'function RaiseAll(b: Boolean): Integer;',
    'function OutParam: Integer;',
    'function WithBody: Integer;',
    'function LoopWrite: Integer;',
    '',
    'implementation',
    '',
    '// Sole statement is a result write.',
    'function Sole(a, b: Integer): Integer;',
    'begin',
    '  Result := a + b;',
    'end;',
    '',
    '// Both branches of the if/else write the result.',
    'function IfElse(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1',
    '  else',
    '    Result := 2;',
    'end;',
    '',
    '// Every case branch AND the else write the result.',
    'function CaseAll(b: Integer): Integer;',
    'begin',
    '  case b of',
    '    0: Result := 1;',
    '    1: Result := 2;',
    '  else',
    '    Result := 3;',
    '  end;',
    'end;',
    '',
    '// The legacy name-style return collapses onto the result element (a write).',
    'function NameForm: Integer;',
    'begin',
    '  NameForm := 1;',
    'end;',
    '',
    '// exit(value) is a value-returning terminator on every path.',
    'function ExitVal: Integer;',
    'begin',
    '  exit(0);',
    'end;',
    '',
    '// One branch writes the result, the other raises (a no-fall-through terminator).',
    'function RaiseAll(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1',
    '  else',
    '    raise EMyError.Create(''no value'');',
    'end;',
    '',
    '// The result is written through an out parameter.',
    'procedure FillIt(out X: Integer);',
    'begin',
    '  X := 0;',
    'end;',
    '',
    'function OutParam: Integer;',
    'begin',
    '  FillIt(Result);',
    'end;',
    '',
    '// A with-scoped write: the graph carries it on the only path => silent.',
    'function WithBody: Integer;',
    'var',
    '  r: TRec;',
    'begin',
    '  r.V := 7;',
    '  with r do',
    '    Result := V;',
    'end;',
    '',
    '// A loop body writes the result: the slot is dropped whole, since the',
    '// zero-trip edge reaches the exit unassigned.',
    'function LoopWrite: Integer;',
    'var',
    '  i: Integer;',
    'begin',
    '  for i := 1 to 10 do',
    '    Result := i;',
    'end;',
    '',
    'end.');

  cRoutineResultAssignedCaseNoElse: array[0..12] of string = (
    'unit cases;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function CaseNoElse(b: Integer): Integer;',
    'implementation',
    'function CaseNoElse(b: Integer): Integer;',
    'begin',
    '  case b of',
    '    0: Result := 1;',
    '    1: Result := 2;',
    '  end;',
    'end;',
    'end.');

  cRoutineResultAssignedReRaise: array[0..19] of string = (
    'unit reraise;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function TryReraise: Integer;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'function TryReraise: Integer;',
    'begin',
    '  try',
    '    Result := 1;',
    '  except',
    '    raise;',
    '  end;',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cRoutineResultAssignedTerminators: array[0..32] of string = (
    'unit terminators;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFailure = class(TObject)',
    '  end;',
    'procedure Halt(aCode: Integer);',
    'function EndsWithRaise: Integer;',
    'function EndsWithExitValue: Integer;',
    'function EndsWithHalt: Integer;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'procedure Halt(aCode: Integer);',
    'begin',
    'end;',
    'function EndsWithRaise: Integer;',
    'begin',
    '  raise TFailure.Create;',
    'end;',
    'function EndsWithExitValue: Integer;',
    'begin',
    '  exit(7);',
    'end;',
    'function EndsWithHalt: Integer;',
    'begin',
    '  Halt(1);',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cRoutineResultAssignedManaged: array[0..14] of string = (
    'unit managed;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function ManagedResult: string;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'function ManagedResult: string;',
    'begin',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cRoutineResultAssignedNestedWrite: array[0..21] of string = (
    'unit nested;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function NestedWrite: Integer;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'function NestedWrite: Integer;',
    '',
    '  procedure Inner;',
    '  begin',
    '    Result := 3;',
    '  end;',
    '',
    'begin',
    '  Inner;',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cRoutineResultAssignedExitInLoop: array[0..19] of string = (
    'unit exitinloop;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function FirstHit(n: Integer): Integer;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'function FirstHit(n: Integer): Integer;',
    'var',
    '  i: Integer;',
    'begin',
    '  for i := 1 to n do',
    '    if i = n then',
    '      Exit(i);',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cRoutineResultAssignedOperatorAndAsm: array[0..22] of string = (
    'unit shapes;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TRec = record',
    '    V: Integer;',
    '  end;',
    'operator = (a, b: TRec) r: Boolean;',
    'function AsmBody: Integer; assembler;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'operator = (a, b: TRec) r: Boolean;',
    'begin',
    'end;',
    'function AsmBody: Integer; assembler;',
    'asm',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cRoutineResultAssignedUnanswerable: array[0..19] of string = (
    'unit unanswerable;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function AsmBody(b: Boolean): Integer;',
    'function Sibling(b: Boolean): Integer;',
    'implementation',
    'function AsmBody(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    '  asm',
    '    nop',
    '  end;',
    'end;',
    'function Sibling(b: Boolean): Integer;',
    'begin',
    '  if b then',
    '    Result := 1;',
    'end;',
    'end.');

  cNoCatchRawExceptionNoncompliant: array[0..25] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'uses SysUtils;',
    '',
    'procedure DoWork;',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'begin',
    '  try',
    '    DoWork;',
    '  except',
    '    on E: Exception do DoWork; // NoCatchRawException',
    '  end;',
    'end;',
    '',
    'end.');

  cNoCatchRawExceptionCompliant: array[0..28] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'uses SysUtils;',
    '',
    'procedure DoWork;',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'begin',
    '  // Specific subclass caught — compliant (silent).',
    '  try DoWork; except on E: EConvertError do DoWork; end;',
    '  // Root caught but re-raised — re-raise exempt (silent).',
    '  try DoWork; except on E: Exception do raise; end;',
    '  // Empty bare except — that is ExceptionsNotSwallowed (#49)''s domain (silent here).',
    '  try DoWork; except end;',
    '  // Non-empty bare catch-all that re-raises — exempt (silent).',
    '  try DoWork; except DoWork; raise; end;',
    'end;',
    '',
    'end.');

  cNoRaiseRawExceptionNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Boom;',
    '',
    'implementation',
    '',
    'uses SysUtils;',
    '',
    'procedure Boom;',
    'begin',
    '  raise Exception.Create(''something failed''); // NoRaiseRawException',
    'end;',
    '',
    'end.');

  cNoRaiseRawExceptionCompliant: array[0..33] of string = (
    'unit compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Demo;',
    '',
    'implementation',
    '',
    'uses SysUtils;',
    '',
    'type',
    '  EMyError = class(Exception);',
    '',
    'procedure Demo;',
    'begin',
    '  // A real RTL subclass raised directly — specific, compliant (silent).',
    '  raise EConvertError.Create(''bad'');',
    'end;',
    '',
    'procedure Rethrow;',
    'begin',
    '  try',
    '    Demo;',
    '  except',
    '    on E: EMyError do',
    '      raise; // bare re-raise, ExceptObject=nil — silent',
    '  end;',
    '  // A user subclass raised directly — specific, compliant (silent).',
    '  raise EMyError.Create(''x'');',
    'end;',
    '',
    'end.');

  cNoPascalStyleResultAssignmentMethod: array[0..16] of string = (
    'unit method;',
    '{$mode objfpc}{$H+}',
    'interface',
    '',
    'type',
    '  TBar = class',
    '    function Calc(a: Integer): Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'function TBar.Calc(a: Integer): Integer;',
    'begin',
    '  Calc := a + 1;',
    'end;',
    '',
    'end.');

  cNoCatchRawExceptionBareExcept: array[0..25] of string = (
    'unit bareexcept;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'uses SysUtils;',
    '',
    'procedure DoWork;',
    'begin',
    'end;',
    '',
    'procedure Run;',
    'begin',
    '  try',
    '    DoWork;',
    '  except',
    '    DoWork; // NoCatchRawException (non-empty bare catch-all, swallows everything)',
    '  end;',
    'end;',
    '',
    'end.');

  cIdenticalBranchesNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure P(c: Boolean);',
    'var',
    '  x: Integer;',
    'begin',
    '  if c then',
    '    x := 1',
    '  else',
    '    x := 1;',
    'end;',
    'end.');

  cIdenticalBranchesCompliant: array[0..27] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure P(c: Boolean);',
    'var',
    '  x, i: Integer;',
    'begin',
    '  if c then',
    '    x := 1',
    '  else',
    '    x := 2;',
    '  if c then',
    '    x := 3;',
    '  if c then else ;',
    '  if c then',
    '  begin',
    '  end',
    '  else',
    '  begin',
    '  end;',
    '  if c then',
    '    for i := 1 to 2 do x := i',
    '  else',
    '    for i := 1 to 2 do x := i;',
    'end;',
    'end.');

  cIdenticalBranchesBlocks: array[0..20] of string = (
    'unit blocks;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure P(c: Boolean);',
    'var',
    '  x, y: Integer;',
    'begin',
    '  if c then',
    '  begin',
    '    x := 1;',
    '    y := 2;',
    '  end',
    '  else',
    '  begin',
    '    x := 1;',
    '    y := 2;',
    '  end;',
    'end;',
    'end.');

  cDuplicateConditionInChainNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a: Integer);',
    'implementation',
    'procedure P(a: Integer);',
    'var',
    '  x: Integer;',
    'begin',
    '  if a > 1 then',
    '    x := 1',
    '  else if a > 1 then',
    '    x := 2;',
    'end;',
    'end.');

  cDuplicateConditionInChainCompliant: array[0..19] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b: Integer);',
    'implementation',
    'procedure P(a, b: Integer);',
    'var',
    '  x: Integer;',
    'begin',
    '  if a > 1 then',
    '    x := 1',
    '  else if b > 1 then',
    '    x := 2;',
    '  if a > 1 then',
    '  begin',
    '    if a > 1 then',
    '      x := 3;',
    '  end;',
    'end;',
    'end.');

  cDuplicateConditionInChainOfThree: array[0..16] of string = (
    'unit chainofthree;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b: Boolean);',
    'implementation',
    'procedure P(a, b: Boolean);',
    'var',
    '  x: Integer;',
    'begin',
    '  if a then',
    '    x := 1',
    '  else if b then',
    '    x := 2',
    '  else if a then',
    '    x := 3;',
    'end;',
    'end.');

  cDuplicateConditionInChainInnerRepeat: array[0..16] of string = (
    'unit innerrepeat;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b: Boolean);',
    'implementation',
    'procedure P(a, b: Boolean);',
    'var',
    '  x: Integer;',
    'begin',
    '  if a then',
    '    x := 1',
    '  else if b then',
    '    x := 2',
    '  else if b then',
    '    x := 3;',
    'end;',
    'end.');

  cDuplicateCaseLabelNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(i: Integer);',
    'implementation',
    'procedure P(i: Integer);',
    'var',
    '  x: Integer;',
    'begin',
    '  case i of',
    '    1: x := 1;',
    '    1: x := 2;',
    '  end;',
    'end;',
    'end.');

  cDuplicateCaseLabelCompliant: array[0..24] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(i: Integer; ch: Char);',
    'implementation',
    'procedure P(i: Integer; ch: Char);',
    'var',
    '  x: Integer;',
    'begin',
    '  case i of',
    '    1..5: x := 1;',
    '    3: x := 2;',
    '  end;',
    '  case i of',
    '    7: x := 1;',
    '    8: x := 2;',
    '  else',
    '    x := 3;',
    '  end;',
    '  case ch of',
    '    ''a''..''z'': x := 1;',
    '    ''A''..''Z'': x := 2;',
    '  end;',
    'end;',
    'end.');

  cDuplicateCaseLabelCommaForm: array[0..14] of string = (
    'unit commalabels;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(i: Integer);',
    'implementation',
    'procedure P(i: Integer);',
    'var',
    '  x: Integer;',
    'begin',
    '  case i of',
    '    2, 2: x := 1;',
    '    9: x := 2;',
    '  end;',
    'end;',
    'end.');

  cDuplicateCaseLabelRangeForm: array[0..14] of string = (
    'unit rangelabels;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(i: Integer);',
    'implementation',
    'procedure P(i: Integer);',
    'var',
    '  x: Integer;',
    'begin',
    '  case i of',
    '    1..3: x := 1;',
    '    1..3: x := 2;',
    '  end;',
    'end;',
    'end.');

  cSelfComparisonNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(x: Integer);',
    'implementation',
    'procedure P(x: Integer);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  if x = x then',
    '    n := 1;',
    'end;',
    'end.');

  cSelfComparisonCompliant: array[0..22] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function F(a: Integer): Integer;',
    'procedure P(x, y: Integer);',
    'implementation',
    'function F(a: Integer): Integer;',
    'begin',
    '  Result := a;',
    'end;',
    'procedure P(x, y: Integer);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  if x = y then',
    '    n := 1;',
    '  if F(1) = F(1) then',
    '    n := 2;',
    '  if 1 = 1 then',
    '    n := 3;',
    'end;',
    'end.');

  cSelfComparisonOrdering: array[0..15] of string = (
    'unit ordering;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBag = class(TObject)',
    '    Count: Integer;',
    '  end;',
    'procedure P(a: TBag);',
    'implementation',
    'procedure P(a: TBag);',
    'var',
    '  b: Boolean;',
    'begin',
    '  b := a.Count <= a.Count;',
    'end;',
    'end.');

  cEmptyThenWithFollowingStatementNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure P(c: Boolean);',
    'var',
    '  x: Integer;',
    'begin',
    '  if c then ;',
    '  x := 1;',
    'end;',
    'end.');

  cEmptyThenWithFollowingStatementCompliant: array[0..20] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(c: Boolean);',
    'begin',
    '  if c then else DoIt;',
    '  if c then begin end;',
    '  DoIt;',
    '  try',
    '    if c then ;',
    '  finally',
    '    DoIt;',
    '  end;',
    '  if c then ;',
    'end;',
    'end.');

  // The two container classes whose Elements really are a sibling statement
  // list, reached through the tightened block predicate.
  cEmptyThenInNestedBlocks: array[0..21] of string = (
    'unit nestedblocks;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(c: Boolean);',
    'begin',
    '  repeat',
    '    if c then ;',
    '    DoIt;',
    '  until c;',
    '  try',
    '    if c then ;',
    '    DoIt;',
    '  finally',
    '    DoIt;',
    '  end;',
    'end;',
    'end.');

  // One unparseable fixture carrying all five shapes: the class declaration is
  // missing its closing parenthesis.
  cDuplicateShapesUnparseable: array[0..29] of string = (
    'unit Broken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBag = class(TObject',
    '  public',
    '    Count: Integer;',
    '  end;',
    'procedure P(a: TBag; c: Boolean; i, x: Integer);',
    'implementation',
    'procedure P(a: TBag; c: Boolean; i, x: Integer);',
    'begin',
    '  if c then',
    '    x := 1',
    '  else',
    '    x := 1;',
    '  if i > 1 then',
    '    x := 2',
    '  else if i > 1 then',
    '    x := 3;',
    '  case i of',
    '    1: x := 4;',
    '    1: x := 5;',
    '  end;',
    '  if x = x then',
    '    x := 6;',
    '  if c then ;',
    '  x := 7;',
    'end;',
    'end.');

  cMixedBooleanAndRelationalNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b, c: Boolean);',
    'implementation',
    'procedure P(a, b, c: Boolean);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  if a and b = c then',
    '    n := 1;',
    'end;',
    'end.');

  cMixedBooleanAndRelationalCompliant: array[0..45] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b, c: Boolean; i, n: Integer);',
    'implementation',
    'procedure P(a, b, c: Boolean; i, n: Integer);',
    'var',
    '  x: Integer;',
    '  arr: array[1..3] of Integer;',
    'begin',
    '  x := 0;',
    '  if a and arr[i = n] then',
    '    x := 10;',
    '  if (a and b) = c then',
    '    x := 1;',
    '  if a and (b = c) then',
    '    x := 2;',
    '  if (i = 1) and (n = 2) then',
    '    x := 3;',
    '  if a and b then',
    '    x := 4;',
    '  if i = n then',
    '    x := 5;',
    '  if a and b then x := i = n;',
    '  if not a = b then',
    '    x := 6;',
    '  try',
    '    b := i = n',
    '  finally',
    '    if a and b then',
    '      x := 7;',
    '  end;',
    '  try',
    '    b := i = n',
    '  except',
    '    if a and b then',
    '      x := 8;',
    '  end;',
    '  case i of',
    '  1: b := i = n',
    '  otherwise',
    '    if a and b then',
    '      x := 9;',
    '  end;',
    'end;',
    'end.');

  cMixedBooleanAndRelationalVariants: array[0..19] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b, c, d, e: Boolean; i, n: Integer; flag: Boolean);',
    'implementation',
    'procedure P(a, b, c, d, e: Boolean; i, n: Integer; flag: Boolean);',
    'var',
    '  x: Integer;',
    'begin',
    '  x := 0;',
    '  if a = b and c then',
    '    x := 1;',
    '  while i < n and flag do',
    '    x := 2;',
    '  if a and b = c and d = e then',
    '    x := 3;',
    '  if a xor b <> c then',
    '    x := 4;',
    'end;',
    'end.');

  // The condition carries a ')' with no opener.
  cMixedBooleanAndRelationalSurplusParen: array[0..10] of string = (
    'unit surplus;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b, c: Boolean);',
    'implementation',
    'procedure P(a, b, c: Boolean);',
    'begin',
    '  if a) and b = c then',
    '    Halt(1);',
    'end;',
    'end.');

  // One unparseable fixture carrying all four operator-trap shapes: the class
  // declaration is missing its closing parenthesis.
  cOperatorTrapsUnparseable: array[0..40] of string = (
    'unit Broken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBag = class(TObject',
    '  public',
    '    Count: Integer;',
    '  end;',
    'function Check(Found: Boolean): Boolean;',
    'procedure P(a, b, c: Boolean; flags: Integer);',
    'implementation',
    'function Check(Found: Boolean): Boolean;',
    'begin',
    '  Result := Found;',
    'end;',
    'procedure P(a, b, c: Boolean; flags: Integer);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  if a and b = c then',
    '    n := 1;',
    '  if flags and $10 then',
    '    n := 2;',
    '  if Check(a := True) then',
    '    n := 3;',
    'end;',
    'function TryGet(var aValue: Integer): Boolean;',
    'begin',
    '  aValue := 1;',
    '  Result := True;',
    'end;',
    'procedure Q(a: Boolean);',
    'var',
    '  m: Integer;',
    'begin',
    '  m := 0;',
    '  if a and TryGet(m) then',
    '    m := 1;',
    'end;',
    'end.');

  cBitwiseOnBooleanOperandsNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(flags: Integer);',
    'implementation',
    'procedure P(flags: Integer);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  if flags and $10 then',
    '    n := 1;',
    'end;',
    'end.');

  cBitwiseOnBooleanOperandsCompliant: array[0..18] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(flags: Integer; a, b: Boolean);',
    'implementation',
    'procedure P(flags: Integer; a, b: Boolean);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  if (flags and $10) <> 0 then',
    '    n := 1;',
    '  n := flags and $F;',
    '  if a and b then',
    '    n := 2;',
    '  if not flags then',
    '    n := 3;',
    'end;',
    'end.');

  cBitwiseOnBooleanOperandsVariants: array[0..15] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(i: Integer);',
    'implementation',
    'procedure P(i: Integer);',
    'var',
    '  n: Integer;',
    '  b: Boolean;',
    'begin',
    '  n := Ord((i > 0) and $F);',
    '  b := True or 1;',
    '  if True and 1 then',
    '    n := 1;',
    'end;',
    'end.');

  cAssignmentInsteadOfComparisonNoncompliant: array[0..20] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function Check(Found: Boolean): Boolean;',
    'procedure P;',
    'implementation',
    'function Check(Found: Boolean): Boolean;',
    'begin',
    '  Result := Found;',
    'end;',
    'procedure P;',
    'var',
    '  n: Integer;',
    '  found: Boolean;',
    'begin',
    '  n := 0;',
    '  found := False;',
    '  if Check(found := True) then',
    '    n := 1;',
    'end;',
    'end.');

  // Stages ':=' in the enum-value, for-control-variable and assignment positions,
  // and as the index named argument ParseParams builds outside a call.
  cAssignmentInsteadOfComparisonCompliant: array[0..29] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TStep = (stFirst := 1, stNext := 2);',
    'function Check(Found: Boolean): Boolean;',
    'procedure P;',
    'implementation',
    'function Check(Found: Boolean): Boolean;',
    'begin',
    '  Result := Found;',
    'end;',
    'procedure P;',
    'var',
    '  n, i: Integer;',
    '  found: Boolean;',
    '  s: TStep;',
    '  arr: array[1..3] of Integer;',
    'begin',
    '  n := 0;',
    '  found := False;',
    '  if Check(found = True) then',
    '    n := 1;',
    '  for i := 1 to 3 do',
    '    n := 2;',
    '  s := stFirst;',
    '  n := Ord(s);',
    '  n := arr[i := 1];',
    'end;',
    'end.');

  cConditionWithSideEffectNoncompliant: array[0..20] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function TryGet(var aValue: Integer): Boolean;',
    'procedure P(ready: Boolean);',
    'implementation',
    'function TryGet(var aValue: Integer): Boolean;',
    'begin',
    '  aValue := 1;',
    '  Result := True;',
    'end;',
    'procedure P(ready: Boolean);',
    'var',
    '  n, v: Integer;',
    'begin',
    '  n := 0;',
    '  v := 0;',
    '  if ready and TryGet(v) then',
    '    n := 1;',
    'end;',
    'end.');

  cConditionWithSideEffectCompliant: array[0..27] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function TryGet(var aValue: Integer): Boolean;',
    'function Get(const aValue: Integer): Boolean;',
    'procedure P(ready: Boolean);',
    'implementation',
    'function TryGet(var aValue: Integer): Boolean;',
    'begin',
    '  aValue := 1;',
    '  Result := True;',
    'end;',
    'function Get(const aValue: Integer): Boolean;',
    'begin',
    '  Result := aValue > 0;',
    'end;',
    'procedure P(ready: Boolean);',
    'var',
    '  n, v: Integer;',
    'begin',
    '  n := 0;',
    '  v := 0;',
    '  if TryGet(v) and ready then',
    '    n := 1;',
    '  if ready and Get(v) then',
    '    n := 2;',
    'end;',
    'end.');

  cConditionWithSideEffectVariants: array[0..31] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function TryGet(var aValue: Integer): Boolean;',
    'function TryOut(out aValue: Integer): Boolean;',
    'procedure P(ready, done: Boolean);',
    'implementation',
    'function TryGet(var aValue: Integer): Boolean;',
    'begin',
    '  aValue := 1;',
    '  Result := True;',
    'end;',
    'function TryOut(out aValue: Integer): Boolean;',
    'begin',
    '  aValue := 1;',
    '  Result := True;',
    'end;',
    'procedure P(ready, done: Boolean);',
    'var',
    '  n, v: Integer;',
    'begin',
    '  n := 0;',
    '  v := 0;',
    '  if ready and (TryGet(v)) then',
    '    n := 1;',
    '  while ready or TryOut(v) do',
    '    n := 2;',
    '  repeat',
    '    n := 3;',
    '  until done or TryGet(v);',
    'end;',
    'end.');

  // The directly-called TryPlain is the positive control for the procedure
  // variable on the line below it.
  cConditionWithSideEffectUnresolvedCallee: array[0..24] of string = (
    'unit unresolved;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TTryGet = function(var aValue: Integer): Boolean;',
    'function TryPlain(var aValue: Integer): Boolean;',
    'procedure P(ready: Boolean; fn: TTryGet);',
    'implementation',
    'function TryPlain(var aValue: Integer): Boolean;',
    'begin',
    '  aValue := 1;',
    '  Result := True;',
    'end;',
    'procedure P(ready: Boolean; fn: TTryGet);',
    'var',
    '  n, v: Integer;',
    'begin',
    '  n := 0;',
    '  v := 0;',
    '  if ready and TryPlain(v) then',
    '    n := 1;',
    '  if ready and fn(v) then',
    '    n := 2;',
    'end;',
    'end.');

  cRedundantElseAfterExitNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(c: Boolean);',
    'begin',
    '  if c then',
    '    Exit',
    '  else',
    '    DoIt;',
    'end;',
    'end.');

  cRedundantElseAfterExitCompliant: array[0..28] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure DoOther;',
    'begin',
    'end;',
    'procedure P(c: Boolean);',
    'begin',
    '  if c then',
    '    Exit;',
    '  if c then',
    '    DoIt',
    '  else',
    '    DoOther;',
    '  if c then',
    '  begin',
    '    Exit;',
    '    DoIt;',
    '  end',
    '  else',
    '    DoOther;',
    '  if c then else DoIt;',
    'end;',
    'end.');

  // The five terminator forms the noncompliant fixture cannot carry without
  // firing more than once.
  cRedundantElseAfterExitVariants: array[0..49] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  EFoo = class(TObject);',
    'function F(c: Boolean): Integer;',
    'procedure Q(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'function F(c: Boolean): Integer;',
    'begin',
    '  Result := 0;',
    '  if c then',
    '  begin',
    '    DoIt;',
    '    Exit;',
    '  end',
    '  else',
    '    DoIt;',
    '  if c then',
    '    Exit(0)',
    '  else',
    '    DoIt;',
    '  if c then',
    '    raise EFoo.Create',
    '  else',
    '    DoIt;',
    '  if c then',
    '    Halt(1)',
    '  else',
    '    DoIt;',
    'end;',
    'procedure Q(c: Boolean);',
    'var',
    '  i: Integer;',
    'begin',
    '  for i := 1 to 3 do',
    '    if c then',
    '      Break',
    '    else',
    '      DoIt;',
    '  while c do',
    '    if c then',
    '      Continue',
    '    else',
    '      DoIt;',
    'end;',
    'end.');

  cCollapsibleNestedIfNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(a, b: Boolean);',
    'begin',
    '  if a then',
    '    if b then',
    '      DoIt;',
    'end;',
    'end.');

  cCollapsibleNestedIfCompliant: array[0..37] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure DoOther;',
    'begin',
    'end;',
    'procedure P(a, b: Boolean);',
    'var',
    '  i: Integer;',
    'begin',
    '  if a then',
    '    if b then',
    '      DoIt',
    '    else',
    '      DoOther;',
    '  if a then',
    '  begin',
    '    if b then',
    '      DoIt;',
    '  end',
    '  else',
    '    DoOther;',
    '  if a then',
    '  begin',
    '    if b then',
    '      DoIt;',
    '    DoOther;',
    '  end;',
    '  if a then',
    '    for i := 1 to 3 do',
    '      DoIt;',
    'end;',
    'end.');

  cCollapsibleNestedIfVariants: array[0..25] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b, p: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(a, b, p: Boolean);',
    'begin',
    '  if a then',
    '  begin',
    '    if b then',
    '      DoIt;',
    '  end;',
    '  if p then',
    '    DoIt',
    '  else if a then',
    '    if b then',
    '      DoIt;',
    '  if a then',
    '    if b then',
    '      if p then',
    '        DoIt;',
    'end;',
    'end.');

  cNegatedConditionWithElseNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure DoOther;',
    'begin',
    'end;',
    'procedure P(c: Boolean);',
    'begin',
    '  if not c then',
    '    DoIt',
    '  else',
    '    DoOther;',
    'end;',
    'end.');

  cNegatedConditionWithElseCompliant: array[0..36] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(a, b, c, d: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure DoOther;',
    'begin',
    'end;',
    'procedure P(a, b, c, d: Boolean);',
    'begin',
    '  if not c then',
    '    DoIt;',
    '  if not c then',
    '    DoIt',
    '  else if d then',
    '    DoOther;',
    '  if not c then',
    '    DoIt',
    '  else ;',
    '  if a <> b then',
    '    DoIt',
    '  else',
    '    DoOther;',
    '  if not a and b then',
    '    DoIt',
    '  else',
    '    DoOther;',
    '  if not c then',
    '    DoIt',
    '  else',
    '  begin',
    '  end;',
    'end;',
    'end.');

  // The two shapes the story names as the disjointness target, in a fixture
  // that resolves so both of those rfResolver rules are actually dispatched.
  cIfShapeOverlapExisting: array[0..19] of string = (
    'unit overlapexisting;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure P(o: TObject);',
    'implementation',
    'procedure P(o: TObject);',
    'var',
    '  i, n: Integer;',
    'begin',
    '  n := 0;',
    '  if Assigned(o) then',
    '    o.Free;',
    '  for i := 1 to 3 do',
    '  begin',
    '    n := n + i;',
    '    Exit;',
    '  end;',
    'end;',
    'end.');

  // The converse fixture: the three new shapes, plus a loop and a Free so the
  // two existing rules have a non-empty population to be silent about.
  cIfShapeOverlapNew: array[0..26] of string = (
    'unit overlapnew;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'procedure P(o: TObject; a, b, c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(o: TObject; a, b, c: Boolean);',
    'var',
    '  i: Integer;',
    'begin',
    '  if c then',
    '    Exit',
    '  else',
    '    DoIt;',
    '  for i := 1 to 3 do',
    '    if a then',
    '      if b then',
    '        DoIt;',
    '  if not c then',
    '    DoIt',
    '  else',
    '    o.Free;',
    'end;',
    'end.');

  // One unparseable fixture carrying all three shapes: the class declaration is
  // missing its closing parenthesis.
  cIfShapeUnparseable: array[0..27] of string = (
    'unit Broken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBag = class(TObject',
    '  public',
    '    Count: Integer;',
    '  end;',
    'procedure P(a, b, c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P(a, b, c: Boolean);',
    'begin',
    '  if c then',
    '    Exit',
    '  else',
    '    DoIt;',
    '  if a then',
    '    if b then',
    '      DoIt;',
    '  if not c then',
    '    DoIt',
    '  else',
    '    DoIt;',
    'end;',
    'end.');

  cSwitchOnBooleanExpressionNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(aFlag: Boolean);',
    'implementation',
    'procedure P(aFlag: Boolean);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  case aFlag of',
    '    True:  n := 1;',
    '    False: n := 2;',
    '  end;',
    'end;',
    'end.');

  cSwitchOnBooleanExpressionCompliant: array[0..27] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TColour = (clRed, clGreen);',
    'procedure P(aColour: TColour; aCount: Integer; aChar: Char);',
    'implementation',
    'procedure P(aColour: TColour; aCount: Integer; aChar: Char);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  case aColour of',
    '    clRed:   n := 1;',
    '    clGreen: n := 2;',
    '  end;',
    '  case aCount of',
    '    0: n := 3;',
    '  else',
    '    n := 4;',
    '  end;',
    '  case aChar of',
    '    ''a'': n := 5;',
    '  else',
    '    n := 6;',
    '  end;',
    'end;',
    'end.');

  cSwitchOnBooleanExpressionVariants: array[0..24] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function IsReady: Boolean;',
    'procedure P(aWide: LongBool);',
    'implementation',
    'function IsReady: Boolean;',
    'begin',
    '  Result := True;',
    'end;',
    'procedure P(aWide: LongBool);',
    'var',
    '  n: Integer;',
    'begin',
    '  n := 0;',
    '  case IsReady of',
    '    True:  n := 1;',
    '    False: n := 2;',
    '  end;',
    '  case aWide of',
    '    True:  n := 3;',
    '    False: n := 4;',
    '  end;',
    'end;',
    'end.');

  // The selector on line 17 is typed by the uninstantiated generic parameter T,
  // which is outside the analysed closure; line 21 is the positive control.
  cSwitchOnBooleanExpressionUnresolvedSelector: array[0..25] of string = (
    'unit unresolved;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  generic TBox<T> = class',
    '  public',
    '    procedure Watch(aFlag: T);',
    '  end;',
    'implementation',
    'procedure TBox.Watch(aFlag: T);',
    'var',
    '  n: Integer;',
    '  lLocal: Boolean;',
    'begin',
    '  n := 0;',
    '  lLocal := True;',
    '  case aFlag of',
    '    True:  n := 1;',
    '    False: n := 2;',
    '  end;',
    '  case lLocal of',
    '    True:  n := 3;',
    '    False: n := 4;',
    '  end;',
    'end;',
    'end.');

  // Both shapes in a fixture the vendored resolver rejects on the repeated case
  // label.
  cSelectorAndLoopUnresolvable: array[0..26] of string = (
    'unit broken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(aFlag: Boolean; i: Integer);',
    'implementation',
    'procedure Note(aValue: Integer);',
    'begin',
    'end;',
    'procedure P(aFlag: Boolean; i: Integer);',
    'var',
    '  lDone: Boolean;',
    '  n: Integer;',
    'begin',
    '  lDone := aFlag;',
    '  n := 0;',
    '  case aFlag of',
    '    True:  n := 1;',
    '    False: n := 2;',
    '  end;',
    '  while not lDone do',
    '    Note(1);',
    '  case i of',
    '    1: n := 3;',
    '    1: n := 4;',
    '  end;',
    'end;',
    'end.');

  cLoopConditionNeverChangesNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(aReady: Boolean);',
    'implementation',
    'procedure Note(aValue: Integer);',
    'begin',
    'end;',
    'procedure P(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    Note(1);',
    'end;',
    'end.');

  // One routine per indirect way the condition can change, plus every shape the
  // gates refuse to read: a call, a member access, an address, a dereference,
  // an index, a procedural name, a var parameter, a literal and a field.
  cLoopConditionNeverChangesCompliant: array[0..227] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  TFlag = record',
    '    Done: Boolean;',
    '  end;',
    '  TFlagPtr = ^Boolean;',
    '  TPred = function: Boolean;',
    '  TGate = class',
    '  private',
    '    FDone: Boolean;',
    '  public',
    '    procedure Watch;',
    '  end;',
    'function Pending(aValue: Integer): Integer;',
    'procedure Fetch(var aDone: Boolean);',
    'procedure Assigns(aReady: Boolean);',
    'procedure Increments;',
    'procedure VarArg(aReady: Boolean);',
    'procedure AddressOf(aReady: Boolean);',
    'procedure ForControl;',
    'procedure Breaks(aReady: Boolean);',
    'procedure Exits(aReady: Boolean);',
    'procedure QualifiedExits(aReady: Boolean);',
    'procedure Raises(aReady: Boolean);',
    'procedure AsmEscape(aReady: Boolean);',
    'procedure NestedCall(aReady: Boolean);',
    'procedure SiblingCall(aReady: Boolean);',
    'procedure CallCondition;',
    'procedure MemberCondition(const aFlag: TFlag);',
    'procedure WithCondition(var aRec: TFlag);',
    'procedure AddressCondition(aReady: Boolean);',
    'procedure DerefCondition(aPtr: TFlagPtr);',
    'procedure IndexCondition;',
    'procedure ProcTypeCondition(aPred: TPred);',
    'procedure VarParamCondition(var aDone: Boolean);',
    'procedure AlwaysTrue;',
    'procedure NoVariable;',
    'implementation',
    'procedure Note(aValue: Integer);',
    'begin',
    'end;',
    'function Pending(aValue: Integer): Integer;',
    'begin',
    '  Result := aValue;',
    'end;',
    'procedure Fetch(var aDone: Boolean);',
    'begin',
    '  aDone := True;',
    'end;',
    'procedure Assigns(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    lDone := True;',
    'end;',
    'procedure Increments;',
    'var',
    '  i: Integer;',
    'begin',
    '  i := 0;',
    '  while i < 3 do',
    '    Inc(i);',
    'end;',
    'procedure VarArg(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    Fetch(lDone);',
    'end;',
    'procedure AddressOf(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    '  lPtr: Pointer;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    lPtr := @lDone;',
    'end;',
    'procedure ForControl;',
    'var',
    '  i: Integer;',
    'begin',
    '  i := 0;',
    '  while i < 3 do',
    '    for i := 1 to 3 do',
    '      Note(i);',
    'end;',
    'procedure Breaks(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    Break;',
    'end;',
    'procedure Exits(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    Exit;',
    'end;',
    'procedure QualifiedExits(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    System.Exit;',
    'end;',
    'procedure Raises(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    raise Exception.Create(''stop'');',
    'end;',
    'procedure AsmEscape(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    asm',
    '      nop',
    '    end;',
    'end;',
    'procedure NestedCall(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    '  procedure Flip;',
    '  begin',
    '    lDone := True;',
    '  end;',
    'begin',
    '  lDone := aReady;',
    '  while not lDone do',
    '    Flip;',
    'end;',
    'procedure SiblingCall(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    '  procedure Flip;',
    '  begin',
    '    lDone := True;',
    '  end;',
    '  procedure Spin;',
    '  begin',
    '    while not lDone do',
    '      Flip;',
    '  end;',
    'begin',
    '  lDone := aReady;',
    '  Spin;',
    'end;',
    'procedure CallCondition;',
    'begin',
    '  while Pending(1) > 0 do',
    '    Note(1);',
    'end;',
    'procedure MemberCondition(const aFlag: TFlag);',
    'begin',
    '  while not aFlag.Done do',
    '    Note(1);',
    'end;',
    'procedure WithCondition(var aRec: TFlag);',
    'begin',
    '  with aRec do',
    '    while not Done do',
    '      Note(1);',
    'end;',
    'procedure AddressCondition(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while @lDone <> nil do',
    '    Note(1);',
    'end;',
    'procedure DerefCondition(aPtr: TFlagPtr);',
    'begin',
    '  while not aPtr^ do',
    '    Note(1);',
    'end;',
    'procedure IndexCondition;',
    'var',
    '  lFlags: array[0..1] of Boolean;',
    'begin',
    '  lFlags[0] := False;',
    '  while lFlags[0] do',
    '    Note(1);',
    'end;',
    'procedure ProcTypeCondition(aPred: TPred);',
    'begin',
    '  while not aPred do',
    '    Note(1);',
    'end;',
    'procedure VarParamCondition(var aDone: Boolean);',
    'begin',
    '  while not aDone do',
    '    Note(1);',
    'end;',
    'procedure AlwaysTrue;',
    'begin',
    '  while True do',
    '    Note(1);',
    'end;',
    'procedure NoVariable;',
    'begin',
    '  repeat',
    '    Note(1);',
    '  until 1 > 2;',
    'end;',
    'procedure TGate.Watch;',
    'begin',
    '  while not FDone do',
    '    Note(1);',
    'end;',
    'end.');

  cLoopConditionNeverChangesVariants: array[0..21] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P(aReady: Boolean);',
    'implementation',
    'procedure Note(aValue: Integer);',
    'begin',
    'end;',
    'procedure P(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    '  i: Integer;',
    'begin',
    '  lDone := aReady;',
    '  i := 0;',
    '  repeat',
    '    Note(1);',
    '  until lDone;',
    '  while i < 3 do',
    '    Note(2);',
    'end;',
    'end.');

  // The condition name on line 19 is declared in Classes, a unit resolved from
  // a stub and never analysed; line 21 is the positive control.
  cLoopConditionNeverChangesUnresolvedName: array[0..23] of string = (
    'unit unresolved;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Classes;',
    'type',
    '  TSortGate = class(TStringList)',
    '  public',
    '    procedure Watch(aReady: Boolean);',
    '  end;',
    'implementation',
    'procedure Note(aValue: Integer);',
    'begin',
    'end;',
    'procedure TSortGate.Watch(aReady: Boolean);',
    'var',
    '  lDone: Boolean;',
    'begin',
    '  lDone := aReady;',
    '  while not Sorted do',
    '    Note(1);',
    '  while not lDone do',
    '    Note(2);',
    'end;',
    'end.');

  cUnreachableCodeNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure P;',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P;',
    'begin',
    '  Exit;',
    '  DoIt;',
    'end;',
    'end.');

  // One routine per way the shape can look present without being it.
  cUnreachableCodeCompliant: array[0..75] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}{$goto on}',
    'interface',
    'procedure Last;',
    'procedure Guarded(c: Boolean);',
    'procedure WithElse(c: Boolean);',
    'procedure Labelled(c: Boolean);',
    'procedure NestedLabel(c: Boolean);',
    'procedure CaseArms(n: Integer);',
    'procedure TryFinally;',
    'procedure ConstantCondition;',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure Last;',
    'begin',
    '  DoIt;',
    '  Exit;',
    'end;',
    'procedure Guarded(c: Boolean);',
    'begin',
    '  if c then',
    '    Exit;',
    '  DoIt;',
    'end;',
    'procedure WithElse(c: Boolean);',
    'begin',
    '  if c then',
    '    Exit',
    '  else',
    '    DoIt;',
    'end;',
    'procedure Labelled(c: Boolean);',
    'label',
    '  L;',
    'begin',
    '  if c then',
    '    goto L;',
    '  Exit;',
    'L:',
    '  DoIt;',
    'end;',
    'procedure NestedLabel(c: Boolean);',
    'label',
    '  M;',
    'begin',
    '  if c then',
    '    goto M;',
    '  Exit;',
    '  begin',
    'M:',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure CaseArms(n: Integer);',
    'begin',
    '  case n of',
    '    1: Exit;',
    '    2: DoIt;',
    '  end;',
    'end;',
    'procedure TryFinally;',
    'begin',
    '  try',
    '    Exit;',
    '  finally',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure ConstantCondition;',
    'begin',
    '  if False then',
    '    DoIt;',
    'end;',
    'end.');

  // The three routines whose dead statement sits in the routine's own list come
  // first.
  cUnreachableCodeVariants: array[0..93] of string = (
    'unit variants;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  EFoo = class(TObject);',
    'procedure RaiseFirst;',
    'procedure HaltFirst;',
    'function ExitValue: Integer;',
    'procedure ForBreak;',
    'procedure WhileContinue(c: Boolean);',
    'procedure RepeatBody(c: Boolean);',
    'procedure TryBody;',
    'procedure CaseElse(n: Integer);',
    'procedure NestedBlock;',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure RaiseFirst;',
    'begin',
    '  raise EFoo.Create;',
    '  DoIt;',
    'end;',
    'procedure HaltFirst;',
    'begin',
    '  Halt(1);',
    '  DoIt;',
    'end;',
    'function ExitValue: Integer;',
    'begin',
    '  Result := 0;',
    '  Exit(0);',
    '  DoIt;',
    'end;',
    'procedure ForBreak;',
    'var',
    '  i: Integer;',
    'begin',
    '  for i := 1 to 3 do',
    '  begin',
    '    Break;',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure WhileContinue(c: Boolean);',
    'begin',
    '  while c do',
    '  begin',
    '    Continue;',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure RepeatBody(c: Boolean);',
    'begin',
    '  repeat',
    '    Exit;',
    '    DoIt;',
    '  until c;',
    'end;',
    'procedure TryBody;',
    'begin',
    '  try',
    '    Exit;',
    '    DoIt;',
    '  finally',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure CaseElse(n: Integer);',
    'begin',
    '  case n of',
    '    1: DoIt;',
    '  else',
    '    Exit;',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure NestedBlock;',
    'begin',
    '  begin',
    '    Exit;',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure TwoTerminators;',
    'begin',
    '  begin',
    '    Exit;',
    '    DoIt;',
    '    Exit;',
    '    DoIt;',
    '  end;',
    'end;',
    'end.');

  // One routine per shape that only full CFG reachability sees.
  cUnreachableCodeWidened: array[0..54] of string = (
    'unit widened;',
    '{$mode objfpc}{$H+}{$goto on}',
    'interface',
    'procedure AfterGoto;',
    'procedure AfterIf(c: Boolean);',
    'procedure AfterTry;',
    'procedure AfterCase(n: Integer);',
    'procedure DeadContainer(c: Boolean);',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure AfterGoto;',
    'label',
    '  L;',
    'begin',
    '  goto L;',
    '  DoIt;',
    'L:',
    '  DoIt;',
    'end;',
    'procedure AfterIf(c: Boolean);',
    'begin',
    '  if c then',
    '    Exit',
    '  else',
    '    Exit;',
    '  DoIt;',
    'end;',
    'procedure AfterTry;',
    'begin',
    '  try',
    '    Exit;',
    '  except',
    '    raise;',
    '  end;',
    '  DoIt;',
    'end;',
    'procedure AfterCase(n: Integer);',
    'begin',
    '  case n of',
    '    1: Exit;',
    '    2: Exit;',
    '  else',
    '    Exit;',
    '  end;',
    '  DoIt;',
    'end;',
    'procedure DeadContainer(c: Boolean);',
    'begin',
    '  Exit;',
    '  if c then',
    '    DoIt;',
    'end;',
    'end.');

  // One routine per shape the graph places in an unreached node although the
  // code runs, or reports twice without the dead-run rule, plus one genuinely
  // dead statement so a fixture that stops parsing cannot pass the test.
  cUnreachableCodeGuards: array[0..42] of string = (
    'unit guards;',
    '{$mode objfpc}{$H+}{$goto on}',
    'interface',
    'procedure DoOnce;',
    'procedure GotoIntoBlock(c: Boolean);',
    'procedure EmptyProtected;',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure DoOnce;',
    'begin',
    '  DoIt;',
    '  repeat',
    '    DoIt;',
    '    Break;',
    '  until False;',
    'end;',
    'procedure GotoIntoBlock(c: Boolean);',
    'label',
    '  M;',
    'begin',
    '  if c then',
    '    goto M;',
    '  Exit;',
    '  begin',
    'M:',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure EmptyProtected;',
    'begin',
    '  try',
    '  except',
    '    DoIt;',
    '  end;',
    'end;',
    'procedure Control;',
    'begin',
    '  Exit;',
    '  DoIt;',
    'end;',
    'end.');

  // The class declaration is missing its closing parenthesis.
  cUnreachableCodeUnparseable: array[0..18] of string = (
    'unit Broken;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TBag = class(TObject',
    '  public',
    '    Count: Integer;',
    '  end;',
    'procedure P;',
    'implementation',
    'procedure DoIt;',
    'begin',
    'end;',
    'procedure P;',
    'begin',
    '  Exit;',
    '  DoIt;',
    'end;',
    'end.');

procedure TRulesControlTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesControlTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesControlTest.FirstById(
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


function TRulesControlTest.NewExhaustiveCaseStatement: TRuleBase;

begin
  Result := TRuleExhaustiveCaseStatement.Create(TRuleMetadata.Make(
    cExhaustiveCaseStatementId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewExceptionRaised: TRuleBase;

begin
  Result := TRuleExceptionRaised.Create(TRuleMetadata.Make(
    cExceptionRaisedId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewSingleIterationLoop: TRuleBase;

begin
  Result := TRuleSingleIterationLoop.Create(TRuleMetadata.Make(
    cSingleIterationLoopId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewNoPascalStyleResultAssignment: TRuleBase;

begin
  Result := TRuleNoPascalStyleResultAssignment.Create(TRuleMetadata.Make(
    cNoPascalStyleResultAssignmentId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesControlTest.NewRedundantAssignedCheckBeforeFree: TRuleBase;

begin
  Result := TRuleRedundantAssignedCheckBeforeFree.Create(TRuleMetadata.Make(
    cRedundantAssignedCheckBeforeFreeId, rtSem, rfResolver, sevMinor,
    itCodeSmell, cfHigh, True, ''));
end;


function TRulesControlTest.NewLoopBeyondCollectionEnd: TRuleBase;

begin
  Result := TRuleLoopBeyondCollectionEnd.Create(TRuleMetadata.Make(
    cLoopBeyondCollectionEndId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewRoutineResultAssigned: TRuleBase;

begin
  Result := TRuleRoutineResultAssigned.Create(TRuleMetadata.Make(
    cRoutineResultAssignedId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewNoCatchRawException: TRuleBase;

begin
  Result := TRuleNoCatchRawException.Create(TRuleMetadata.Make(
    cNoCatchRawExceptionId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewNoRaiseRawException: TRuleBase;

begin
  Result := TRuleNoRaiseRawException.Create(TRuleMetadata.Make(
    cNoRaiseRawExceptionId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewIdenticalBranches: TRuleBase;

begin
  Result := TRuleIdenticalBranches.Create(TRuleMetadata.Make(
    cIdenticalBranchesId, rtAst, rfAst, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewDuplicateConditionInChain: TRuleBase;

begin
  Result := TRuleDuplicateConditionInChain.Create(TRuleMetadata.Make(
    cDuplicateConditionInChainId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewDuplicateCaseLabel: TRuleBase;

begin
  Result := TRuleDuplicateCaseLabel.Create(TRuleMetadata.Make(
    cDuplicateCaseLabelId, rtAst, rfAst, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewSelfComparison: TRuleBase;

begin
  Result := TRuleSelfComparison.Create(TRuleMetadata.Make(
    cSelfComparisonId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewEmptyThenWithFollowingStatement: TRuleBase;

begin
  Result := TRuleEmptyThenWithFollowingStatement.Create(TRuleMetadata.Make(
    cEmptyThenWithFollowingStatementId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewMixedBooleanAndRelational: TRuleBase;

begin
  Result := TRuleMixedBooleanAndRelational.Create(TRuleMetadata.Make(
    cMixedBooleanAndRelationalId, rtTok, rfTokenStream, sevMinor, itCodeSmell,
    cfMedium, True, ''));
end;


function TRulesControlTest.NewBitwiseOnBooleanOperands: TRuleBase;

begin
  Result := TRuleBitwiseOnBooleanOperands.Create(TRuleMetadata.Make(
    cBitwiseOnBooleanOperandsId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewAssignmentInsteadOfComparison: TRuleBase;

begin
  Result := TRuleAssignmentInsteadOfComparison.Create(TRuleMetadata.Make(
    cAssignmentInsteadOfComparisonId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewConditionWithSideEffect: TRuleBase;

begin
  Result := TRuleConditionWithSideEffect.Create(TRuleMetadata.Make(
    cConditionWithSideEffectId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewRedundantElseAfterExit: TRuleBase;

begin
  Result := TRuleRedundantElseAfterExit.Create(TRuleMetadata.Make(
    cRedundantElseAfterExitId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewCollapsibleNestedIf: TRuleBase;

begin
  Result := TRuleCollapsibleNestedIf.Create(TRuleMetadata.Make(
    cCollapsibleNestedIfId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewNegatedConditionWithElse: TRuleBase;

begin
  Result := TRuleNegatedConditionWithElse.Create(TRuleMetadata.Make(
    cNegatedConditionWithElseId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesControlTest.NewSwitchOnBooleanExpression: TRuleBase;

begin
  Result := TRuleSwitchOnBooleanExpression.Create(TRuleMetadata.Make(
    cSwitchOnBooleanExpressionId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesControlTest.NewLoopConditionNeverChanges: TRuleBase;

begin
  Result := TRuleLoopConditionNeverChanges.Create(TRuleMetadata.Make(
    cLoopConditionNeverChangesId, rtSem, rfResolver, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesControlTest.NewUnreachableCode: TRuleBase;

begin
  Result := TRuleUnreachableCode.Create(TRuleMetadata.Make(
    cUnreachableCodeId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


procedure TRulesControlTest.CheckControlRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: one issue at the construct line, column 1, carrying aArgs
    // as the message args.
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

    // Compliant: the FP guards stay silent. A fixture that failed to stage or
    // to parse yields the same zero.
    lPath := lFix.Add('compliant.pas', aCompliant);
    AssertFixtureParses(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lPath, lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.CheckControlTokenRuleSrc(aRule,
  aCompliantRule: TRuleBase; const aId: string; aDeclLine, aDeclCol: Integer;
  const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', aDeclCol, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', aDeclCol, lc.Issues[k].EndCol);
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

    lPath := lFix.Add('compliant.pas', aCompliant);
    AssertFixtureParses(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lPath, lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.ExhaustiveCaseStatementPositions;

begin
  // Noncompliant: 'case c of' over TColor omitting cBlue, no else (case keyword
  // line 12, probe-locked); arg is the joined missing-value names. Compliant: the
  // same case covering all three values => silent.
  CheckControlRuleSrc(NewExhaustiveCaseStatement, NewExhaustiveCaseStatement,
    cExhaustiveCaseStatementId, 12, ['cBlue'],
    cExhaustiveCaseStatementNoncompliant, cExhaustiveCaseStatementCompliant);
end;


procedure TRulesControlTest.ExceptionRaisedPositions;

begin
  // Noncompliant: 'EBadValue.Create('bad value');' as a bare statement
  // (construction line 11, probe-locked); arg is the exception class name.
  CheckControlRuleSrc(NewExceptionRaised, NewExceptionRaised,
    cExceptionRaisedId, 11, ['EBadValue'],
    cExceptionRaisedNoncompliant, cExceptionRaisedCompliant);
end;


procedure TRulesControlTest.SingleIterationLoopPositions;

begin
  // Noncompliant: 'while not done do begin n := n + 1; break; end;' (while
  // keyword line 13, probe-locked); no message args.
  CheckControlRuleSrc(NewSingleIterationLoop, NewSingleIterationLoop,
    cSingleIterationLoopId, 13, [],
    cSingleIterationLoopNoncompliant, cSingleIterationLoopCompliant);
end;


procedure TRulesControlTest.NoPascalStyleResultAssignmentPositions;

begin
  // Noncompliant: 'Add := a + b;', the legacy function-name return (assignment
  // LHS line 8, probe-locked); arg is the function name.
  CheckControlRuleSrc(NewNoPascalStyleResultAssignment,
    NewNoPascalStyleResultAssignment, cNoPascalStyleResultAssignmentId, 8,
    ['Add'],
    cNoPascalStyleResultAssignmentNoncompliant, cNoPascalStyleResultAssignmentCompliant);
end;


procedure TRulesControlTest.NoPascalStyleResultAssignmentMethodFires;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // A class method's TPasProcedure.Name is dotted ('TBar.Calc') while the
  // source LHS uses the bare member name ('Calc'); method.pas line 14. The arg
  // is the qualified routine name.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewNoPascalStyleResultAssignment,
        lFix.Add('method.pas', cNoPascalStyleResultAssignmentMethod), lc);
      AssertEquals('method name-style return fires once', 1,
        CountById(lc, cNoPascalStyleResultAssignmentId));
      k := FirstById(lc, cNoPascalStyleResultAssignmentId);
      AssertEquals('method start line', 14, lc.Issues[k].StartLine);
      AssertEquals('method arg is the qualified routine name', 'TBar.Calc',
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.RedundantAssignedCheckBeforeFreePositions;

begin
  // Noncompliant: 'if Assigned(o) then' newline 'o.Free;' with no else
  // (if-condition line 9, probe-locked); no message args.
  CheckControlRuleSrc(NewRedundantAssignedCheckBeforeFree,
    NewRedundantAssignedCheckBeforeFree, cRedundantAssignedCheckBeforeFreeId, 9,
    [],
    cRedundantAssignedCheckBeforeFreeNoncompliant, cRedundantAssignedCheckBeforeFreeCompliant);
end;


procedure TRulesControlTest.LoopBeyondCollectionEndPositions;

begin
  // Noncompliant: 'for i := 0 to Length(A) do' over 'A: array[0..9] of
  // Integer'; Length(A)=10 reaches index 10, one past High(A)=9 (the
  // overrunning index node A[i] on line 12, probe-locked); no message args.
  CheckControlRuleSrc(NewLoopBeyondCollectionEnd, NewLoopBeyondCollectionEnd,
    cLoopBeyondCollectionEndId, 12, [],
    cLoopBeyondCollectionEndNoncompliant, cLoopBeyondCollectionEndCompliant);
end;


procedure TRulesControlTest.RoutineResultAssignedPositions;

begin
  // Noncompliant: 'function Pick(b: Boolean): Integer;' whose body is just
  // 'if b then Result := 1;' with no else; the b=False path falls off 'end'
  // with no result write (function declaration row 11, probe-locked); no
  // message args.
  CheckControlRuleSrc(NewRoutineResultAssigned, NewRoutineResultAssigned,
    cRoutineResultAssignedId, 11, [],
    cRoutineResultAssignedNoncompliant, cRoutineResultAssignedCompliant);
end;


procedure TRulesControlTest.RoutineResultAssignedReportsCaseWithoutElse;

begin
  // Every case branch writes the result and there is no else.
  CheckResultAssignedSoleRow('cases.pas', cRoutineResultAssignedCaseNoElse, 6);
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnReRaisingExcept;

begin
  // A result written inside a try's protected region escapes and carries no
  // slot at all (DW-618).
  CheckResultAssignedSoleRow('reraise.pas', cRoutineResultAssignedReRaise, 15);
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnTerminatingPath;

begin
  // raise, exit(value) and Halt each end the routine without an unassigned edge
  // to the exit node. The sibling at row 28 is the live control.
  CheckResultAssignedSoleRow('terminators.pas',
    cRoutineResultAssignedTerminators, 28);
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnManagedResult;

begin
  // A string result is outside the type kinds a slot may stand for.
  CheckResultAssignedSoleRow('managed.pas', cRoutineResultAssignedManaged, 10);
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnNestedRoutineWrite;

begin
  // The nested routine's mention puts the result in the escape set and the slot
  // is dropped whole. The sibling at row 17 is the live control.
  CheckResultAssignedSoleRow('nested.pas',
    cRoutineResultAssignedNestedWrite, 17);
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnExitValueInLoop;

begin
  // Exit(i) is the only write and it sits in a loop body, whose zero-trip edge
  // reaches the exit unassigned.
  CheckResultAssignedSoleRow('exitinloop.pas',
    cRoutineResultAssignedExitInLoop, 15);
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnOperatorAndAssembler;

begin
  // An operator carries no result slot though its Boolean result is a tracked
  // kind, and neither does an assembler body. The sibling at row 18 is the
  // live control.
  CheckResultAssignedSoleRow('shapes.pas',
    cRoutineResultAssignedOperatorAndAsm, 18);
end;


procedure TRulesControlTest.RoutineResultAssignedDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  // The fixture does not parse, so no module reaches the resolver and the
  // query is False rather than an empty answer.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cOperatorTrapsUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRoutineResultAssigned, lPath, lc);
      AssertEquals('no resolver => silent', 0,
        CountById(lc, cRoutineResultAssignedId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.RoutineResultAssignedSilentOnUnresolvedOperand;

begin
  // The resolver is live and the fixture resolves, but the asm statement is one
  // no statement-access answer covers.
  CheckResultAssignedSoleRow('unanswerable.pas',
    cRoutineResultAssignedUnanswerable, 15);
end;


procedure TRulesControlTest.NoCatchRawExceptionPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // Noncompliant: 'on E: Exception do DoWork;'; the caught type alias-resolves
  // to the root Exception (on-handler line 22, probe-locked); no message args.
  CheckControlRuleSrc(NewNoCatchRawException, NewNoCatchRawException,
    cNoCatchRawExceptionId, 22, [],
    cNoCatchRawExceptionNoncompliant, cNoCatchRawExceptionCompliant);

  // The bare catch-all branch fires positively: a non-empty 'except DoWork; end'
  // with no 'on' and no re-raise swallows everything (the except keyword line 21,
  // probe-locked). Folded here to keep the suite at +2 published tests.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewNoCatchRawException,
        lFix.Add('bareexcept.pas', cNoCatchRawExceptionBareExcept), lc);
      AssertEquals('bare catch-all fires once', 1,
        CountById(lc, cNoCatchRawExceptionId));
      AssertEquals('bare catch-all line',
        21, lc.Issues[FirstById(lc, cNoCatchRawExceptionId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.NoRaiseRawExceptionPositions;

begin
  // Noncompliant: 'raise Exception.Create('something failed');'; the
  // constructed class is exactly the root Exception (raised-expr line 15,
  // probe-locked); no message args.
  CheckControlRuleSrc(NewNoRaiseRawException, NewNoRaiseRawException,
    cNoRaiseRawExceptionId, 15, [],
    cNoRaiseRawExceptionNoncompliant, cNoRaiseRawExceptionCompliant);
end;


procedure TRulesControlTest.IdenticalBranchesPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // Noncompliant: 'if c then x := 1 else x := 1', two structurally identical
  // assignments (the if keyword line 10, probe-locked); the arg is the
  // condition text.
  CheckControlRuleSrc(NewIdenticalBranches, NewIdenticalBranches,
    cIdenticalBranchesId, 10, ['c'],
    cIdenticalBranchesNoncompliant, cIdenticalBranchesCompliant);

  // The begin-block form fires as well: element-wise identical two-statement
  // blocks (the if keyword line 10, probe-locked).
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewIdenticalBranches,
        lFix.Add('blocks.pas', cIdenticalBranchesBlocks), lc);
      AssertEquals('identical begin blocks fire once', 1,
        CountById(lc, cIdenticalBranchesId));
      AssertEquals('identical begin blocks line',
        10, lc.Issues[FirstById(lc, cIdenticalBranchesId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.IdenticalBranchesDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cDuplicateShapesUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewIdenticalBranches, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cIdenticalBranchesId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.DuplicateConditionInChainPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // Noncompliant: 'if a > 1 ... else if a > 1'; the second link repeats the
  // first link's condition text (the repeated condition's own line 12,
  // probe-locked); the arg is that text.
  CheckControlRuleSrc(NewDuplicateConditionInChain,
    NewDuplicateConditionInChain, cDuplicateConditionInChainId, 12, ['a > 1'],
    cDuplicateConditionInChainNoncompliant,
    cDuplicateConditionInChainCompliant);

  // A three-link chain whose first condition returns as the last: one issue, at
  // the third link (line 14, probe-locked).
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDuplicateConditionInChain,
        lFix.Add('chainofthree.pas', cDuplicateConditionInChainOfThree), lc);
      AssertEquals('three-link chain fires once', 1,
        CountById(lc, cDuplicateConditionInChainId));
      AssertEquals('third link line',
        14, lc.Issues[FirstById(lc, cDuplicateConditionInChainId)].StartLine);
      AssertEquals('third link arg', 'a',
        lc.Issues[FirstById(lc,
        cDuplicateConditionInChainId)].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;

  // A repeat between links TWO and THREE: the shape that discriminates the
  // chain-head guard, since only a re-walk from link 2 emits a second issue.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDuplicateConditionInChain,
        lFix.Add('innerrepeat.pas', cDuplicateConditionInChainInnerRepeat), lc);
      AssertEquals('an inner-link repeat fires once, not twice', 1,
        CountById(lc, cDuplicateConditionInChainId));
      AssertEquals('inner-link repeat line',
        14, lc.Issues[FirstById(lc, cDuplicateConditionInChainId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.DuplicateConditionInChainDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cDuplicateShapesUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDuplicateConditionInChain, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cDuplicateConditionInChainId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.DuplicateCaseLabelPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // Noncompliant: 'case i of 1: ...; 1: ...; end' (the repeated label's own
  // line 12, probe-locked); the arg is the label text.
  CheckControlRuleSrc(NewDuplicateCaseLabel, NewDuplicateCaseLabel,
    cDuplicateCaseLabelId, 12, ['1'],
    cDuplicateCaseLabelNoncompliant, cDuplicateCaseLabelCompliant);

  // The noncompliant fixture above parses but does not resolve: the vendored
  // resolver raises nDuplicateCaseValueXatY on it, which is why the rule is
  // AST tier.
  lFix := TTempFixtures.Create;
  try
    AssertFixtureParsesWithoutResolving(
      lFix.Add('noncompliant.pas', cDuplicateCaseLabelNoncompliant));
  finally
    lFix.Free;
  end;

  // The positive control: a case statement with distinct labels does resolve.
  // This rule's own compliant fixture cannot play the part -- its overlapping
  // 1..5 / 3 pair trips the same resolver check.
  lFix := TTempFixtures.Create;
  try
    AssertFixtureResolves(
      lFix.Add('compliant.pas', cExhaustiveCaseStatementCompliant));
  finally
    lFix.Free;
  end;

  // Two labels in ONE branch list: one issue, on their shared line 11.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDuplicateCaseLabel,
        lFix.Add('commalabels.pas', cDuplicateCaseLabelCommaForm), lc);
      AssertEquals('comma label list fires once', 1,
        CountById(lc, cDuplicateCaseLabelId));
      AssertEquals('comma label line',
        11, lc.Issues[FirstById(lc, cDuplicateCaseLabelId)].StartLine);
      AssertEquals('comma label arg', '2',
        lc.Issues[FirstById(lc, cDuplicateCaseLabelId)].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;

  // An identical range repeated: one issue at the second range (line 12).
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDuplicateCaseLabel,
        lFix.Add('rangelabels.pas', cDuplicateCaseLabelRangeForm), lc);
      AssertEquals('repeated range fires once', 1,
        CountById(lc, cDuplicateCaseLabelId));
      AssertEquals('repeated range line',
        12, lc.Issues[FirstById(lc, cDuplicateCaseLabelId)].StartLine);
      AssertEquals('repeated range arg', '1..3',
        lc.Issues[FirstById(lc, cDuplicateCaseLabelId)].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.DuplicateCaseLabelDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cDuplicateShapesUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewDuplicateCaseLabel, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cDuplicateCaseLabelId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.SelfComparisonPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if x = x then' (the comparison's line 11, probe-locked); the
  // args are the operand text and the operator spelling.
  CheckControlRuleSrc(NewSelfComparison, NewSelfComparison,
    cSelfComparisonId, 11, ['x', '='],
    cSelfComparisonNoncompliant, cSelfComparisonCompliant);

  // The ordering operators count too, and a member-access chain is one operand:
  // 'b := a.Count <= a.Count' on line 14 (probe-locked).
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewSelfComparison,
        lFix.Add('ordering.pas', cSelfComparisonOrdering), lc);
      AssertEquals('ordering comparison fires once', 1,
        CountById(lc, cSelfComparisonId));
      k := FirstById(lc, cSelfComparisonId);
      AssertEquals('ordering comparison line', 14, lc.Issues[k].StartLine);
      AssertEquals('ordering operand arg', 'a.Count',
        lc.Issues[k].MessageArgs[0]);
      AssertEquals('ordering operator arg', '<=',
        lc.Issues[k].MessageArgs[1]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.SelfComparisonDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cDuplicateShapesUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewSelfComparison, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cSelfComparisonId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.EmptyThenWithFollowingStatementPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if c then ;' with 'x := 1;' as its next sibling (the if
  // keyword line 10, probe-locked); the arg is the condition text.
  CheckControlRuleSrc(NewEmptyThenWithFollowingStatement,
    NewEmptyThenWithFollowingStatement, cEmptyThenWithFollowingStatementId, 10,
    ['c'],
    cEmptyThenWithFollowingStatementNoncompliant,
    cEmptyThenWithFollowingStatementCompliant);

  // A repeat body and a try body are the two sibling statement lists that are
  // neither a routine root nor a begin-block; both must still be reached.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewEmptyThenWithFollowingStatement,
        lFix.Add('nestedblocks.pas', cEmptyThenInNestedBlocks), lc);
      AssertEquals('repeat body and try body both fire', 2,
        CountById(lc, cEmptyThenWithFollowingStatementId));
      k := FirstById(lc, cEmptyThenWithFollowingStatementId);
      AssertEquals('repeat-body line', 12, lc.Issues[k].StartLine);
      AssertEquals('try-body line', 16, lc.Issues[k + 1].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.EmptyThenWithFollowingStatementDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cDuplicateShapesUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewEmptyThenWithFollowingStatement, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cEmptyThenWithFollowingStatementId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.MixedBooleanAndRelationalPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if a and b = c then' (the 'and' keyword at line 11 column 8,
  // probe-locked); the args are the boolean operator and the comparison.
  CheckControlTokenRuleSrc(NewMixedBooleanAndRelational,
    NewMixedBooleanAndRelational, cMixedBooleanAndRelationalId, 11, 8,
    ['and', '='],
    cMixedBooleanAndRelationalNoncompliant, cMixedBooleanAndRelationalCompliant);

  // The remaining positive rows: the comparison-first order, a loop condition,
  // two pairings in one region (which must report twice, not once) and the 'xor'
  // keyword against the two-character '<>'.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewMixedBooleanAndRelational,
        lFix.Add('variants.pas', cMixedBooleanAndRelationalVariants), lc);
      AssertEquals('five pairings', 5,
        CountById(lc, cMixedBooleanAndRelationalId));
      k := FirstById(lc, cMixedBooleanAndRelationalId);
      AssertEquals('comparison first row', 11, lc.Issues[k].StartLine);
      AssertEquals('comparison first col', 12, lc.Issues[k].StartCol);
      AssertEquals('loop condition row', 13, lc.Issues[k + 1].StartLine);
      AssertEquals('loop condition col', 15, lc.Issues[k + 1].StartCol);
      AssertEquals('loop condition operator arg', '<',
        lc.Issues[k + 1].MessageArgs[1]);
      AssertEquals('first pairing row', 15, lc.Issues[k + 2].StartLine);
      AssertEquals('first pairing col', 8, lc.Issues[k + 2].StartCol);
      AssertEquals('second pairing row', 15, lc.Issues[k + 3].StartLine);
      AssertEquals('second pairing col', 18, lc.Issues[k + 3].StartCol);
      AssertEquals('xor pairing row', 17, lc.Issues[k + 4].StartLine);
      AssertEquals('xor pairing col', 8, lc.Issues[k + 4].StartCol);
      AssertEquals('xor pairing boolean arg', 'xor',
        lc.Issues[k + 4].MessageArgs[0]);
      AssertEquals('xor pairing comparison arg', '<>',
        lc.Issues[k + 4].MessageArgs[1]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.MixedBooleanAndRelationalRunsWithoutParse;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k: Integer;

begin
  // The parse failure is pinned first, then the shape at line 21 column 8.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cOperatorTrapsUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewMixedBooleanAndRelational, lPath, lc);
      AssertEquals('reports without a module', 1,
        CountById(lc, cMixedBooleanAndRelationalId));
      k := FirstById(lc, cMixedBooleanAndRelationalId);
      AssertEquals('unparseable row', 21, lc.Issues[k].StartLine);
      AssertEquals('unparseable col', 8, lc.Issues[k].StartCol);
    finally
      lc.Free;
    end;

    // A surplus ')' drives the depth state below its own floor.
    lPath := lFix.Add('surplus.pas', cMixedBooleanAndRelationalSurplusParen);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewMixedBooleanAndRelational, lPath, lc);
      AssertEquals('surplus paren, one pairing', 1,
        CountById(lc, cMixedBooleanAndRelationalId));
      k := FirstById(lc, cMixedBooleanAndRelationalId);
      AssertEquals('surplus paren row', 8, lc.Issues[k].StartLine);
      AssertEquals('surplus paren col', 9, lc.Issues[k].StartCol);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.BitwiseOnBooleanOperandsPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if flags and $10 then' (the expression's line 11,
  // probe-locked); the arg is the operator spelling.
  CheckControlRuleSrc(NewBitwiseOnBooleanOperands, NewBitwiseOnBooleanOperands,
    cBitwiseOnBooleanOperandsId, 11, ['and'],
    cBitwiseOnBooleanOperandsNoncompliant, cBitwiseOnBooleanOperandsCompliant);

  // The operand-kind rows: a comparison against a numeric literal, a boolean
  // literal against one, and a node both shapes match, which reports once.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewBitwiseOnBooleanOperands,
        lFix.Add('variants.pas', cBitwiseOnBooleanOperandsVariants), lc);
      AssertEquals('three mixed-kind nodes', 3,
        CountById(lc, cBitwiseOnBooleanOperandsId));
      k := FirstById(lc, cBitwiseOnBooleanOperandsId);
      AssertEquals('comparison operand row', 11, lc.Issues[k].StartLine);
      AssertEquals('comparison operand arg', 'and',
        lc.Issues[k].MessageArgs[0]);
      AssertEquals('boolean literal row', 12, lc.Issues[k + 1].StartLine);
      AssertEquals('boolean literal arg', 'or', lc.Issues[k + 1].MessageArgs[0]);
      AssertEquals('both shapes row', 13, lc.Issues[k + 2].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.BitwiseOnBooleanOperandsDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cOperatorTrapsUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewBitwiseOnBooleanOperands, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cBitwiseOnBooleanOperandsId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.AssignmentInsteadOfComparisonPositions;

begin
  // Noncompliant: 'if Check(found := True) then' (the name expression's line
  // 18, probe-locked); the arg is the argument name.
  CheckControlRuleSrc(NewAssignmentInsteadOfComparison,
    NewAssignmentInsteadOfComparison, cAssignmentInsteadOfComparisonId, 18,
    ['found'],
    cAssignmentInsteadOfComparisonNoncompliant,
    cAssignmentInsteadOfComparisonCompliant);
end;


procedure TRulesControlTest.AssignmentInsteadOfComparisonDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cOperatorTrapsUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewAssignmentInsteadOfComparison, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cAssignmentInsteadOfComparisonId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.ConditionWithSideEffectPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if ready and TryGet(v) then' (the call's line 18,
  // probe-locked); the arg is the routine name. Compliant: the same call in the
  // FIRST operand, which always runs, and a callee whose only parameter is const.
  CheckControlRuleSrc(NewConditionWithSideEffect, NewConditionWithSideEffect,
    cConditionWithSideEffectId, 18, ['TryGet'],
    cConditionWithSideEffectNoncompliant, cConditionWithSideEffectCompliant);

  // The remaining positive rows: redundant parentheses round the call (not in the
  // AST), an 'or' in a while condition and an 'or' in an until condition.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConditionWithSideEffect,
        lFix.Add('variants.pas', cConditionWithSideEffectVariants), lc);
      AssertEquals('three guarded mutating calls', 3,
        CountById(lc, cConditionWithSideEffectId));
      k := FirstById(lc, cConditionWithSideEffectId);
      AssertEquals('parenthesised call row', 24, lc.Issues[k].StartLine);
      AssertEquals('while or row', 26, lc.Issues[k + 1].StartLine);
      AssertEquals('out argument arg', 'TryOut', lc.Issues[k + 1].MessageArgs[0]);
      AssertEquals('until or row', 30, lc.Issues[k + 2].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.ConditionWithSideEffectDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  // The fixture carries 'if a and TryGet(m) then' against a var-argument
  // callee, the shape ConditionWithSideEffectPositions measures firing.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cOperatorTrapsUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConditionWithSideEffect, lPath, lc);
      AssertEquals('no resolver => silent', 0,
        CountById(lc, cConditionWithSideEffectId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.ConditionWithSideEffectSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k, m: Integer;

begin
  // Mode 2: the resolver is live and the fixture resolves, but the callee is a
  // procedure variable.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('unresolved.pas', cConditionWithSideEffectUnresolvedCallee);
    AssertFixtureResolves(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewConditionWithSideEffect, lPath, lc);
      AssertEquals('only the direct call reports', 1,
        CountById(lc, cConditionWithSideEffectId));
      k := FirstById(lc, cConditionWithSideEffectId);
      AssertEquals('positive control row', 20, lc.Issues[k].StartLine);
      AssertEquals('positive control arg', 'TryPlain',
        lc.Issues[k].MessageArgs[0]);
      for m := 0 to lc.Count - 1 do
        if lc.Issues[m].RuleId = cConditionWithSideEffectId then
          AssertFalse('no callee declaration => silent',
            lc.Issues[m].StartLine = 22);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.RedundantElseAfterExitPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if c then Exit else DoIt' (the if keyword's line 11,
  // probe-locked); the arg is the terminator word.
  CheckControlRuleSrc(NewRedundantElseAfterExit, NewRedundantElseAfterExit,
    cRedundantElseAfterExitId, 11, ['exit'],
    cRedundantElseAfterExitNoncompliant, cRedundantElseAfterExitCompliant);

  // The remaining positive rows: a block ending in Exit, the Exit(x) call form,
  // raise, Halt(1), and Break / Continue inside their loops.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRedundantElseAfterExit,
        lFix.Add('variants.pas', cRedundantElseAfterExitVariants), lc);
      AssertEquals('six terminator forms', 6,
        CountById(lc, cRedundantElseAfterExitId));
      k := FirstById(lc, cRedundantElseAfterExitId);
      AssertEquals('block tail row', 15, lc.Issues[k].StartLine);
      AssertEquals('block tail arg', 'exit', lc.Issues[k].MessageArgs[0]);
      AssertEquals('Exit(x) row', 22, lc.Issues[k + 1].StartLine);
      AssertEquals('Exit(x) arg', 'exit', lc.Issues[k + 1].MessageArgs[0]);
      AssertEquals('raise row', 26, lc.Issues[k + 2].StartLine);
      AssertEquals('raise arg', 'raise', lc.Issues[k + 2].MessageArgs[0]);
      AssertEquals('Halt row', 30, lc.Issues[k + 3].StartLine);
      AssertEquals('Halt arg', 'halt', lc.Issues[k + 3].MessageArgs[0]);
      AssertEquals('Break row', 40, lc.Issues[k + 4].StartLine);
      AssertEquals('Break arg', 'break', lc.Issues[k + 4].MessageArgs[0]);
      AssertEquals('Continue row', 45, lc.Issues[k + 5].StartLine);
      AssertEquals('Continue arg', 'continue', lc.Issues[k + 5].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.RedundantElseAfterExitDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cIfShapeUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRedundantElseAfterExit, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cRedundantElseAfterExitId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.CollapsibleNestedIfPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'if a then if b then DoIt' (the outer if keyword's line 11,
  // probe-locked); the arg is the inner condition text.
  CheckControlRuleSrc(NewCollapsibleNestedIf, NewCollapsibleNestedIf,
    cCollapsibleNestedIfId, 11, ['b'],
    cCollapsibleNestedIfNoncompliant, cCollapsibleNestedIfCompliant);

  // The remaining positive rows: the inner if wrapped in a one-statement block,
  // an else-if chain link that nests, and a three-deep chain, which is the only
  // form that distinguishes one issue per node from one per outermost node.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewCollapsibleNestedIf,
        lFix.Add('variants.pas', cCollapsibleNestedIfVariants), lc);
      AssertEquals('four collapsible nestings', 4,
        CountById(lc, cCollapsibleNestedIfId));
      k := FirstById(lc, cCollapsibleNestedIfId);
      AssertEquals('one-statement block row', 11, lc.Issues[k].StartLine);
      AssertEquals('one-statement block arg', 'b', lc.Issues[k].MessageArgs[0]);
      AssertEquals('chain link row', 18, lc.Issues[k + 1].StartLine);
      AssertEquals('chain link arg', 'b', lc.Issues[k + 1].MessageArgs[0]);
      AssertEquals('three-deep outer row', 21, lc.Issues[k + 2].StartLine);
      AssertEquals('three-deep outer arg', 'b', lc.Issues[k + 2].MessageArgs[0]);
      AssertEquals('three-deep inner row', 22, lc.Issues[k + 3].StartLine);
      AssertEquals('three-deep inner arg', 'p', lc.Issues[k + 3].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.CollapsibleNestedIfDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cIfShapeUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewCollapsibleNestedIf, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cCollapsibleNestedIfId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.NegatedConditionWithElsePositions;

begin
  // Noncompliant: 'if not c then DoIt else DoOther' (the if keyword's line 14,
  // probe-locked); the arg is the operand text.
  CheckControlRuleSrc(NewNegatedConditionWithElse, NewNegatedConditionWithElse,
    cNegatedConditionWithElseId, 14, ['c'],
    cNegatedConditionWithElseNoncompliant, cNegatedConditionWithElseCompliant);
end;


procedure TRulesControlTest.NegatedConditionWithElseDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cIfShapeUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewNegatedConditionWithElse, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cNegatedConditionWithElseId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.SwitchOnBooleanExpressionPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'case aFlag of True: ...; False: ...' (the case keyword's
  // line 11, probe-locked); the arg is the selector text. Compliant: an enum,
  // an integer and a char selector => all silent.
  CheckControlRuleSrc(NewSwitchOnBooleanExpression,
    NewSwitchOnBooleanExpression, cSwitchOnBooleanExpressionId, 11, ['aFlag'],
    cSwitchOnBooleanExpressionNoncompliant,
    cSwitchOnBooleanExpressionCompliant);

  // The compliant zero above is pinned on parse only, and this is an rfResolver
  // rule.
  lFix := TTempFixtures.Create;
  try
    AssertFixtureResolves(lFix.Add('compliant.pas',
      cSwitchOnBooleanExpressionCompliant));
  finally
    lFix.Free;
  end;

  // The remaining positive rows: a Boolean-returning call as the selector, and
  // a LongBool selector, which ltkBool covers along with the Boolean aliases.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewSwitchOnBooleanExpression,
        lFix.Add('variants.pas', cSwitchOnBooleanExpressionVariants), lc);
      AssertEquals('two Boolean selectors', 2,
        CountById(lc, cSwitchOnBooleanExpressionId));
      k := FirstById(lc, cSwitchOnBooleanExpressionId);
      AssertEquals('call selector row', 16, lc.Issues[k].StartLine);
      AssertEquals('call selector arg', 'IsReady', lc.Issues[k].MessageArgs[0]);
      AssertEquals('LongBool selector row', 20, lc.Issues[k + 1].StartLine);
      AssertEquals('LongBool selector arg', 'aWide',
        lc.Issues[k + 1].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.SwitchOnBooleanExpressionDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  // The fixture carries the Boolean selector SwitchOnBooleanExpressionPositions
  // measures firing.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cSelectorAndLoopUnresolvable);
    AssertFixtureParsesWithoutResolving(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewSwitchOnBooleanExpression, lPath, lc);
      AssertEquals('no resolver => silent', 0,
        CountById(lc, cSwitchOnBooleanExpressionId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.SwitchOnBooleanExpressionSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k: Integer;

begin
  // Mode 2: the resolver is live and the fixture resolves, but the selector on
  // line 17 is typed by the generic parameter T, which no instantiation in the
  // closure binds.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('unresolved.pas',
      cSwitchOnBooleanExpressionUnresolvedSelector);
    AssertFixtureResolves(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewSwitchOnBooleanExpression, lPath, lc);
      AssertEquals('only the Boolean local reports', 1,
        CountById(lc, cSwitchOnBooleanExpressionId));
      k := FirstById(lc, cSwitchOnBooleanExpressionId);
      AssertEquals('positive control row', 21, lc.Issues[k].StartLine);
      AssertEquals('positive control arg', 'lLocal',
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.LoopConditionNeverChangesPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: 'while not lDone do Note(1);' (the while keyword's line 14,
  // probe-locked); the arg is the condition text as PasTree serializes it.
  CheckControlRuleSrc(NewLoopConditionNeverChanges,
    NewLoopConditionNeverChanges, cLoopConditionNeverChangesId, 14,
    ['not  lDone'], cLoopConditionNeverChangesNoncompliant,
    cLoopConditionNeverChangesCompliant);

  // The compliant zero above is pinned on parse only, and this is an rfResolver
  // rule.
  lFix := TTempFixtures.Create;
  try
    AssertFixtureResolves(lFix.Add('compliant.pas',
      cLoopConditionNeverChangesCompliant));
  finally
    lFix.Free;
  end;

  // The remaining positive rows: the repeat form, reported at the repeat row
  // rather than the until row, and a relational condition over an integer.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewLoopConditionNeverChanges,
        lFix.Add('variants.pas', cLoopConditionNeverChangesVariants), lc);
      AssertEquals('two unchanging loops', 2,
        CountById(lc, cLoopConditionNeverChangesId));
      k := FirstById(lc, cLoopConditionNeverChangesId);
      AssertEquals('repeat row', 16, lc.Issues[k].StartLine);
      AssertEquals('repeat arg', 'lDone', lc.Issues[k].MessageArgs[0]);
      AssertEquals('relational while row', 19, lc.Issues[k + 1].StartLine);
      AssertEquals('relational while arg', 'i < 3',
        lc.Issues[k + 1].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.LoopConditionNeverChangesDegradesWithoutResolver;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  // The fixture carries the 'while not lDone do Note(1);' shape
  // LoopConditionNeverChangesPositions measures firing.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cSelectorAndLoopUnresolvable);
    AssertFixtureParsesWithoutResolving(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewLoopConditionNeverChanges, lPath, lc);
      AssertEquals('no resolver => silent', 0,
        CountById(lc, cLoopConditionNeverChangesId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.LoopConditionNeverChangesSilentOnUnresolvedOperand;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k: Integer;

begin
  // Mode 2: the resolver is live and the fixture resolves, but the condition
  // name on line 19 is TStringList.Sorted, declared in Classes -- a unit
  // resolved from a stub and never analysed.
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('unresolved.pas',
      cLoopConditionNeverChangesUnresolvedName);
    AssertFixtureResolves(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewLoopConditionNeverChanges, lPath, lc);
      AssertEquals('only the local condition reports', 1,
        CountById(lc, cLoopConditionNeverChangesId));
      k := FirstById(lc, cLoopConditionNeverChangesId);
      AssertEquals('positive control row', 21, lc.Issues[k].StartLine);
      AssertEquals('positive control arg', 'not  lDone',
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.UnreachableCodePositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Noncompliant: the statement after Exit (line 12, probe-locked); the message
  // carries no arg.
  CheckControlRuleSrc(NewUnreachableCode, NewUnreachableCode,
    cUnreachableCodeId, 12, [], cUnreachableCodeNoncompliant,
    cUnreachableCodeCompliant);

  // The remaining positive rows: raise, Halt and the Exit(x) call form in a
  // routine's own list, then Break, Continue and Exit in a for, while, repeat,
  // try, case else and nested begin list, and last a list carrying two
  // terminators.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewUnreachableCode,
        lFix.Add('variants.pas', cUnreachableCodeVariants), lc);
      AssertEquals('ten dead statements', 10,
        CountById(lc, cUnreachableCodeId));
      k := FirstById(lc, cUnreachableCodeId);
      AssertEquals('raise row', 22, lc.Issues[k].StartLine);
      AssertEquals('Halt row', 27, lc.Issues[k + 1].StartLine);
      AssertEquals('Exit(x) row', 33, lc.Issues[k + 2].StartLine);
      AssertEquals('for body row', 42, lc.Issues[k + 3].StartLine);
      AssertEquals('while body row', 50, lc.Issues[k + 4].StartLine);
      AssertEquals('repeat body row', 57, lc.Issues[k + 5].StartLine);
      AssertEquals('try body row', 64, lc.Issues[k + 6].StartLine);
      AssertEquals('case else row', 75, lc.Issues[k + 7].StartLine);
      AssertEquals('nested block row', 82, lc.Issues[k + 8].StartLine);
      // Two terminators in one list still report once, at the first tail
      // statement: line 91 is dead too and stays unreported.
      AssertEquals('two terminators row', 89, lc.Issues[k + 9].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.UnreachableCodeReportsFlowDeadCode;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // The five shapes no adjacent terminator explains: after a goto, after an if
  // whose branches both exit, after an exhausted try..except, after a case
  // whose every arm exits, and a dead container at its own row.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewUnreachableCode,
        lFix.Add('widened.pas', cUnreachableCodeWidened), lc);
      AssertEquals('five dead statements', 5,
        CountById(lc, cUnreachableCodeId));
      k := FirstById(lc, cUnreachableCodeId);
      AssertEquals('after goto row', 18, lc.Issues[k].StartLine);
      AssertEquals('after if row', 28, lc.Issues[k + 1].StartLine);
      AssertEquals('after try row', 37, lc.Issues[k + 2].StartLine);
      AssertEquals('after case row', 47, lc.Issues[k + 3].StartLine);
      AssertEquals('dead container row', 52, lc.Issues[k + 4].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.UnreachableCodeSilentOnLiveContainers;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // The repeat element sits in the never-evaluated condition node and the
  // goto'd block in a node nothing reaches, yet both bodies run; the except
  // handler of an empty protected body is unreached but opens its own list.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewUnreachableCode,
        lFix.Add('guards.pas', cUnreachableCodeGuards), lc);
      AssertEquals('only the control routine reports', 1,
        CountById(lc, cUnreachableCodeId));
      AssertEquals('control row', 41,
        lc.Issues[FirstById(lc, cUnreachableCodeId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.UnreachableCodeDegradesOnParseFailure;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('broken.pas', cUnreachableCodeUnparseable);
    AssertFixtureDoesNotParse(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewUnreachableCode, lPath, lc);
      AssertEquals('no module => silent', 0,
        CountById(lc, cUnreachableCodeId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.IfShapeRulesDoNotOverlapLoopAndFreeRules;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    // Direction one: the two rules the story names fire on their own shapes and
    // none of the three new ids sees anything in them.
    lPath := lFix.Add('existing.pas', cIfShapeOverlapExisting);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRedundantAssignedCheckBeforeFree, lPath, lc);
      RunRule(NewSingleIterationLoop, lPath, lc);
      RunRule(NewRedundantElseAfterExit, lPath, lc);
      RunRule(NewCollapsibleNestedIf, lPath, lc);
      RunRule(NewNegatedConditionWithElse, lPath, lc);
      AssertEquals('the Free guard reports', 1,
        CountById(lc, cRedundantAssignedCheckBeforeFreeId));
      AssertEquals('the single-iteration loop reports', 1,
        CountById(lc, cSingleIterationLoopId));
      AssertEquals('RedundantElseAfterExit silent', 0,
        CountById(lc, cRedundantElseAfterExitId));
      AssertEquals('CollapsibleNestedIf silent', 0,
        CountById(lc, cCollapsibleNestedIfId));
      AssertEquals('NegatedConditionWithElse silent', 0,
        CountById(lc, cNegatedConditionWithElseId));
    finally
      lc.Free;
    end;

    // Direction two: on the three new shapes the two existing ids are silent.
    // The fixture is pinned as resolving.
    lPath := lFix.Add('new.pas', cIfShapeOverlapNew);
    AssertFixtureResolves(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRedundantAssignedCheckBeforeFree, lPath, lc);
      RunRule(NewSingleIterationLoop, lPath, lc);
      RunRule(NewRedundantElseAfterExit, lPath, lc);
      RunRule(NewCollapsibleNestedIf, lPath, lc);
      RunRule(NewNegatedConditionWithElse, lPath, lc);
      AssertEquals('RedundantElseAfterExit reports', 1,
        CountById(lc, cRedundantElseAfterExitId));
      AssertEquals('CollapsibleNestedIf reports', 1,
        CountById(lc, cCollapsibleNestedIfId));
      AssertEquals('NegatedConditionWithElse reports', 1,
        CountById(lc, cNegatedConditionWithElseId));
      AssertEquals('the Free guard silent', 0,
        CountById(lc, cRedundantAssignedCheckBeforeFreeId));
      AssertEquals('the single-iteration loop silent', 0,
        CountById(lc, cSingleIterationLoopId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.AssertFixtureParsesWithoutResolving(
  const aPath: string);

var
  lSrc: TFpSonarSourceFile;

begin
  lSrc := TFpSonarSourceFile.Create;
  try
    lSrc.Analyze(aPath, cMode, cDefines);
    AssertTrue('fixture parsed', lSrc.ParseSucceeded);
    AssertTrue('a resolver was built', lSrc.Resolver <> nil);
    AssertFalse('the resolver rejected the module', lSrc.Resolver.Succeeded);
  finally
    lSrc.Free;
  end;
end;


procedure TRulesControlTest.AssertFixtureResolves(const aPath: string);

var
  lSrc: TFpSonarSourceFile;

begin
  lSrc := TFpSonarSourceFile.Create;
  try
    lSrc.Analyze(aPath, cMode, cDefines);
    AssertTrue('fixture parsed', lSrc.ParseSucceeded);
    AssertTrue('a resolver was built', lSrc.Resolver <> nil);
    AssertTrue('the resolver accepted the module', lSrc.Resolver.Succeeded);
  finally
    lSrc.Free;
  end;
end;


procedure TRulesControlTest.AssertFixtureDoesNotParse(const aPath: string);

var
  lSrc: TFpSonarSourceFile;

begin
  lSrc := TFpSonarSourceFile.Create;
  try
    lSrc.Analyze(aPath, cMode, cDefines);
    AssertFalse('fixture did not parse', lSrc.ParseSucceeded);
    AssertTrue('no module reaches the AST feed', lSrc.Module = nil);
  finally
    lSrc.Free;
  end;
end;


procedure TRulesControlTest.AssertFixtureParses(const aPath: string);

var
  lSrc: TFpSonarSourceFile;

begin
  lSrc := TFpSonarSourceFile.Create;
  try
    lSrc.Analyze(aPath, cMode, cDefines);
    AssertTrue('fixture parsed', lSrc.ParseSucceeded);
    AssertTrue('a module reaches the feed', lSrc.Module <> nil);
  finally
    lSrc.Free;
  end;
end;


procedure TRulesControlTest.CheckResultAssignedSoleRow(const aName: string;
  const aSrc: array of string; aRow: Integer);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lPath: string;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add(aName, aSrc);
    AssertFixtureResolves(lPath);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewRoutineResultAssigned, lPath, lc);
      AssertEquals('one issue for ' + aName, 1,
        CountById(lc, cRoutineResultAssignedId));
      k := FirstById(lc, cRoutineResultAssignedId);
      AssertEquals('reported row', aRow, lc.Issues[k].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesControlTest.ControlRulesSelfRegisterGlobally;

begin
  // The production initialization registered all twenty-four control-flow rules
  // into the global registry.
  AssertTrue('ExhaustiveCaseStatement registered',
    RuleRegistry.FindById(cExhaustiveCaseStatementId) <> nil);
  AssertTrue('ExceptionRaised registered',
    RuleRegistry.FindById(cExceptionRaisedId) <> nil);
  AssertTrue('SingleIterationLoop registered',
    RuleRegistry.FindById(cSingleIterationLoopId) <> nil);
  AssertTrue('NoPascalStyleResultAssignment registered',
    RuleRegistry.FindById(cNoPascalStyleResultAssignmentId) <> nil);
  AssertTrue('RedundantAssignedCheckBeforeFree registered',
    RuleRegistry.FindById(cRedundantAssignedCheckBeforeFreeId) <> nil);
  AssertTrue('LoopBeyondCollectionEnd registered',
    RuleRegistry.FindById(cLoopBeyondCollectionEndId) <> nil);
  AssertTrue('RoutineResultAssigned registered',
    RuleRegistry.FindById(cRoutineResultAssignedId) <> nil);
  AssertTrue('NoCatchRawException registered',
    RuleRegistry.FindById(cNoCatchRawExceptionId) <> nil);
  AssertTrue('NoRaiseRawException registered',
    RuleRegistry.FindById(cNoRaiseRawExceptionId) <> nil);
  AssertTrue('IdenticalBranches registered',
    RuleRegistry.FindById(cIdenticalBranchesId) <> nil);
  AssertTrue('DuplicateConditionInChain registered',
    RuleRegistry.FindById(cDuplicateConditionInChainId) <> nil);
  AssertTrue('DuplicateCaseLabel registered',
    RuleRegistry.FindById(cDuplicateCaseLabelId) <> nil);
  AssertTrue('SelfComparison registered',
    RuleRegistry.FindById(cSelfComparisonId) <> nil);
  AssertTrue('EmptyThenWithFollowingStatement registered',
    RuleRegistry.FindById(cEmptyThenWithFollowingStatementId) <> nil);
  // The five AST-tier rules ship disabled (INV-3).
  AssertFalse('IdenticalBranches ships disabled',
    RuleRegistry.FindById(cIdenticalBranchesId).Metadata.DefaultEnabled);
  AssertFalse('DuplicateConditionInChain ships disabled',
    RuleRegistry.FindById(cDuplicateConditionInChainId).Metadata.DefaultEnabled);
  AssertFalse('DuplicateCaseLabel ships disabled',
    RuleRegistry.FindById(cDuplicateCaseLabelId).Metadata.DefaultEnabled);
  AssertFalse('SelfComparison ships disabled',
    RuleRegistry.FindById(cSelfComparisonId).Metadata.DefaultEnabled);
  AssertFalse('EmptyThenWithFollowingStatement ships disabled',
    RuleRegistry.FindById(
    cEmptyThenWithFollowingStatementId).Metadata.DefaultEnabled);
  // The operator-trap cluster, also disabled (INV-3).
  AssertTrue('MixedBooleanAndRelational registered',
    RuleRegistry.FindById(cMixedBooleanAndRelationalId) <> nil);
  AssertFalse('MixedBooleanAndRelational ships disabled',
    RuleRegistry.FindById(cMixedBooleanAndRelationalId).Metadata.DefaultEnabled);
  AssertTrue('BitwiseOnBooleanOperands registered',
    RuleRegistry.FindById(cBitwiseOnBooleanOperandsId) <> nil);
  AssertFalse('BitwiseOnBooleanOperands ships disabled',
    RuleRegistry.FindById(cBitwiseOnBooleanOperandsId).Metadata.DefaultEnabled);
  AssertTrue('AssignmentInsteadOfComparison registered',
    RuleRegistry.FindById(cAssignmentInsteadOfComparisonId) <> nil);
  AssertFalse('AssignmentInsteadOfComparison ships disabled',
    RuleRegistry.FindById(
    cAssignmentInsteadOfComparisonId).Metadata.DefaultEnabled);
  AssertTrue('ConditionWithSideEffect registered',
    RuleRegistry.FindById(cConditionWithSideEffectId) <> nil);
  AssertFalse('ConditionWithSideEffect ships disabled',
    RuleRegistry.FindById(cConditionWithSideEffectId).Metadata.DefaultEnabled);
  // The if-shape cluster, also disabled (INV-3).
  AssertTrue('RedundantElseAfterExit registered',
    RuleRegistry.FindById(cRedundantElseAfterExitId) <> nil);
  AssertFalse('RedundantElseAfterExit ships disabled',
    RuleRegistry.FindById(cRedundantElseAfterExitId).Metadata.DefaultEnabled);
  AssertTrue('CollapsibleNestedIf registered',
    RuleRegistry.FindById(cCollapsibleNestedIfId) <> nil);
  AssertFalse('CollapsibleNestedIf ships disabled',
    RuleRegistry.FindById(cCollapsibleNestedIfId).Metadata.DefaultEnabled);
  AssertTrue('NegatedConditionWithElse registered',
    RuleRegistry.FindById(cNegatedConditionWithElseId) <> nil);
  AssertFalse('NegatedConditionWithElse ships disabled',
    RuleRegistry.FindById(cNegatedConditionWithElseId).Metadata.DefaultEnabled);
  // The selector-and-loop cluster, also disabled (INV-3).
  AssertTrue('SwitchOnBooleanExpression registered',
    RuleRegistry.FindById(cSwitchOnBooleanExpressionId) <> nil);
  AssertFalse('SwitchOnBooleanExpression ships disabled',
    RuleRegistry.FindById(
    cSwitchOnBooleanExpressionId).Metadata.DefaultEnabled);
  AssertTrue('LoopConditionNeverChanges registered',
    RuleRegistry.FindById(cLoopConditionNeverChangesId) <> nil);
  AssertFalse('LoopConditionNeverChanges ships disabled',
    RuleRegistry.FindById(
    cLoopConditionNeverChangesId).Metadata.DefaultEnabled);
  // The unreachable-code rule, also disabled (INV-3).
  AssertTrue('UnreachableCode registered',
    RuleRegistry.FindById(cUnreachableCodeId) <> nil);
  AssertFalse('UnreachableCode ships disabled',
    RuleRegistry.FindById(cUnreachableCodeId).Metadata.DefaultEnabled);
end;


initialization
  RegisterTest(TRulesControlTest);

end.
