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

{ The control-flow SEM rule tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Control, UtstFixtures;

type
  { SEM-tier control-flow-rule position + registration tests. }
  TRulesControlTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires exactly once at aDeclLine, column 1, with key
    // rule.<aId>.message and message args = aArgs; and zero on the compliant
    // fixture. Accepts a zero-length aArgs. Fixtures supplied inline (one array
    // element per source line) and materialised to a temp dir.
    procedure CheckControlRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewExhaustiveCaseStatement: TRuleBase;
    function NewExceptionRaised: TRuleBase;
    function NewSingleIterationLoop: TRuleBase;
    function NewNoPascalStyleResultAssignment: TRuleBase;
    function NewRedundantAssignedCheckBeforeFree: TRuleBase;
    function NewLoopBeyondCollectionEnd: TRuleBase;
    function NewRoutineResultAssigned: TRuleBase;
    function NewNoCatchRawException: TRuleBase;
    function NewNoRaiseRawException: TRuleBase;
  published
    procedure ExhaustiveCaseStatementPositions;
    procedure ExceptionRaisedPositions;
    procedure SingleIterationLoopPositions;
    procedure NoPascalStyleResultAssignmentPositions;
    procedure NoPascalStyleResultAssignmentMethodFires;
    procedure RedundantAssignedCheckBeforeFreePositions;
    procedure LoopBeyondCollectionEndPositions;
    procedure RoutineResultAssignedPositions;
    procedure NoCatchRawExceptionPositions;
    procedure NoRaiseRawExceptionPositions;
    procedure ControlRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cExhaustiveCaseStatementId = 'ExhaustiveCaseStatement';
  cExceptionRaisedId = 'ExceptionRaised';
  cSingleIterationLoopId = 'SingleIterationLoop';
  cNoPascalStyleResultAssignmentId = 'NoPascalStyleResultAssignment';
  cRedundantAssignedCheckBeforeFreeId = 'RedundantAssignedCheckBeforeFree';
  cLoopBeyondCollectionEndId = 'LoopBeyondCollectionEnd';
  cRoutineResultAssignedId = 'RoutineResultAssigned';
  cNoCatchRawExceptionId = 'NoCatchRawException';
  cNoRaiseRawExceptionId = 'NoRaiseRawException';

  // Embedded control-flow-rule fixtures (Approach A rollout): line i+1 == [i].

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
    '// A with-scoped write: the whole query abstains on `with` (unprovable) => silent.',
    'function WithBody: Integer;',
    'var',
    '  r: TRec;',
    'begin',
    '  r.V := 7;',
    '  with r do',
    '    Result := V;',
    'end;',
    '',
    '// A loop body writes the result: the conservative subtree-write fallback',
    '// suppresses (a write anywhere in the subtree may suffice).',
    'function LoopWrite: Integer;',
    'var',
    '  i: Integer;',
    'begin',
    '  for i := 1 to 10 do',
    '    Result := i;',
    'end;',
    '',
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
    lEngine.Analyze(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
      aCollector);
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


procedure TRulesControlTest.CheckControlRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at the construct line, column 1 (AST/SEM
    // nodes carry no column), carrying aArgs as the message args.
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

    // Compliant: the FP guards stay silent (and the fixture MUST resolve clean,
    // else the rfResolver feed is absent -> rule skipped -> a false 0).
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
  // Compliant: a raised form, an assigned form, and a discarded NON-exception
  // TObject.Create() (ancestry not Exception => silent) => all silent.
  CheckControlRuleSrc(NewExceptionRaised, NewExceptionRaised,
    cExceptionRaisedId, 11, ['EBadValue'],
    cExceptionRaisedNoncompliant, cExceptionRaisedCompliant);
end;


procedure TRulesControlTest.SingleIterationLoopPositions;

begin
  // Noncompliant: 'while not done do begin n := n + 1; break; end;' (while keyword
  // line 13, probe-locked); no message args (a fixed string). Compliant: a
  // guarded 'if n > 10 then break' (loop can iterate) + a Continue-bearing loop
  // => all silent.
  CheckControlRuleSrc(NewSingleIterationLoop, NewSingleIterationLoop,
    cSingleIterationLoopId, 13, [],
    cSingleIterationLoopNoncompliant, cSingleIterationLoopCompliant);
end;


procedure TRulesControlTest.NoPascalStyleResultAssignmentPositions;

begin
  // Noncompliant: 'Add := a + b;' (the legacy function-name return; assignment
  // LHS line 8, probe-locked); arg is the function name. Compliant: the 'Result'
  // form AND a function with a local variable shadowing the function name
  // (resolves to the local, NOT the result element) => both silent.
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
  // Regression: a class method's TPasProcedure.Name is dotted ('TBar.Calc') but
  // the source LHS uses the bare member name ('Calc') — the rule must still fire
  // on the legacy 'Calc := a + 1' (method.pas line 14), not silently drop every
  // method. The arg is the qualified routine name.
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
  // (if-condition line 9, probe-locked); no message args (a fixed string).
  // Compliant: an unguarded o.Free, a two-statement guarded branch, and a
  // different-object guard ('if Assigned(a) then b.Free') => all silent.
  CheckControlRuleSrc(NewRedundantAssignedCheckBeforeFree,
    NewRedundantAssignedCheckBeforeFree, cRedundantAssignedCheckBeforeFreeId, 9,
    [],
    cRedundantAssignedCheckBeforeFreeNoncompliant, cRedundantAssignedCheckBeforeFreeCompliant);
end;


procedure TRulesControlTest.LoopBeyondCollectionEndPositions;

begin
  // Noncompliant: 'for i := 0 to Length(A) do' newline 'A[i] := 0;' over
  // 'A: array[0..9] of Integer' — Length(A)=10 reaches index 10, one past
  // High(A)=9 (the overrunning index node A[i] on line 12, probe-locked); no
  // message args (a fixed string). Compliant: the correctly-bounded
  // 'Low(A)..High(A)' loop, a dynamic array (runtime length), and a non-constant
  // bound => all silent (the load-bearing degrade canaries).
  CheckControlRuleSrc(NewLoopBeyondCollectionEnd, NewLoopBeyondCollectionEnd,
    cLoopBeyondCollectionEndId, 12, [],
    cLoopBeyondCollectionEndNoncompliant, cLoopBeyondCollectionEndCompliant);
end;


procedure TRulesControlTest.RoutineResultAssignedPositions;

begin
  // Noncompliant: 'function Pick(b: Boolean): Integer;' whose body is just
  // 'if b then Result := 1;' with no else — the b=False path falls off 'end'
  // with no result write (the function declaration row 11, probe-locked); no
  // message args (a fixed string). Compliant: the if/else, case+else, sole write,
  // legacy name form, exit(value), raise terminator, out-param write, with-bodied
  // write (abstained), and writing loop (subtree-write) => all silent.
  CheckControlRuleSrc(NewRoutineResultAssigned, NewRoutineResultAssigned,
    cRoutineResultAssignedId, 11, [],
    cRoutineResultAssignedNoncompliant, cRoutineResultAssignedCompliant);
end;


procedure TRulesControlTest.NoCatchRawExceptionPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // Noncompliant: 'on E: Exception do DoWork;' — the caught type alias-resolves to
  // the root Exception (on-handler line 22, probe-locked); no message args.
  // Compliant: 'on E: EConvertError do' (subclass), 'on E: Exception do raise'
  // (re-raise exempt), an EMPTY bare 'except end' (another rule's domain), and a non-empty
  // bare 'except DoWork; raise; end' (re-raise) => all silent.
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
  // Noncompliant: 'raise Exception.Create('something failed');' — the constructed
  // class is exactly the root Exception (raised-expr line 15, probe-locked); no
  // message args. Compliant: 'raise EConvertError.Create' and
  // 'raise EMyError.Create' (specific subclasses), and a bare 'raise;'
  // (ExceptObject=nil re-raise) => all silent.
  CheckControlRuleSrc(NewNoRaiseRawException, NewNoRaiseRawException,
    cNoRaiseRawExceptionId, 15, [],
    cNoRaiseRawExceptionNoncompliant, cNoRaiseRawExceptionCompliant);
end;


procedure TRulesControlTest.ControlRulesSelfRegisterGlobally;

begin
  // The production initialization registered all nine SEM control-flow rules
  // into the GLOBAL registry (this is what the CLI process runs).
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
end;


initialization
  RegisterTest(TRulesControlTest);

end.
