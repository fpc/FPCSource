{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the reference / procedural-value (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesRefs;

{ The resolver-backed (SEM) reference / proc-value rule tests }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Resolver,
  FpSonar.Rules.Refs, UtstFixtures;

type
  { SEM-tier reference / proc-value rule position + registration tests. }
  TRulesRefsTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires exactly once at aLine, column 1, with key
    // rule.<aId>.message and ZERO message args; and zero on the compliant fixture
    // (which must resolve clean — the silent-skip canary). Fixtures supplied
    // inline (one array element per source line) and materialised to a temp dir.
    procedure CheckSingleRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aLine: Integer;
      const aNoncompliant, aCompliant: array of string);
    // Asserts aRule fires exactly twice at aLine1 then aLine2 (both column 1),
    // each with key rule.<aId>.message and ZERO args; and zero on the compliant
    // fixture. Fixtures supplied inline, materialised to a temp dir. (For
    // NoObjectAsInterface's two offending sites: the assignment then the call arg.)
    procedure CheckTwoRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aLine1, aLine2: Integer;
      const aNoncompliant, aCompliant: array of string);
    // Asserts aFixture resolves clean (Succeeded=True) — guards the #66 compliant
    // canary against silently degrading to a vacuous silent-skip (false green).
    procedure AssertResolvesClean(const aFixture: string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the unit's
    // self-registration; empty key defaults to rule.<RuleId>.message).
    function NewNoObjectAsInterface: TRuleBase;
    function NewNoNestedRoutineAsProcValue: TRuleBase;
    function NewNoInlineVarCapturedByAnonMethod: TRuleBase;
  published
    procedure NoObjectAsInterfacePositions;
    procedure NoNestedRoutineAsProcValuePositions;
    procedure NoInlineVarCapturedByAnonMethodPositions;
    procedure RefsRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cNoObjectAsInterfaceId = 'NoObjectAsInterface';
  cNoNestedRoutineAsProcValueId = 'NoNestedRoutineAsProcValue';
  cNoInlineVarCapturedByAnonMethodId = 'NoInlineVarCapturedByAnonMethod';

  // Embedded reference-rule fixtures (Approach A rollout): line i+1 == [i].

  cNoObjectAsInterfaceNoncompliant: array[0..26] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  IMyIntf = interface',
    '    procedure DoIt;',
    '  end;',
    '  TMyClass = class(TInterfacedObject, IMyIntf)',
    '    procedure DoIt;',
    '  end;',
    'implementation',
    'procedure Use(I: IMyIntf);',
    'begin',
    'end;',
    'procedure Demo;',
    'var',
    '  Obj: TMyClass;',
    '  Intf: IMyIntf;',
    'begin',
    '  Obj := TMyClass.Create;',
    '  Intf := Obj;',
    '  Use(Obj);',
    'end;',
    'procedure TMyClass.DoIt;',
    'begin',
    'end;',
    'end.');

  cNoObjectAsInterfaceCompliant: array[0..41] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  IMyIntf = interface',
    '    procedure DoIt;',
    '  end;',
    '  TMyClass = class(TInterfacedObject, IMyIntf)',
    '    procedure DoIt;',
    '  end;',
    '{$INTERFACES CORBA}',
    '  ICorbaIntf = interface',
    '    procedure DoCorba;',
    '  end;',
    '  TCorbaClass = class(TObject, ICorbaIntf)',
    '    procedure DoCorba;',
    '  end;',
    '{$INTERFACES COM}',
    'implementation',
    'procedure Demo;',
    'var',
    '  Obj: TMyClass;',
    '  Intf: IMyIntf;',
    '  Other: IMyIntf;',
    '  CObj: TCorbaClass;',
    '  Corba: ICorbaIntf;',
    'begin',
    '  Obj := TMyClass.Create;',
    '  Intf := Obj as IMyIntf;',
    '  Intf := nil;',
    '  Other := Intf;',
    '  Intf := TMyClass.Create as IMyIntf;',
    '  CObj := TCorbaClass.Create;',
    '  Corba := CObj;',
    'end;',
    'procedure TMyClass.DoIt;',
    'begin',
    'end;',
    'procedure TCorbaClass.DoCorba;',
    'begin',
    'end;',
    'end.');

  cNoNestedRoutineAsProcValueNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    '{$modeswitch nestedprocvars}',
    'interface',
    'type',
    '  TNestedProc = procedure(I: Integer) is nested;',
    'implementation',
    'var',
    '  Cb: TNestedProc;',
    'procedure Outer;',
    '  procedure Inner(I: Integer);',
    '  begin',
    '  end;',
    'begin',
    '  Cb := @Inner;',
    'end;',
    'end.');

  cNoNestedRoutineAsProcValueCompliant: array[0..26] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    '{$modeswitch nestedprocvars}',
    'interface',
    'type',
    '  TNestedProc = procedure(I: Integer) is nested;',
    'implementation',
    'var',
    '  Cb: procedure(I: Integer);',
    'procedure ForEach(P: TNestedProc);',
    'begin',
    'end;',
    'procedure TopLevel(I: Integer);',
    'begin',
    'end;',
    'procedure Outer;',
    'var',
    '  Local: TNestedProc;',
    '  procedure Inner(I: Integer);',
    '  begin',
    '  end;',
    'begin',
    '  ForEach(@Inner);',
    '  Local := @Inner;',
    '  Cb := @TopLevel;',
    'end;',
    'end.');

  cNoInlineVarCapturedByAnonMethodNoncompliant: array[0..40] of string = (
    'unit noncompliant;',
    '',
    '{ NoInlineVarCapturedByAnonMethod (#66) noncompliant fixture.',
    '  {$mode delphi} enables BOTH msAnonymousFunctions and msInlineVars (the analyzer',
    '  never sets these modeswitches itself — the source must opt in). Two captures of',
    '  narrower-lifetime variables by an anonymous method:',
    '    (a) a block-scoped inline `var Inner` (Parent is the enclosing TPasImplBlock);',
    '    (b) a `for var I` per-iteration loop control variable (owning loop IsVarDef=true).',
    '  Must resolve clean against the in-process synthetic RTL. }',
    '',
    '{$mode delphi}',
    '',
    'interface',
    '',
    'procedure Demo;',
    '',
    'implementation',
    '',
    'type',
    '  TProc = reference to procedure;',
    '',
    'procedure Use(const aValue);',
    'begin',
    'end;',
    '',
    'procedure Run(aCb: TProc);',
    'begin',
    '  aCb();',
    'end;',
    '',
    'procedure Demo;',
    'begin',
    '  begin',
    '    var Inner := 0;',
    '    Run(procedure begin Use(Inner); end);',
    '  end;',
    '  for var I := 0 to 9 do',
    '    Run(procedure begin Use(I); end);',
    'end;',
    '',
    'end.');

  cNoInlineVarCapturedByAnonMethodCompliant: array[0..48] of string = (
    'unit compliant;',
    '',
    '{ NoInlineVarCapturedByAnonMethod (#66) compliant fixture (silent-skip',
    '  canary: MUST resolve clean AND emit 0). {$mode delphi} enables the anonymous-method',
    '  and inline-var modeswitches. Three safe shapes:',
    '    * a closure capturing a routine-level local (Total: var-section, Parent is the',
    '      routine''s TProcedureBody/TPasSection — lives as long as the routine);',
    '    * a `for I :=` loop reusing a pre-existing routine-level local (IsVarDef=false);',
    '    * a closure using only its OWN local (not a capture). }',
    '',
    '{$mode delphi}',
    '',
    'interface',
    '',
    'procedure Demo;',
    '',
    'implementation',
    '',
    'type',
    '  TProc = reference to procedure;',
    '',
    'procedure Use(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure Run(aCb: TProc);',
    'begin',
    '  aCb();',
    'end;',
    '',
    'procedure Demo;',
    'var',
    '  Total: Integer;',
    '  I: Integer;',
    'begin',
    '  Total := 0;',
    '  Run(procedure begin Use(Total); end);',
    '  for I := 0 to 9 do',
    '    Run(procedure begin Use(I); end);',
    '  Run(procedure',
    '      var',
    '        L: Integer;',
    '      begin',
    '        L := 0;',
    '        Use(L);',
    '      end);',
    'end;',
    '',
    'end.');

procedure TRulesRefsTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesRefsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesRefsTest.FirstById(
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


procedure TRulesRefsTest.AssertResolvesClean(const aFixture: string);

var
  lRes: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  lRes := TFpSonarResolver.Create;
  try
    AssertTrue('#66 compliant fixture must resolve clean (else the zero-issue ' +
      'assert is a vacuous silent-skip)',
      lRes.BuildFor(aFixture, cMode, ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'],
        [], [], lDiag)
      and lRes.Succeeded);
  finally
    lRes.Free;
  end;
end;


function TRulesRefsTest.NewNoObjectAsInterface: TRuleBase;

begin
  Result := TRuleNoObjectAsInterface.Create(TRuleMetadata.Make(
    cNoObjectAsInterfaceId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesRefsTest.NewNoNestedRoutineAsProcValue: TRuleBase;

begin
  Result := TRuleNoNestedRoutineAsProcValue.Create(TRuleMetadata.Make(
    cNoNestedRoutineAsProcValueId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesRefsTest.NewNoInlineVarCapturedByAnonMethod: TRuleBase;

begin
  Result := TRuleNoInlineVarCapturedByAnonMethod.Create(TRuleMetadata.Make(
    cNoInlineVarCapturedByAnonMethodId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


procedure TRulesRefsTest.CheckSingleRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aLine: Integer;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at the offending node's row, column 1 (SEM
    // nodes carry no column), carrying NO message args.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('no args', 0, Length(lc.Issues[k].MessageArgs));
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


procedure TRulesRefsTest.CheckTwoRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aLine1, aLine2: Integer;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly two issues (the two offending sites of this rule's
    // fixture, in statement-then-expression order), each at its node row, column 1,
    // with key rule.<aId>.message and zero args.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('two issues for ' + aId, 2, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('first node line', aLine1, lc.Issues[k].StartLine);
      AssertEquals('first start col', 1, lc.Issues[k].StartCol);
      AssertEquals('first key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('first no args', 0, Length(lc.Issues[k].MessageArgs));
      AssertEquals('second node line', aLine2, lc.Issues[k + 1].StartLine);
      AssertEquals('second start col', 1, lc.Issues[k + 1].StartCol);
      AssertEquals('second no args', 0, Length(lc.Issues[k + 1].MessageArgs));
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


procedure TRulesRefsTest.NoObjectAsInterfacePositions;

begin
  // Noncompliant: 'Intf := Obj;' (assignment RHS, row 21) and 'Use(Obj)' (call
  // arg, row 22), both probe-locked; the anchor is the source object expression.
  // Compliant: an explicit 'Obj as IMyIntf', 'Intf := nil', an interface->interface
  // assignment, a freshly-created 'TMyClass.Create as IMyIntf', and a CORBA-interface
  // target (corbaInterfacesExempt) — all silent.
  CheckTwoRuleSrc(NewNoObjectAsInterface, NewNoObjectAsInterface,
    cNoObjectAsInterfaceId, 21, 22,
    cNoObjectAsInterfaceNoncompliant, cNoObjectAsInterfaceCompliant);
end;


procedure TRulesRefsTest.NoNestedRoutineAsProcValuePositions;

begin
  // Noncompliant: 'Cb := @Inner;' (the @-expr on row 15, probe-locked) where Inner
  // is nested in Outer and Cb is a global var that outlives the frame. Compliant:
  // a synchronous call-arg 'ForEach(@Inner)', a same-frame 'Local := @Inner', and a
  // non-nested 'Cb := @TopLevel' — all silent.
  CheckSingleRuleSrc(NewNoNestedRoutineAsProcValue, NewNoNestedRoutineAsProcValue,
    cNoNestedRoutineAsProcValueId, 15,
    cNoNestedRoutineAsProcValueNoncompliant, cNoNestedRoutineAsProcValueCompliant);
end;


procedure TRulesRefsTest.NoInlineVarCapturedByAnonMethodPositions;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lCompliant: string;

begin
  // The noncompliant target shapes are kept as documentation in the embedded
  // const cNoInlineVarCapturedByAnonMethodNoncompliant (not asserted — see below).
  //
  // KNOWN LIMITATION: the vendored fcl-passrc resolver
  // does NOT resolve the two constructs this rule targets — a block-scoped inline
  // 'var' statement is "not yet implemented" in ResolveImplElement, and an inline
  // 'for var I' control variable is never declared by FinishForLoopHeader. A
  // fixture containing either fails to resolve, so the rfResolver feed is absent
  // and the rule degrades to silence on its own target shapes; a positive assert on
  // a noncompliant fixture is therefore UNATTAINABLE until that resolver support
  // lands (the noncompliant fixture is kept as documentation of the target shapes).
  //
  // What IS asserted here: the compliant fixture resolves CLEAN (Succeeded=True, so
  // the rule is genuinely dispatched — the silent-skip canary) and the full capture
  // walk emits ZERO. Its three shapes — a closure capturing a routine-level local
  // (Parent=TProcedureBody, lives as long as the routine), a 'for I :=' reusing a
  // pre-existing local (IsVarDef=false), and a closure using only its OWN local —
  // are all correctly NOT flagged. This proves the wrapper never false-positives on
  // resolvable code (the DEGRADED contract: a missed hazard is acceptable, a false
  // positive is not).
  //
  // The zero-issue assert ALONE cannot distinguish "resolved clean, found nothing"
  // from "failed to resolve, rule skipped" (the false-green trap). So we FIRST assert
  // the fixture genuinely resolves — without this the canary could silently go
  // vacuous if a future change broke compliant-fixture resolution.
  lFix := TTempFixtures.Create;
  try
    lCompliant := lFix.Add('compliant.pas', cNoInlineVarCapturedByAnonMethodCompliant);
    AssertResolvesClean(lCompliant);

    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewNoInlineVarCapturedByAnonMethod, lCompliant, lc);
      AssertEquals('compliant resolvable capture => zero', 0,
        CountById(lc, cNoInlineVarCapturedByAnonMethodId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesRefsTest.RefsRulesSelfRegisterGlobally;

begin
  // The production initialization registered the SEM reference rules into the
  // GLOBAL registry (this is what the CLI process runs).
  AssertTrue('NoObjectAsInterface registered',
    RuleRegistry.FindById(cNoObjectAsInterfaceId) <> nil);
  AssertTrue('NoNestedRoutineAsProcValue registered',
    RuleRegistry.FindById(cNoNestedRoutineAsProcValueId) <> nil);
  AssertTrue('NoInlineVarCapturedByAnonMethod registered',
    RuleRegistry.FindById(cNoInlineVarCapturedByAnonMethodId) <> nil);
end;


initialization
  RegisterTest(TRulesRefsTest);

end.
