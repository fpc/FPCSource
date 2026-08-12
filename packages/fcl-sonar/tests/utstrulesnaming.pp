{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the naming-convention (AST) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesNaming;

{ The five AST-tier naming rules }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Naming, UtstFixtures;

type
  { AST-tier naming-rule position + registration tests. }
  TRulesNamingTest = class(TTestCase)
  private
    // Runs aRule (taken into a fresh local registry, freed here) over aFixture,
    // collecting issues into aCollector (caller-owned).
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts the rule fires exactly once at aDeclLine, column 1, with message
    // args [aName, aPattern]; and zero on the compliant fixture. Fixtures supplied
    // inline (one array element per source line) and materialised to a temp dir.
    procedure CheckNamingRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aName, aPattern: string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewClassNaming: TRuleBase;
    function NewRecordNaming: TRuleBase;
    function NewInterfaceNaming: TRuleBase;
    function NewEnumNaming: TRuleBase;
    function NewHelperNaming: TRuleBase;
    function NewPointerNaming: TRuleBase;
    function NewAttributeNaming: TRuleBase;
    function NewConstantNaming: TRuleBase;
    function NewFieldNaming: TRuleBase;
    function NewVariableNaming: TRuleBase;
    function NewRoutineNaming: TRuleBase;
    function NewConstructorNaming: TRuleBase;
    function NewUnitNaming: TRuleBase;
    function NewIdentifierTooShort: TRuleBase;
  published
    procedure ClassNamingPositions;
    procedure RecordNamingPositions;
    procedure InterfaceNamingPositions;
    procedure EnumNamingPositions;
    procedure HelperNamingPositions;
    procedure PointerNamingPositions;
    procedure AttributeNamingPositions;
    procedure ConstantNamingPositions;
    procedure FieldNamingPositions;
    procedure VariableNamingPositions;
    procedure RoutineNamingPositions;
    procedure ConstructorNamingPositions;
    procedure UnitNamingPositions;
    procedure IdentifierTooShortPositions;
    procedure RulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cClassNamingId = 'ClassNaming';
  cRecordNamingId = 'RecordNaming';
  cInterfaceNamingId = 'InterfaceNaming';
  cEnumNamingId = 'EnumNaming';
  cHelperNamingId = 'HelperNaming';
  cPointerNamingId = 'PointerNaming';
  cAttributeNamingId = 'AttributeNaming';
  cConstantNamingId = 'ConstantNaming';
  cFieldNamingId = 'FieldNaming';
  cVariableNamingId = 'VariableNaming';
  cRoutineNamingId = 'RoutineNaming';
  cConstructorNamingId = 'ConstructorNaming';
  cUnitNamingId = 'UnitNaming';
  cIdentifierTooShortId = 'IdentifierTooShort';

  // The default patterns asserted in the message args (mirror the unit consts).
  cPatT = '^T[A-Z][A-Za-z0-9]*$';
  cPatI = '^I[A-Z][A-Za-z0-9]*$';
  cPatHelper = '^T[A-Z][A-Za-z0-9]*Helper$';
  cPatPointer = '^P[A-Z][A-Za-z0-9]*$';
  cPatAttribute = '^[A-Z][A-Za-z0-9]*Attribute$';
  cPatConst = '^[A-Z][A-Za-z0-9_]*$';
  cPatField = '^F[A-Z][A-Za-z0-9]*$';
  cPatVariable = '^[A-Z][A-Za-z0-9]*$';
  cPatRoutine = '^[A-Z][A-Za-z0-9]*$';
  cPatCtor = '^Create[A-Za-z0-9]*$';
  cPatUnit = '^[A-Z][A-Za-z0-9.]*$';
  // IdentifierTooShort carries IntToStr(cMinIdentLength) in arg 1, not a pattern.
  cMinLenArg = '3';

  // Embedded naming-rule fixtures (Approach A rollout): line i+1 == [i].

  cClassNamingNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  badClass = class',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cClassNamingCompliant: array[0..12] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGood = class',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cRecordNamingNoncompliant: array[0..13] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  badRec = record',
    '    X: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cRecordNamingCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint = record',
    '    X: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceNamingNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWrong = interface',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceNamingCompliant: array[0..12] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IGood = interface',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cEnumNamingNoncompliant: array[0..11] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  badEnum = (eaOne, eaTwo);',
    '',
    'implementation',
    '',
    'end.');

  cEnumNamingCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TColor = (ccRed, ccGreen);',
    '',
    'implementation',
    '',
    'end.');

  cHelperNamingNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBadOne = class helper for TObject',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cHelperNamingCompliant: array[0..12] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TObjectHelper = class helper for TObject',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cPointerNamingNoncompliant: array[0..11] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  pBad = ^Integer;',
    '',
    'implementation',
    '',
    'end.');

  cPointerNamingCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  PInteger = ^Integer;',
    '',
    'implementation',
    '',
    'end.');

  cAttributeNamingNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  badAttr = class(TCustomAttribute)',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cAttributeNamingCompliant: array[0..12] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGoodAttribute = class(TCustomAttribute)',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cConstantNamingNoncompliant: array[0..11] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  badConst = 1;',
    '',
    'implementation',
    '',
    'end.');

  cConstantNamingCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  MaxValue = 100;',
    '',
    'implementation',
    '',
    'end.');

  cFieldNamingNoncompliant: array[0..13] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = record',
    '    badField: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cFieldNamingCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = record',
    '    FValue: Integer;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cVariableNamingNoncompliant: array[0..11] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  badVar: Integer;',
    '',
    'implementation',
    '',
    'end.');

  cVariableNamingCompliant: array[0..11] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GlobalCount: Integer;',
    '',
    'implementation',
    '',
    'end.');

  cRoutineNamingNoncompliant: array[0..14] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'function doStuff: Integer;',
    '',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'end.');

  cRoutineNamingCompliant: array[0..14] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'function DoStuff: Integer;',
    '',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'end.');

  cConstructorNamingNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '    constructor make;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TThing.make;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cConstructorNamingCompliant: array[0..19] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TThing.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cUnitNamingNoncompliant: array[0..8] of string = (
    'unit lowerUnit;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cUnitNamingCompliant: array[0..8] of string = (
    'unit GoodUnit;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'implementation',
    '',
    'end.');

  cIdentifierTooShortNoncompliant: array[0..11] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  Pi = 3;',
    '',
    'implementation',
    '',
    'end.');

  cIdentifierTooShortCompliant: array[0..14] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  Total = 100;',
    '',
    'var',
    '  i: Integer;',
    '',
    'implementation',
    '',
    'end.');

procedure TRulesNamingTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesNamingTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesNamingTest.FirstById(
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


function TRulesNamingTest.NewClassNaming: TRuleBase;

begin
  Result := TRuleClassNaming.Create(TRuleMetadata.Make(cClassNamingId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewRecordNaming: TRuleBase;

begin
  Result := TRuleRecordNaming.Create(TRuleMetadata.Make(cRecordNamingId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewInterfaceNaming: TRuleBase;

begin
  Result := TRuleInterfaceNaming.Create(TRuleMetadata.Make(cInterfaceNamingId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewEnumNaming: TRuleBase;

begin
  Result := TRuleEnumNaming.Create(TRuleMetadata.Make(cEnumNamingId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewHelperNaming: TRuleBase;

begin
  Result := TRuleHelperNaming.Create(TRuleMetadata.Make(cHelperNamingId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewPointerNaming: TRuleBase;

begin
  Result := TRulePointerNaming.Create(TRuleMetadata.Make(cPointerNamingId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewAttributeNaming: TRuleBase;

begin
  Result := TRuleAttributeNaming.Create(TRuleMetadata.Make(cAttributeNamingId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewConstantNaming: TRuleBase;

begin
  Result := TRuleConstantNaming.Create(TRuleMetadata.Make(cConstantNamingId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewFieldNaming: TRuleBase;

begin
  Result := TRuleFieldNaming.Create(TRuleMetadata.Make(cFieldNamingId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewVariableNaming: TRuleBase;

begin
  Result := TRuleVariableNaming.Create(TRuleMetadata.Make(cVariableNamingId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewRoutineNaming: TRuleBase;

begin
  Result := TRuleRoutineNaming.Create(TRuleMetadata.Make(cRoutineNamingId, rtAst,
    rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewConstructorNaming: TRuleBase;

begin
  Result := TRuleConstructorNaming.Create(TRuleMetadata.Make(cConstructorNamingId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewUnitNaming: TRuleBase;

begin
  Result := TRuleUnitNaming.Create(TRuleMetadata.Make(cUnitNamingId, rtAst, rfAst,
    sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesNamingTest.NewIdentifierTooShort: TRuleBase;

begin
  Result := TRuleIdentifierTooShort.Create(TRuleMetadata.Make(cIdentifierTooShortId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


procedure TRulesNamingTest.CheckNamingRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aName, aPattern: string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at the type declaration line, column 1
    // (AST nodes carry no column), carrying [offending name, pattern] as args.
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
      AssertEquals('arg 0 is the offending name', aName,
        lc.Issues[k].MessageArgs[0]);
      AssertEquals('arg 1 is the required pattern', aPattern,
        lc.Issues[k].MessageArgs[1]);
    finally
      lc.Free;
    end;

    // Compliant: a conforming type => nothing flagged.
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


procedure TRulesNamingTest.ClassNamingPositions;

begin
  // Noncompliant: badClass (decl line 8) violates ^T[A-Z]...
  CheckNamingRuleSrc(NewClassNaming, NewClassNaming, cClassNamingId, 8,
    'badClass', cPatT,
    cClassNamingNoncompliant, cClassNamingCompliant);
end;


procedure TRulesNamingTest.RecordNamingPositions;

begin
  // Noncompliant: badRec (decl line 8) violates ^T[A-Z]...
  CheckNamingRuleSrc(NewRecordNaming, NewRecordNaming, cRecordNamingId, 8,
    'badRec', cPatT,
    cRecordNamingNoncompliant, cRecordNamingCompliant);
end;


procedure TRulesNamingTest.InterfaceNamingPositions;

begin
  // Noncompliant: TWrong (decl line 8) violates ^I[A-Z]... (T-prefix, not I).
  CheckNamingRuleSrc(NewInterfaceNaming, NewInterfaceNaming, cInterfaceNamingId, 8,
    'TWrong', cPatI,
    cInterfaceNamingNoncompliant, cInterfaceNamingCompliant);
end;


procedure TRulesNamingTest.EnumNamingPositions;

begin
  // Noncompliant: badEnum (decl line 8) violates ^T[A-Z]...
  CheckNamingRuleSrc(NewEnumNaming, NewEnumNaming, cEnumNamingId, 8,
    'badEnum', cPatT,
    cEnumNamingNoncompliant, cEnumNamingCompliant);
end;


procedure TRulesNamingTest.HelperNamingPositions;

begin
  // Noncompliant: TBadOne (decl line 8) violates ^T[A-Z][A-Za-z0-9]*Helper$.
  CheckNamingRuleSrc(NewHelperNaming, NewHelperNaming, cHelperNamingId, 8,
    'TBadOne', cPatHelper,
    cHelperNamingNoncompliant, cHelperNamingCompliant);
end;


procedure TRulesNamingTest.PointerNamingPositions;

begin
  // Noncompliant: pBad (decl line 8) violates ^P[A-Z]...
  CheckNamingRuleSrc(NewPointerNaming, NewPointerNaming, cPointerNamingId, 8,
    'pBad', cPatPointer,
    cPointerNamingNoncompliant, cPointerNamingCompliant);
end;


procedure TRulesNamingTest.AttributeNamingPositions;

begin
  // Noncompliant: badAttr (decl line 8) violates ^[A-Z]...Attribute$.
  CheckNamingRuleSrc(NewAttributeNaming, NewAttributeNaming, cAttributeNamingId, 8,
    'badAttr', cPatAttribute,
    cAttributeNamingNoncompliant, cAttributeNamingCompliant);
end;


procedure TRulesNamingTest.ConstantNamingPositions;

begin
  // Noncompliant: badConst (decl line 8) violates ^[A-Z][A-Za-z0-9_]*$.
  CheckNamingRuleSrc(NewConstantNaming, NewConstantNaming, cConstantNamingId, 8,
    'badConst', cPatConst,
    cConstantNamingNoncompliant, cConstantNamingCompliant);
end;


procedure TRulesNamingTest.FieldNamingPositions;

begin
  // Noncompliant: badField (decl line 9 — the field line inside the record)
  // violates ^F[A-Z]...
  CheckNamingRuleSrc(NewFieldNaming, NewFieldNaming, cFieldNamingId, 9,
    'badField', cPatField,
    cFieldNamingNoncompliant, cFieldNamingCompliant);
end;


procedure TRulesNamingTest.VariableNamingPositions;

begin
  // Noncompliant: badVar (decl line 8) violates ^[A-Z]...
  CheckNamingRuleSrc(NewVariableNaming, NewVariableNaming, cVariableNamingId, 8,
    'badVar', cPatVariable,
    cVariableNamingNoncompliant, cVariableNamingCompliant);
end;


procedure TRulesNamingTest.RoutineNamingPositions;

begin
  // Noncompliant: impl-section 'function doStuff' (decl line 9) violates
  // ^[A-Z]...; the simple name is reported.
  CheckNamingRuleSrc(NewRoutineNaming, NewRoutineNaming, cRoutineNamingId, 9,
    'doStuff', cPatRoutine,
    cRoutineNamingNoncompliant, cRoutineNamingCompliant);
end;


procedure TRulesNamingTest.ConstructorNamingPositions;

begin
  // Noncompliant: impl-section 'constructor TThing.make' (decl line 14)
  // violates ^Create...; the reported name is the
  // SIMPLE 'make', NOT the qualified 'TThing.make' (validates LastIdentifier).
  CheckNamingRuleSrc(NewConstructorNaming, NewConstructorNaming,
    cConstructorNamingId, 14, 'make', cPatCtor,
    cConstructorNamingNoncompliant, cConstructorNamingCompliant);
end;


procedure TRulesNamingTest.UnitNamingPositions;

begin
  // Noncompliant: unit 'lowerUnit' (the 'unit' line, 1) violates ^[A-Z]...
  CheckNamingRuleSrc(NewUnitNaming, NewUnitNaming, cUnitNamingId, 1,
    'lowerUnit', cPatUnit,
    cUnitNamingNoncompliant, cUnitNamingCompliant);
end;


procedure TRulesNamingTest.IdentifierTooShortPositions;

begin
  // Noncompliant: const 'Pi' (2 chars, decl line 8) is shorter than 3; the
  // second message arg is the minimum length ('3'), not a pattern. The
  // compliant fixture's allowlisted 'i' proves the allowlist suppresses it.
  CheckNamingRuleSrc(NewIdentifierTooShort, NewIdentifierTooShort,
    cIdentifierTooShortId, 8, 'Pi', cMinLenArg,
    cIdentifierTooShortNoncompliant, cIdentifierTooShortCompliant);
end;


procedure TRulesNamingTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all fourteen naming rules into the
  // GLOBAL registry (this is what the CLI process runs).
  AssertTrue('ClassNaming registered',
    RuleRegistry.FindById(cClassNamingId) <> nil);
  AssertTrue('RecordNaming registered',
    RuleRegistry.FindById(cRecordNamingId) <> nil);
  AssertTrue('InterfaceNaming registered',
    RuleRegistry.FindById(cInterfaceNamingId) <> nil);
  AssertTrue('EnumNaming registered',
    RuleRegistry.FindById(cEnumNamingId) <> nil);
  AssertTrue('HelperNaming registered',
    RuleRegistry.FindById(cHelperNamingId) <> nil);
  AssertTrue('PointerNaming registered',
    RuleRegistry.FindById(cPointerNamingId) <> nil);
  AssertTrue('AttributeNaming registered',
    RuleRegistry.FindById(cAttributeNamingId) <> nil);
  AssertTrue('ConstantNaming registered',
    RuleRegistry.FindById(cConstantNamingId) <> nil);
  AssertTrue('FieldNaming registered',
    RuleRegistry.FindById(cFieldNamingId) <> nil);
  AssertTrue('VariableNaming registered',
    RuleRegistry.FindById(cVariableNamingId) <> nil);
  AssertTrue('RoutineNaming registered',
    RuleRegistry.FindById(cRoutineNamingId) <> nil);
  AssertTrue('ConstructorNaming registered',
    RuleRegistry.FindById(cConstructorNamingId) <> nil);
  AssertTrue('UnitNaming registered',
    RuleRegistry.FindById(cUnitNamingId) <> nil);
  AssertTrue('IdentifierTooShort registered',
    RuleRegistry.FindById(cIdentifierTooShortId) <> nil);
end;


initialization
  RegisterTest(TRulesNamingTest);

end.
