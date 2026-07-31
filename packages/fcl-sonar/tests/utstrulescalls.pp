{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the call-based (SEM) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesCalls;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Calls, UtstFixtures;

type
  { SEM-tier Format-rule position + registration tests. }
  TRulesCallsTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts aRule fires once at aLine, column 1, with key rule.<aId>.message
    // and message args aArgs, and zero on the compliant fixture.
    procedure CheckCallRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aLine: Integer; const aArgs: array of string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewFormatArgumentType: TRuleBase;
    function NewFormatArgumentCount: TRuleBase;
    function NewValidFormatString: TRuleBase;
    function NewFreeAndNilArgument: TRuleBase;
    function NewConstructorOnInstanceVariable: TRuleBase;
    function NewStringListDuplicatesNeedsSorted: TRuleBase;
    function NewDestructorShouldOverrideDestroy: TRuleBase;
    function NewOverrideOnlyCallsInherited: TRuleBase;
    function NewIfThenNotShortCircuit: TRuleBase;
    function NewAssertWithoutMessage: TRuleBase;
    function NewDefaultFormatSettingsInDateFormat: TRuleBase;
    function NewExplicitDefaultArrayProperty: TRuleBase;
    function NewStringFirstCharByIndex: TRuleBase;
    function NewTListLastByIndex: TRuleBase;
    function NewRedundantInherited: TRuleBase;
    function NewImplicitTEncodingDefault: TRuleBase;
    function NewSingleOverloadOfMathFunction: TRuleBase;
  published
    procedure FormatArgumentTypePositions;
    procedure FormatArgumentCountPositions;
    procedure ValidFormatStringPositions;
    procedure FreeAndNilArgumentPositions;
    procedure ConstructorOnInstanceVariablePositions;
    procedure StringListDuplicatesNeedsSortedPositions;
    procedure DestructorShouldOverrideDestroyPositions;
    procedure OverrideOnlyCallsInheritedPositions;
    procedure IfThenNotShortCircuitPositions;
    procedure AssertWithoutMessagePositions;
    procedure DefaultFormatSettingsInDateFormatPositions;
    procedure ExplicitDefaultArrayPropertyPositions;
    procedure StringFirstCharByIndexPositions;
    procedure TListLastByIndexPositions;
    procedure RedundantInheritedPositions;
    procedure ImplicitTEncodingDefaultPositions;
    procedure SingleOverloadOfMathFunctionPositions;
    procedure CallsRulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cFormatArgumentTypeId = 'FormatArgumentType';
  cFormatArgumentCountId = 'FormatArgumentCount';
  cValidFormatStringId = 'ValidFormatString';
  cFreeAndNilArgumentId = 'FreeAndNilArgument';
  cConstructorOnInstanceVariableId = 'ConstructorOnInstanceVariable';
  cStringListDuplicatesNeedsSortedId = 'StringListDuplicatesNeedsSorted';
  cDestructorShouldOverrideDestroyId = 'DestructorShouldOverrideDestroy';
  cOverrideOnlyCallsInheritedId = 'OverrideOnlyCallsInherited';
  cIfThenNotShortCircuitId = 'IfThenNotShortCircuit';
  cAssertWithoutMessageId = 'AssertWithoutMessage';
  cDefaultFormatSettingsInDateFormatId = 'DefaultFormatSettingsInDateFormat';
  cExplicitDefaultArrayPropertyId = 'ExplicitDefaultArrayProperty';
  cStringFirstCharByIndexId = 'StringFirstCharByIndex';
  cTListLastByIndexId = 'TListLastByIndex';
  cRedundantInheritedId = 'RedundantInherited';
  cImplicitTEncodingDefaultId = 'ImplicitTEncodingDefault';
  cSingleOverloadOfMathFunctionId = 'SingleOverloadOfMathFunction';

  // Embedded AssertWithoutMessage fixtures: line i+1 == [i]. Noncompliant line
  // 8 is the 1-arg Assert (the probe-locked finding).
  cAssertWithoutMessageNoncompliant: array[0..9] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure CheckPositive(aValue: Integer);',
    'implementation',
    'procedure CheckPositive(aValue: Integer);',
    'begin',
    '  Assert(aValue > 0);',
    'end;',
    'end.');
  cAssertWithoutMessageCompliant: array[0..9] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'procedure CheckPositive(aValue: Integer);',
    'implementation',
    'procedure CheckPositive(aValue: Integer);',
    'begin',
    '  Assert(aValue > 0, ''aValue must be positive'');',
    'end;',
    'end.');

  // Embedded Calls-rule fixtures: line i+1 == [i].

  cFormatArgumentTypeNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses SysUtils;',
    'procedure UseIt;',
    'var',
    '  S: AnsiString;',
    '  Price: Double;',
    'begin',
    '  S := Format(''%d items'', [Price]);',
    'end;',
    'end.');

  cFormatArgumentTypeCompliant: array[0..13] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses SysUtils;',
    'procedure UseIt;',
    'var',
    '  S: AnsiString;',
    '  Count: Integer;',
    '  Price: Double;',
    'begin',
    '  S := Format(''%d items, %.2f total'', [Count, Price]);',
    'end;',
    'end.');

  cFormatArgumentCountNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses SysUtils;',
    'procedure UseIt;',
    'var',
    '  S: AnsiString;',
    '  Name: AnsiString;',
    'begin',
    '  S := Format(''%s = %d'', [Name]);',
    'end;',
    'end.');

  cFormatArgumentCountCompliant: array[0..13] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses SysUtils;',
    'procedure UseIt;',
    'var',
    '  S: AnsiString;',
    '  Name: AnsiString;',
    '  Value: Integer;',
    'begin',
    '  S := Format(''%s = %d'', [Name, Value]);',
    'end;',
    'end.');

  cValidFormatStringNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses SysUtils;',
    'procedure UseIt;',
    'var',
    '  S: AnsiString;',
    '  X: Integer;',
    'begin',
    '  S := Format(''value: %q'', [X]);',
    'end;',
    'end.');

  cValidFormatStringCompliant: array[0..12] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'implementation',
    'uses SysUtils;',
    'procedure UseIt;',
    'var',
    '  S: AnsiString;',
    '  Ratio: Double;',
    'begin',
    '  S := Format(''%-10.2f%%'', [Ratio]);',
    'end;',
    'end.');

  cFreeAndNilArgumentNoncompliant: array[0..15] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'type',
    '  TRec = record',
    '    X: Integer;',
    '  end;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  R: TRec;',
    'begin',
    '  FreeAndNil(R);',
    'end;',
    'end.');

  cFreeAndNilArgumentCompliant: array[0..11] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils, Classes;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  L: TStringList;',
    'begin',
    '  FreeAndNil(L);',
    'end;',
    'end.');

  cConstructorOnInstanceVariableNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TMyClass = class(TObject)',
    '    constructor Create;',
    '  end;',
    'implementation',
    'constructor TMyClass.Create;',
    'begin',
    'end;',
    'procedure UseIt;',
    'var',
    '  Obj: TMyClass;',
    'begin',
    '  Obj := TMyClass.Create;',
    '  Obj.Create;',
    'end;',
    'end.');

  cConstructorOnInstanceVariableCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TMyClass = class(TObject)',
    '    constructor Create;',
    '  end;',
    'implementation',
    'constructor TMyClass.Create;',
    'begin',
    'end;',
    'procedure UseIt;',
    'var',
    '  Obj: TMyClass;',
    'begin',
    '  Obj := TMyClass.Create;',
    'end;',
    'end.');

  cStringListDuplicatesNeedsSortedNoncompliant: array[0..12] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Classes;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  L: TStringList;',
    'begin',
    '  L := TStringList.Create;',
    '  L.Duplicates := dupIgnore;',
    'end;',
    'end.');

  cStringListDuplicatesNeedsSortedCompliant: array[0..13] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Classes;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  L: TStringList;',
    'begin',
    '  L := TStringList.Create;',
    '  L.Sorted := True;',
    '  L.Duplicates := dupIgnore;',
    'end;',
    'end.');

  cDestructorShouldOverrideDestroyNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFoo = class(TObject)',
    '    destructor Done;',
    '  end;',
    'implementation',
    'destructor TFoo.Done;',
    'begin',
    'end;',
    'end.');

  cDestructorShouldOverrideDestroyCompliant: array[0..11] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFoo = class(TObject)',
    '    destructor Destroy; override;',
    '  end;',
    'implementation',
    'destructor TFoo.Destroy;',
    'begin',
    'end;',
    'end.');

  cOverrideOnlyCallsInheritedNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TParent = class(TObject)',
    '    procedure DoIt(X: Integer); virtual;',
    '  end;',
    '  TChild = class(TParent)',
    '    procedure DoIt(X: Integer); override;',
    '  end;',
    'implementation',
    'procedure TParent.DoIt(X: Integer);',
    'begin',
    'end;',
    'procedure TChild.DoIt(X: Integer);',
    'begin',
    '  inherited DoIt(X);',
    'end;',
    'end.');

  cOverrideOnlyCallsInheritedCompliant: array[0..20] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TParent = class(TObject)',
    '    procedure DoIt(X: Integer); virtual;',
    '  end;',
    '  TChild = class(TParent)',
    '    FValue: Integer;',
    '    procedure DoIt(X: Integer); override;',
    '  end;',
    'implementation',
    'procedure TParent.DoIt(X: Integer);',
    'begin',
    'end;',
    'procedure TChild.DoIt(X: Integer);',
    'begin',
    '  inherited DoIt(X);',
    '  FValue := X;',
    'end;',
    'end.');

  cIfThenNotShortCircuitNoncompliant: array[0..16] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Math;',
    'type',
    '  TFoo = class(TObject)',
    '    Value: Integer;',
    '  end;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  P: TFoo;',
    '  R: Integer;',
    'begin',
    '  R := IfThen(Assigned(P), P.Value, 0);',
    'end;',
    'end.');

  cIfThenNotShortCircuitCompliant: array[0..12] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Math;',
    'implementation',
    'procedure UseIt;',
    'var',
    '  Cond: Boolean;',
    '  R: Integer;',
    'begin',
    '  R := IfThen(Cond, 10, 20);',
    'end;',
    'end.');

  cDefaultFormatSettingsInDateFormatNoncompliant: array[0..10] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'function Stamp: AnsiString;',
    'implementation',
    'function Stamp: AnsiString;',
    'begin',
    '  Result := FormatDateTime(''yyyy-mm-dd'', Now);',
    'end;',
    'end.');

  cDefaultFormatSettingsInDateFormatCompliant: array[0..12] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses SysUtils;',
    'function Stamp: AnsiString;',
    'implementation',
    'function Stamp: AnsiString;',
    'var',
    '  lSettings: TFormatSettings;',
    'begin',
    '  Result := FormatDateTime(''yyyy-mm-dd'', Now, lSettings);',
    'end;',
    'end.');

  cExplicitDefaultArrayPropertyNoncompliant: array[0..17] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFoo = class',
    '    function GetItem(aIndex: Integer): Integer;',
    '    property Items[aIndex: Integer]: Integer read GetItem; default;',
    '  end;',
    'function UseIt(aFoo: TFoo): Integer;',
    'implementation',
    'function TFoo.GetItem(aIndex: Integer): Integer;',
    'begin',
    'end;',
    'function UseIt(aFoo: TFoo): Integer;',
    'begin',
    '  Result := aFoo.Items[0];',
    'end;',
    'end.');

  cExplicitDefaultArrayPropertyCompliant: array[0..17] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TFoo = class',
    '    function GetItem(aIndex: Integer): Integer;',
    '    property Items[aIndex: Integer]: Integer read GetItem; default;',
    '  end;',
    'function UseIt(aFoo: TFoo): Integer;',
    'implementation',
    'function TFoo.GetItem(aIndex: Integer): Integer;',
    'begin',
    'end;',
    'function UseIt(aFoo: TFoo): Integer;',
    'begin',
    '  Result := aFoo[0];',
    'end;',
    'end.');

  cStringFirstCharByIndexNoncompliant: array[0..9] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function FirstChar(const aValue: string): Char;',
    'implementation',
    'function FirstChar(const aValue: string): Char;',
    'begin',
    '  Result := aValue[1];',
    'end;',
    'end.');

  cStringFirstCharByIndexCompliant: array[0..9] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'function NthChar(const aValue: string; aIndex: Integer): Char;',
    'implementation',
    'function NthChar(const aValue: string; aIndex: Integer): Char;',
    'begin',
    '  Result := aValue[aIndex];',
    'end;',
    'end.');

  cTListLastByIndexNoncompliant: array[0..11] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses',
    '  Classes;',
    'function LastItem(aList: TList): Pointer;',
    'implementation',
    'function LastItem(aList: TList): Pointer;',
    'begin',
    '  Result := aList[aList.Count - 1];',
    'end;',
    'end.');

  cTListLastByIndexCompliant: array[0..11] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses',
    '  Classes;',
    'function LastItem(aList: TList): Pointer;',
    'implementation',
    'function LastItem(aList: TList): Pointer;',
    'begin',
    '  Result := aList.Last;',
    'end;',
    'end.');

  cRedundantInheritedNoncompliant: array[0..18] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TParent = class(TObject)',
    '    procedure Init; virtual;',
    '  end;',
    '  TChild = class(TParent)',
    '    procedure Helper;',
    '  end;',
    'implementation',
    'procedure TParent.Init;',
    'begin',
    'end;',
    'procedure TChild.Helper;',
    'begin',
    '  inherited;',
    'end;',
    'end.');

  cRedundantInheritedCompliant: array[0..18] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'type',
    '  TParent = class(TObject)',
    '    procedure Init; virtual;',
    '  end;',
    '  TChild = class(TParent)',
    '    procedure Init; override;',
    '  end;',
    'implementation',
    'procedure TParent.Init;',
    'begin',
    'end;',
    'procedure TChild.Init;',
    'begin',
    '  inherited;',
    'end;',
    'end.');

  cImplicitTEncodingDefaultNoncompliant: array[0..14] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Classes;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  sl: TStringList;',
    'begin',
    '  sl := TStringList.Create;',
    '  sl.LoadFromFile(''data.txt'');',
    '  sl.Free;',
    'end;',
    'end.');

  cImplicitTEncodingDefaultCompliant: array[0..15] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Classes, SysUtils;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  sl: TStringList;',
    '  enc: TEncoding;',
    'begin',
    '  sl := TStringList.Create;',
    '  sl.LoadFromFile(''data.txt'', enc);',
    '  sl.Free;',
    'end;',
    'end.');

  cSingleOverloadOfMathFunctionNoncompliant: array[0..13] of string = (
    'unit noncompliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Math;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  s: Single;',
    'begin',
    '  s := 2.0;',
    '  s := Sqrt(s);',
    'end;',
    'end.');

  cSingleOverloadOfMathFunctionCompliant: array[0..13] of string = (
    'unit compliant;',
    '{$mode objfpc}{$H+}',
    'interface',
    'uses Math;',
    'procedure Run;',
    'implementation',
    'procedure Run;',
    'var',
    '  d: Double;',
    'begin',
    '  d := 2.0;',
    '  d := Sqrt(d);',
    'end;',
    'end.');

procedure TRulesCallsTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesCallsTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesCallsTest.FirstById(
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


function TRulesCallsTest.NewFormatArgumentType: TRuleBase;

begin
  Result := TRuleFormatArgumentType.Create(TRuleMetadata.Make(
    cFormatArgumentTypeId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewFormatArgumentCount: TRuleBase;

begin
  Result := TRuleFormatArgumentCount.Create(TRuleMetadata.Make(
    cFormatArgumentCountId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewValidFormatString: TRuleBase;

begin
  Result := TRuleValidFormatString.Create(TRuleMetadata.Make(
    cValidFormatStringId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewFreeAndNilArgument: TRuleBase;

begin
  Result := TRuleFreeAndNilArgument.Create(TRuleMetadata.Make(
    cFreeAndNilArgumentId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewConstructorOnInstanceVariable: TRuleBase;

begin
  Result := TRuleConstructorOnInstanceVariable.Create(TRuleMetadata.Make(
    cConstructorOnInstanceVariableId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewStringListDuplicatesNeedsSorted: TRuleBase;

begin
  Result := TRuleStringListDuplicatesNeedsSorted.Create(TRuleMetadata.Make(
    cStringListDuplicatesNeedsSortedId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesCallsTest.NewDestructorShouldOverrideDestroy: TRuleBase;

begin
  Result := TRuleDestructorShouldOverrideDestroy.Create(TRuleMetadata.Make(
    cDestructorShouldOverrideDestroyId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewOverrideOnlyCallsInherited: TRuleBase;

begin
  Result := TRuleOverrideOnlyCallsInherited.Create(TRuleMetadata.Make(
    cOverrideOnlyCallsInheritedId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewIfThenNotShortCircuit: TRuleBase;

begin
  Result := TRuleIfThenNotShortCircuit.Create(TRuleMetadata.Make(
    cIfThenNotShortCircuitId, rtSem, rfResolver, sevMajor, itBug, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewAssertWithoutMessage: TRuleBase;

begin
  Result := TRuleAssertWithoutMessage.Create(TRuleMetadata.Make(
    cAssertWithoutMessageId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewDefaultFormatSettingsInDateFormat: TRuleBase;

begin
  Result := TRuleDefaultFormatSettingsInDateFormat.Create(TRuleMetadata.Make(
    cDefaultFormatSettingsInDateFormatId, rtSem, rfResolver, sevMinor,
    itCodeSmell, cfHigh, True, ''));
end;


function TRulesCallsTest.NewExplicitDefaultArrayProperty: TRuleBase;

begin
  Result := TRuleExplicitDefaultArrayProperty.Create(TRuleMetadata.Make(
    cExplicitDefaultArrayPropertyId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesCallsTest.NewStringFirstCharByIndex: TRuleBase;

begin
  Result := TRuleStringFirstCharByIndex.Create(TRuleMetadata.Make(
    cStringFirstCharByIndexId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewTListLastByIndex: TRuleBase;

begin
  Result := TRuleTListLastByIndex.Create(TRuleMetadata.Make(
    cTListLastByIndexId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewRedundantInherited: TRuleBase;

begin
  Result := TRuleRedundantInherited.Create(TRuleMetadata.Make(
    cRedundantInheritedId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewImplicitTEncodingDefault: TRuleBase;

begin
  Result := TRuleImplicitTEncodingDefault.Create(TRuleMetadata.Make(
    cImplicitTEncodingDefaultId, rtSem, rfResolver, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesCallsTest.NewSingleOverloadOfMathFunction: TRuleBase;

begin
  Result := TRuleSingleOverloadOfMathFunction.Create(TRuleMetadata.Make(
    cSingleOverloadOfMathFunctionId, rtSem, rfResolver, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


procedure TRulesCallsTest.CheckCallRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aLine: Integer; const aArgs: array of string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k, m: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: one issue at aLine, column 1, carrying aArgs.
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
      AssertEquals('arg count', Length(aArgs),
        Length(lc.Issues[k].MessageArgs));
      for m := 0 to High(aArgs) do
        AssertEquals('arg ' + IntToStr(m), aArgs[m],
          lc.Issues[k].MessageArgs[m]);
    finally
      lc.Free;
    end;

    // Compliant: silent; the fixture must resolve clean.
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


procedure TRulesCallsTest.FormatArgumentTypePositions;

begin
  // Noncompliant: 'S := Format('%d items', [Price]);' where Price:Double (cast
  // arg line 11, probe-locked); %d needs an integer. Args are the offending
  // specifier letter then the resolved type name.
  CheckCallRuleSrc(NewFormatArgumentType, NewFormatArgumentType,
    cFormatArgumentTypeId, 11, ['d', 'Double'],
    cFormatArgumentTypeNoncompliant, cFormatArgumentTypeCompliant);
end;


procedure TRulesCallsTest.FormatArgumentCountPositions;

begin
  // Noncompliant: 'S := Format('%s = %d', [Name]);' (format-string line 11,
  // probe-locked); 2 specifiers require 2 args, 1 supplied. Args are the
  // required then supplied counts.
  CheckCallRuleSrc(NewFormatArgumentCount, NewFormatArgumentCount,
    cFormatArgumentCountId, 11, ['2', '1'],
    cFormatArgumentCountNoncompliant, cFormatArgumentCountCompliant);
end;


procedure TRulesCallsTest.ValidFormatStringPositions;

begin
  // Noncompliant: 'S := Format('value: %q', [X]);' (format-string line 11,
  // probe-locked); %q is not a valid FPC conversion. Arg is the offending
  // conversion text.
  CheckCallRuleSrc(NewValidFormatString, NewValidFormatString,
    cValidFormatStringId, 11, ['%q'],
    cValidFormatStringNoncompliant, cValidFormatStringCompliant);
end;


procedure TRulesCallsTest.FreeAndNilArgumentPositions;

begin
  // Noncompliant: 'FreeAndNil(R);' where R:TRec (a record) at line 14
  // (probe-locked) — a record is not a class instance.
  CheckCallRuleSrc(NewFreeAndNilArgument, NewFreeAndNilArgument,
    cFreeAndNilArgumentId, 14, ['TRec'],
    cFreeAndNilArgumentNoncompliant, cFreeAndNilArgumentCompliant);
end;


procedure TRulesCallsTest.ConstructorOnInstanceVariablePositions;

begin
  // Noncompliant: 'Obj.Create;' on an already-allocated instance at line 17
  // (probe-locked); the preceding 'Obj := TMyClass.Create;' is the legitimate
  // construction. Arg is the constructor name.
  CheckCallRuleSrc(NewConstructorOnInstanceVariable, NewConstructorOnInstanceVariable,
    cConstructorOnInstanceVariableId, 17, ['Create'],
    cConstructorOnInstanceVariableNoncompliant, cConstructorOnInstanceVariableCompliant);
end;


procedure TRulesCallsTest.StringListDuplicatesNeedsSortedPositions;

begin
  // Noncompliant: 'L.Duplicates := dupIgnore;' at line 11 (probe-locked) with
  // no 'L.Sorted := True' in the same routine. Arg is the enum value name.
  CheckCallRuleSrc(NewStringListDuplicatesNeedsSorted,
    NewStringListDuplicatesNeedsSorted,
    cStringListDuplicatesNeedsSortedId, 11, ['dupIgnore'],
    cStringListDuplicatesNeedsSortedNoncompliant, cStringListDuplicatesNeedsSortedCompliant);
end;


procedure TRulesCallsTest.DestructorShouldOverrideDestroyPositions;

begin
  // Noncompliant: 'destructor Done;' in a TFoo = class(TObject) at line 6
  // (probe-locked): a TObject-descendant destructor not named Destroy cannot be
  // reached by Free. Arg is the destructor name.
  CheckCallRuleSrc(NewDestructorShouldOverrideDestroy,
    NewDestructorShouldOverrideDestroy,
    cDestructorShouldOverrideDestroyId, 6, ['Done'],
    cDestructorShouldOverrideDestroyNoncompliant, cDestructorShouldOverrideDestroyCompliant);
end;


procedure TRulesCallsTest.OverrideOnlyCallsInheritedPositions;

begin
  // Noncompliant: 'TChild.DoIt' (override of TParent.DoIt) whose whole body is
  // 'inherited DoIt(X);'; the impl header is line 15 (probe-locked). Arg is the
  // implementation proc's qualified Decl.Name.
  CheckCallRuleSrc(NewOverrideOnlyCallsInherited, NewOverrideOnlyCallsInherited,
    cOverrideOnlyCallsInheritedId, 15, ['TChild.DoIt'],
    cOverrideOnlyCallsInheritedNoncompliant, cOverrideOnlyCallsInheritedCompliant);
end;


procedure TRulesCallsTest.IfThenNotShortCircuitPositions;

begin
  // Noncompliant: 'R := IfThen(Assigned(P), P.Value, 0);' at line 15
  // (probe-locked) — IfThen evaluates BOTH value args.
  CheckCallRuleSrc(NewIfThenNotShortCircuit, NewIfThenNotShortCircuit,
    cIfThenNotShortCircuitId, 15, ['P'],
    cIfThenNotShortCircuitNoncompliant, cIfThenNotShortCircuitCompliant);
end;


procedure TRulesCallsTest.AssertWithoutMessagePositions;

begin
  // Noncompliant: 'Assert(aValue > 0);' (1 arg, no message) at line 8
  // (probe-locked) — the raised EAssertionFailed carries no diagnostic text.
  // The message is a fixed string.
  CheckCallRuleSrc(NewAssertWithoutMessage, NewAssertWithoutMessage,
    cAssertWithoutMessageId, 8, [],
    cAssertWithoutMessageNoncompliant, cAssertWithoutMessageCompliant);
end;


procedure TRulesCallsTest.DefaultFormatSettingsInDateFormatPositions;

begin
  // Noncompliant: 'Result := FormatDateTime('yyyy-mm-dd', Now);' at line 9
  // (probe-locked) — the bound overload takes no TFormatSettings.
  CheckCallRuleSrc(NewDefaultFormatSettingsInDateFormat,
    NewDefaultFormatSettingsInDateFormat,
    cDefaultFormatSettingsInDateFormatId, 9, ['FormatDateTime'],
    cDefaultFormatSettingsInDateFormatNoncompliant, cDefaultFormatSettingsInDateFormatCompliant);
end;


procedure TRulesCallsTest.ExplicitDefaultArrayPropertyPositions;

begin
  // Noncompliant: 'Result := aFoo.Items[0];' at line 16 (probe-locked) where
  // Items is the default array property. Arg is the resolved property name.
  CheckCallRuleSrc(NewExplicitDefaultArrayProperty, NewExplicitDefaultArrayProperty,
    cExplicitDefaultArrayPropertyId, 16, ['Items'],
    cExplicitDefaultArrayPropertyNoncompliant, cExplicitDefaultArrayPropertyCompliant);
end;


procedure TRulesCallsTest.StringFirstCharByIndexPositions;

begin
  // Noncompliant: 'Result := aValue[1];' where aValue:string at line 8 (probe-
  // locked) — reading the first character by literal index; the index
  // const-folds to the low bound 1. The message is a fixed string.
  CheckCallRuleSrc(NewStringFirstCharByIndex, NewStringFirstCharByIndex,
    cStringFirstCharByIndexId, 8, [],
    cStringFirstCharByIndexNoncompliant, cStringFirstCharByIndexCompliant);
end;


procedure TRulesCallsTest.TListLastByIndexPositions;

begin
  // Noncompliant: 'Result := aList[aList.Count - 1];' where aList:Classes.TList
  // at line 10 (probe-locked); the Count-1 index is on the same receiver being
  // indexed. The message is a fixed string.
  CheckCallRuleSrc(NewTListLastByIndex, NewTListLastByIndex,
    cTListLastByIndexId, 10, [],
    cTListLastByIndexNoncompliant, cTListLastByIndexCompliant);
end;


procedure TRulesCallsTest.RedundantInheritedPositions;

begin
  // Noncompliant: 'TChild.Helper' has no overridable parent member (no TParent
  // Helper).
  CheckCallRuleSrc(NewRedundantInherited, NewRedundantInherited,
    cRedundantInheritedId, 17, [],
    cRedundantInheritedNoncompliant, cRedundantInheritedCompliant);
end;


procedure TRulesCallsTest.ImplicitTEncodingDefaultPositions;

begin
  // Noncompliant: 'sl.LoadFromFile('data.txt')' at line 12 (probe-locked) binds
  // the 1-arg TStrings.LoadFromFile overload, which omits the encoding
  // parameter while the 2-arg sibling exists. The message is a fixed string.
  CheckCallRuleSrc(NewImplicitTEncodingDefault, NewImplicitTEncodingDefault,
    cImplicitTEncodingDefaultId, 12, [],
    cImplicitTEncodingDefaultNoncompliant, cImplicitTEncodingDefaultCompliant);
end;


procedure TRulesCallsTest.SingleOverloadOfMathFunctionPositions;

begin
  // Noncompliant: 's := Sqrt(s);' at line 12 (probe-locked) — 's: Single' feeds
  // the call.
  CheckCallRuleSrc(NewSingleOverloadOfMathFunction, NewSingleOverloadOfMathFunction,
    cSingleOverloadOfMathFunctionId, 12, [],
    cSingleOverloadOfMathFunctionNoncompliant, cSingleOverloadOfMathFunctionCompliant);
end;


procedure TRulesCallsTest.CallsRulesSelfRegisterGlobally;

begin
  // The production initialization registered all SEVENTEEN SEM call rules into
  // the global registry.
  AssertTrue('FormatArgumentType registered',
    RuleRegistry.FindById(cFormatArgumentTypeId) <> nil);
  AssertTrue('FormatArgumentCount registered',
    RuleRegistry.FindById(cFormatArgumentCountId) <> nil);
  AssertTrue('ValidFormatString registered',
    RuleRegistry.FindById(cValidFormatStringId) <> nil);
  AssertTrue('FreeAndNilArgument registered',
    RuleRegistry.FindById(cFreeAndNilArgumentId) <> nil);
  AssertTrue('ConstructorOnInstanceVariable registered',
    RuleRegistry.FindById(cConstructorOnInstanceVariableId) <> nil);
  AssertTrue('StringListDuplicatesNeedsSorted registered',
    RuleRegistry.FindById(cStringListDuplicatesNeedsSortedId) <> nil);
  AssertTrue('DestructorShouldOverrideDestroy registered',
    RuleRegistry.FindById(cDestructorShouldOverrideDestroyId) <> nil);
  AssertTrue('OverrideOnlyCallsInherited registered',
    RuleRegistry.FindById(cOverrideOnlyCallsInheritedId) <> nil);
  AssertTrue('IfThenNotShortCircuit registered',
    RuleRegistry.FindById(cIfThenNotShortCircuitId) <> nil);
  AssertTrue('AssertWithoutMessage registered',
    RuleRegistry.FindById(cAssertWithoutMessageId) <> nil);
  AssertTrue('DefaultFormatSettingsInDateFormat registered',
    RuleRegistry.FindById(cDefaultFormatSettingsInDateFormatId) <> nil);
  AssertTrue('ExplicitDefaultArrayProperty registered',
    RuleRegistry.FindById(cExplicitDefaultArrayPropertyId) <> nil);
  AssertTrue('StringFirstCharByIndex registered',
    RuleRegistry.FindById(cStringFirstCharByIndexId) <> nil);
  AssertTrue('TListLastByIndex registered',
    RuleRegistry.FindById(cTListLastByIndexId) <> nil);
  AssertTrue('RedundantInherited registered',
    RuleRegistry.FindById(cRedundantInheritedId) <> nil);
  AssertTrue('ImplicitTEncodingDefault registered',
    RuleRegistry.FindById(cImplicitTEncodingDefaultId) <> nil);
  AssertTrue('SingleOverloadOfMathFunction registered',
    RuleRegistry.FindById(cSingleOverloadOfMathFunctionId) <> nil);
end;


initialization
  RegisterTest(TRulesCallsTest);

end.
