{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the class-hygiene (AST) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesClasses;

{ The three AST-tier class-hygiene rules }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Classes, UtstFixtures;

type
  { AST-tier class-hygiene-rule position + registration tests. }
  TRulesClassesTest = class(TTestCase)
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
    // args [aArg]; and zero on the compliant fixture. Fixtures supplied inline
    // (one array element per source line) and materialised to a temp dir.
    procedure CheckClassRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArg: string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule (metadata mirrors the
    // unit's self-registration; empty key defaults to rule.<RuleId>.message).
    function NewVisibilityAscendingOrder: TRuleBase;
    function NewDeclarationsFollowVisibilityOrder: TRuleBase;
    function NewFieldsNotPublic: TRuleBase;
    function NewFileNotTooManyClasses: TRuleBase;
    function NewInterfaceNotEmpty: TRuleBase;
    function NewInterfaceUniqueGuid: TRuleBase;
    function NewConstructorInherited: TRuleBase;
    function NewDestructorInherited: TRuleBase;
    function NewTopLevelClassInheritsTObject: TRuleBase;
  published
    procedure VisibilityAscendingOrderPositions;
    procedure DeclarationsFollowVisibilityOrderPositions;
    procedure FieldsNotPublicPositions;
    procedure FileNotTooManyClassesPositions;
    procedure InterfaceNotEmptyPositions;
    procedure InterfaceUniqueGuidPositions;
    procedure InterfaceUniqueGuidDuplicateFlagsBoth;
    procedure ConstructorInheritedPositions;
    procedure DestructorInheritedPositions;
    procedure TopLevelClassInheritsTObjectPositions;
    procedure RulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cVisibilityAscendingOrderId = 'VisibilityAscendingOrder';
  cDeclarationsFollowVisibilityOrderId = 'DeclarationsFollowVisibilityOrder';
  cFieldsNotPublicId = 'FieldsNotPublic';
  cFileNotTooManyClassesId = 'FileNotTooManyClasses';
  cInterfaceNotEmptyId = 'InterfaceNotEmpty';
  cInterfaceUniqueGuidId = 'InterfaceUniqueGuid';
  cConstructorInheritedId = 'ConstructorInherited';
  cDestructorInheritedId = 'DestructorInherited';
  cTopLevelClassInheritsTObjectId = 'TopLevelClassInheritsTObject';

  // Embedded class-hygiene-rule fixtures (Approach A rollout): line i+1 == [i].

  cVisibilityAscendingOrderNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  public',
    '    procedure DoPublic;',
    '  private',
    '    FName: string;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoPublic;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cVisibilityAscendingOrderCompliant: array[0..21] of string = (
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
    '    procedure DoPublic;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoPublic;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cDeclarationsFollowVisibilityOrderNoncompliant: array[0..15] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    property Name: string;',
    '    FName: string;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDeclarationsFollowVisibilityOrderCompliant: array[0..22] of string = (
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
    '    procedure SetName(const aValue: string);',
    '    property Name: string read FName write SetName;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.SetName(const aValue: string);',
    '',
    'begin',
    '  FName := aValue;',
    'end;',
    '',
    'end.');

  cFieldsNotPublicNoncompliant: array[0..14] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  public',
    '    FName: string;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cFieldsNotPublicCompliant: array[0..16] of string = (
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
    '    property Name: string read FName;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cFileNotTooManyClassesNoncompliant: array[0..16] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TC1 = class(TObject) end;',
    '  TC2 = class(TObject) end;',
    '  TC3 = class(TObject) end;',
    '  TC4 = class(TObject) end;',
    '  TC5 = class(TObject) end;',
    '  TC6 = class(TObject) end;',
    '',
    'implementation',
    '',
    'end.');

  cFileNotTooManyClassesCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TC1 = class(TObject) end;',
    '  TC2 = class(TObject) end;',
    '  TC3 = class(TObject) end;',
    '  TC4 = class(TObject) end;',
    '  TC5 = class(TObject) end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceNotEmptyNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IEmpty = interface',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceNotEmptyCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IThing = interface',
    '    procedure DoIt;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceUniqueGuidNoncompliant: array[0..13] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IService = interface',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceUniqueGuidCompliant: array[0..20] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  {$interfaces corba}',
    '  ICorba = interface',
    '    procedure A;',
    '  end;',
    '',
    '  {$interfaces com}',
    '  IComWithGuid = interface',
    '    [''{12345678-1234-1234-1234-123456789ABC}'']',
    '    procedure B;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cConstructorInheritedNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    'begin',
    '  // no inherited call',
    'end;',
    '',
    'end.');

  cConstructorInheritedCompliant: array[0..25] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    constructor Create;',
    '    class constructor InitClass;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'class constructor TWidget.InitClass;',
    'begin',
    '  // a class constructor never chains to inherited; must not be flagged',
    'end;',
    '',
    'end.');

  cDestructorInheritedNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    'begin',
    '  // no inherited call',
    'end;',
    '',
    'end.');

  cDestructorInheritedCompliant: array[0..25] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    destructor Destroy; override;',
    '    class destructor DoneClass;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    'begin',
    '  inherited Destroy;',
    'end;',
    '',
    'class destructor TWidget.DoneClass;',
    'begin',
    '  // a class destructor never chains to inherited; must not be flagged',
    'end;',
    '',
    'end.');

  cTopLevelClassInheritsTObjectNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFoo = class',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cTopLevelClassInheritsTObjectCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBar = class(TObject)',
    '  public',
    '    type',
    '      TInner = class',
    '      end;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceUniqueGuidDuplicate: array[0..19] of string = (
    'unit Duplicate;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IAlpha = interface',
    '    [''{11111111-1111-1111-1111-111111111111}'']',
    '    procedure A;',
    '  end;',
    '',
    '  IBeta = interface',
    '    [''{11111111-1111-1111-1111-111111111111}'']',
    '    procedure B;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

procedure TRulesClassesTest.RunRule(aRule: TRuleBase; const aFixture: string;
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


function TRulesClassesTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesClassesTest.FirstById(
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


function TRulesClassesTest.NewVisibilityAscendingOrder: TRuleBase;

begin
  Result := TRuleVisibilityAscendingOrder.Create(TRuleMetadata.Make(
    cVisibilityAscendingOrderId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesClassesTest.NewDeclarationsFollowVisibilityOrder: TRuleBase;

begin
  Result := TRuleDeclarationsFollowVisibilityOrder.Create(TRuleMetadata.Make(
    cDeclarationsFollowVisibilityOrderId, rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesClassesTest.NewFieldsNotPublic: TRuleBase;

begin
  Result := TRuleFieldsNotPublic.Create(TRuleMetadata.Make(cFieldsNotPublicId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewFileNotTooManyClasses: TRuleBase;

begin
  Result := TRuleFileNotTooManyClasses.Create(TRuleMetadata.Make(
    cFileNotTooManyClassesId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesClassesTest.NewInterfaceNotEmpty: TRuleBase;

begin
  Result := TRuleInterfaceNotEmpty.Create(TRuleMetadata.Make(cInterfaceNotEmptyId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewInterfaceUniqueGuid: TRuleBase;

begin
  Result := TRuleInterfaceUniqueGuid.Create(TRuleMetadata.Make(cInterfaceUniqueGuidId,
    rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesClassesTest.NewConstructorInherited: TRuleBase;

begin
  Result := TRuleConstructorInherited.Create(TRuleMetadata.Make(
    cConstructorInheritedId, rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesClassesTest.NewDestructorInherited: TRuleBase;

begin
  Result := TRuleDestructorInherited.Create(TRuleMetadata.Make(cDestructorInheritedId,
    rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesClassesTest.NewTopLevelClassInheritsTObject: TRuleBase;

begin
  Result := TRuleTopLevelClassInheritsTObject.Create(TRuleMetadata.Make(
    cTopLevelClassInheritsTObjectId, rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


procedure TRulesClassesTest.CheckClassRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArg: string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: exactly one issue at the offending member's line, column 1
    // (AST nodes carry no column), carrying [aArg] as the single message arg.
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
      AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
      AssertEquals('arg 0 is the offending name/keyword', aArg,
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;

    // Compliant: a conforming class => nothing flagged.
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


procedure TRulesClassesTest.VisibilityAscendingOrderPositions;

begin
  // Noncompliant: a 'private' section follows 'public'; the first member of the
  // out-of-order section (FName, line 12) is flagged, arg the section keyword.
  CheckClassRuleSrc(NewVisibilityAscendingOrder, NewVisibilityAscendingOrder,
    cVisibilityAscendingOrderId, 12, 'private',
    cVisibilityAscendingOrderNoncompliant, cVisibilityAscendingOrderCompliant);
end;


procedure TRulesClassesTest.DeclarationsFollowVisibilityOrderPositions;

begin
  // Noncompliant: a field (FName, line 11) follows a property in one section;
  // the out-of-phase member is flagged, arg its Name.
  CheckClassRuleSrc(NewDeclarationsFollowVisibilityOrder,
    NewDeclarationsFollowVisibilityOrder,
    cDeclarationsFollowVisibilityOrderId, 11, 'FName',
    cDeclarationsFollowVisibilityOrderNoncompliant, cDeclarationsFollowVisibilityOrderCompliant);
end;


procedure TRulesClassesTest.FieldsNotPublicPositions;

begin
  // Noncompliant: a public field (FName, line 10) is flagged, arg its Name.
  CheckClassRuleSrc(NewFieldsNotPublic, NewFieldsNotPublic, cFieldsNotPublicId, 10,
    'FName',
    cFieldsNotPublicNoncompliant, cFieldsNotPublicCompliant);
end;


procedure TRulesClassesTest.FileNotTooManyClassesPositions;

begin
  // Noncompliant: 6 classes (> 5) flagged once at the unit line (line 1),
  // arg the class count "6"; compliant (5 classes) flags nothing.
  CheckClassRuleSrc(NewFileNotTooManyClasses, NewFileNotTooManyClasses,
    cFileNotTooManyClassesId, 1, '6',
    cFileNotTooManyClassesNoncompliant, cFileNotTooManyClassesCompliant);
end;


procedure TRulesClassesTest.InterfaceNotEmptyPositions;

begin
  // Noncompliant: an empty interface (IEmpty, line 8) is flagged, arg its name.
  CheckClassRuleSrc(NewInterfaceNotEmpty, NewInterfaceNotEmpty,
    cInterfaceNotEmptyId, 8, 'IEmpty',
    cInterfaceNotEmptyNoncompliant, cInterfaceNotEmptyCompliant);
end;


procedure TRulesClassesTest.InterfaceUniqueGuidPositions;

begin
  // Noncompliant: a COM interface with no GUID (IService, line 8) is flagged;
  // compliant pairs a CORBA interface with no GUID (skipped) and a COM
  // interface with a distinct GUID — both yield nothing.
  CheckClassRuleSrc(NewInterfaceUniqueGuid, NewInterfaceUniqueGuid,
    cInterfaceUniqueGuidId, 8, 'IService',
    cInterfaceUniqueGuidNoncompliant, cInterfaceUniqueGuidCompliant);
end;


procedure TRulesClassesTest.InterfaceUniqueGuidDuplicateFlagsBoth;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  i, lAlpha, lBeta: Integer;

begin
  // The duplicate-GUID half of InterfaceUniqueGuid (collect GUID literals across
  // the unit and report duplicates): two COM interfaces sharing one GUID each emit
  // one issue, in declaration order, arg = interface name. CheckClassRuleSrc cannot
  // cover this (it asserts a single issue), so verify the pair explicitly.
  lAlpha := -1;
  lBeta := -1;
  lFix := TTempFixtures.Create;
  try
  lc := TFpSonarIssueCollector.Create;
  try
    RunRule(NewInterfaceUniqueGuid,
      lFix.Add('duplicate.pas', cInterfaceUniqueGuidDuplicate), lc);
    AssertEquals('two duplicate-GUID issues', 2,
      CountById(lc, cInterfaceUniqueGuidId));
    for i := 0 to lc.Count - 1 do
      if lc.Issues[i].RuleId = cInterfaceUniqueGuidId then
        begin
          if lc.Issues[i].MessageArgs[0] = 'IAlpha' then
            lAlpha := i
          else if lc.Issues[i].MessageArgs[0] = 'IBeta' then
            lBeta := i;
        end;
    AssertTrue('IAlpha flagged', lAlpha >= 0);
    AssertTrue('IBeta flagged', lBeta >= 0);
    AssertEquals('IAlpha at its declaration line', 8,
      lc.Issues[lAlpha].StartLine);
    AssertEquals('IAlpha col 1', 1, lc.Issues[lAlpha].StartCol);
    AssertEquals('IBeta at its declaration line', 13,
      lc.Issues[lBeta].StartLine);
    AssertEquals('IBeta col 1', 1, lc.Issues[lBeta].StartCol);
  finally
    lc.Free;
  end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesClassesTest.ConstructorInheritedPositions;

begin
  // Noncompliant: a constructor implementation (line 15) with no inherited call
  // is flagged, arg its qualified name; compliant calls inherited (and its
  // class constructor is excluded) => nothing.
  CheckClassRuleSrc(NewConstructorInherited, NewConstructorInherited,
    cConstructorInheritedId, 15, 'TWidget.Create',
    cConstructorInheritedNoncompliant, cConstructorInheritedCompliant);
end;


procedure TRulesClassesTest.DestructorInheritedPositions;

begin
  // Noncompliant: a destructor implementation (line 15) with no inherited call
  // is flagged; compliant calls inherited (and its class destructor is
  // excluded) => nothing.
  CheckClassRuleSrc(NewDestructorInherited, NewDestructorInherited,
    cDestructorInheritedId, 15, 'TWidget.Destroy',
    cDestructorInheritedNoncompliant, cDestructorInheritedCompliant);
end;


procedure TRulesClassesTest.TopLevelClassInheritsTObjectPositions;

begin
  // Noncompliant: a top-level ancestor-less class (TFoo = class, line 8) is
  // flagged; compliant has an explicit ancestor and a nested ancestor-less
  // class (not top-level) => nothing.
  CheckClassRuleSrc(NewTopLevelClassInheritsTObject,
    NewTopLevelClassInheritsTObject, cTopLevelClassInheritsTObjectId, 8, 'TFoo',
    cTopLevelClassInheritsTObjectNoncompliant, cTopLevelClassInheritsTObjectCompliant);
end;


procedure TRulesClassesTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all nine class-hygiene rules into
  // the GLOBAL registry (this is what the CLI process runs).
  AssertTrue('VisibilityAscendingOrder registered',
    RuleRegistry.FindById(cVisibilityAscendingOrderId) <> nil);
  AssertTrue('DeclarationsFollowVisibilityOrder registered',
    RuleRegistry.FindById(cDeclarationsFollowVisibilityOrderId) <> nil);
  AssertTrue('FieldsNotPublic registered',
    RuleRegistry.FindById(cFieldsNotPublicId) <> nil);
  AssertTrue('FileNotTooManyClasses registered',
    RuleRegistry.FindById(cFileNotTooManyClassesId) <> nil);
  AssertTrue('InterfaceNotEmpty registered',
    RuleRegistry.FindById(cInterfaceNotEmptyId) <> nil);
  AssertTrue('InterfaceUniqueGuid registered',
    RuleRegistry.FindById(cInterfaceUniqueGuidId) <> nil);
  AssertTrue('ConstructorInherited registered',
    RuleRegistry.FindById(cConstructorInheritedId) <> nil);
  AssertTrue('DestructorInherited registered',
    RuleRegistry.FindById(cDestructorInheritedId) <> nil);
  AssertTrue('TopLevelClassInheritsTObject registered',
    RuleRegistry.FindById(cTopLevelClassInheritsTObjectId) <> nil);
end;


initialization
  RegisterTest(TRulesClassesTest);

end.
