{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Delta tests for the dual-engine unused-declaration rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesUnusedDelta;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Config, FpSonar.Traversal, FpSonar.Engine,
  FpSonar.Rules.Unused, UtstFixtures,
  PasTree, FpSonar.Resolver, FpSonar.UseAnalysis;

type
  { Dual-engine precision tests, one per I/O-matrix row. }
  TRulesUnusedDeltaTest = class(TTestCase)
  private
    // Runs aRule over aFixture with useTier.resolution = prefer.
    procedure RunPrefer(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector);
    // Runs aRule over aSubject with aIndex attached and resolution = prefer.
    procedure RunPreferWithIndex(aRule: TRuleBase; const aSubject: string;
      aIndex: TFpSonarProjectIndex;
      const aCollector: TFpSonarIssueCollector);
    // Builds a project-wide index over aFiles, caller frees it.
    function BuildIndex(const aFiles: array of string): TFpSonarProjectIndex;
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function NewRule(const aId: string): TRuleBase;
  published
    procedure ShadowedFieldReportedOnce;
    procedure UncalledOverloadReportedOnce;
    procedure PublishedAccessorChainSilent;
    procedure AttributeOnlyReferenceSilent;
    procedure OverrideChainCallSilent;
    procedure GenericMemberUseSilent;
    procedure ProjectScopeGlobalAnsweredByIndex;
    procedure IncompleteClosureFallsBack;
    procedure ImplicitSystemOnlyUsesPreciseEngine;
  end;

implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

  cFieldId = 'RemoveUnusedField';
  cRoutineId = 'RemoveUnusedRoutine';
  cTypeId = 'RemoveUnusedType';
  cGlobalId = 'RemoveUnusedGlobalVariable';

  // Rows 1-7 are self-contained (no uses clause).

  cShadowField: array[0..26] of string = (
    'unit deltashadow;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FCount: Longint;',
    '  public',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Run;',
    'begin',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  FCount: Longint;',
    'begin',
    '  FCount := 1;',
    'end;',
    '',
    'end.');

  cOverloadRoutine: array[0..29] of string = (
    'unit deltaoverload;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Handle(aValue: Longint); overload;',
    '    procedure Handle(aFlag: Boolean); overload;',
    '  public',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Handle(aValue: Longint);',
    'begin',
    'end;',
    '',
    'procedure TThing.Handle(aFlag: Boolean);',
    'begin',
    'end;',
    '',
    'procedure TThing.Run;',
    'begin',
    '  Handle(1);',
    'end;',
    '',
    'end.');

  cPublishedRtti: array[0..23] of string = (
    'unit deltapublished;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  {$M+}',
    '  TThing = class',
    '  private',
    '    FCount: Longint;',
    '    function GetCount: Longint;',
    '  published',
    '    property Count: Longint read GetCount write FCount;',
    '  end;',
    '  {$M-}',
    '',
    'implementation',
    '',
    'function TThing.GetCount: Longint;',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'end.');

  // A locally declared TCustomAttribute is rejected by the resolver's
  // attribute-class check.
  cAttributeOnly: array[0..36] of string = (
    'unit deltaattribute;',
    '{$mode objfpc}{$H+}',
    '{$modeswitch prefixedattributes}',
    '',
    'interface',
    '',
    'type',
    '  TCustomAttribute = class',
    '  end;',
    '',
    'implementation',
    '',
    'type',
    '  TMarkAttribute = class(TCustomAttribute)',
    '  end;',
    '',
    '  [TMarkAttribute]',
    '  TWorker = class',
    '  public',
    '    procedure Go;',
    '  end;',
    '',
    'procedure TWorker.Go;',
    'begin',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  lWorker: TWorker;',
    'begin',
    '  lWorker := TWorker.Create;',
    '  lWorker.Go;',
    'end;',
    '',
    'initialization',
    '  Drive;',
    'end.');

  cOverrideChain: array[0..33] of string = (
    'unit deltaoverride;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class',
    '  public',
    '    procedure Run; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  private',
    '    procedure Helper;',
    '  public',
    '    procedure Run; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Helper;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Run;',
    'begin',
    '  Helper;',
    'end;',
    '',
    'end.');

  cGenericMember: array[0..32] of string = (
    'unit deltageneric;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  generic TBox<T> = class',
    '  private',
    '    FValue: T;',
    '  public',
    '    procedure Store(aValue: T);',
    '  end;',
    '',
    '  TIntBox = specialize TBox<Longint>;',
    '',
    'implementation',
    '',
    'procedure TBox.Store(aValue: T);',
    'begin',
    '  FValue := aValue;',
    'end;',
    '',
    'procedure Drive;',
    'var',
    '  lBox: TIntBox;',
    'begin',
    '  lBox := TIntBox.Create;',
    '  lBox.Store(3);',
    'end;',
    '',
    'initialization',
    '  Drive;',
    'end.');

  cProjectLive: array[0..10] of string = (
    'unit deltaglobalsubject;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GDeltaLive: Longint;',
    '',
    'implementation',
    '',
    'end.');

  cProjectDead: array[0..10] of string = (
    'unit deltaglobaldead;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GDeltaDead: Longint;',
    '',
    'implementation',
    '',
    'end.');

  cProjectUser: array[0..15] of string = (
    'unit deltaglobaluser;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  deltaglobalsubject;',
    '',
    'implementation',
    '',
    'procedure Touch;',
    'begin',
    '  GDeltaLive := 1;',
    'end;',
    '',
    'end.');

  cIncompleteDep: array[0..13] of string = (
    'unit deltaincompletedep;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TDepThing = class',
    '  public',
    '    Value: Longint;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  // The row-2 overload shape behind a uses clause: the dependency is parsed
  // interface-only.
  cIncompleteSubject: array[0..33] of string = (
    'unit deltaincomplete;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  deltaincompletedep;',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Handle(aValue: Longint); overload;',
    '    procedure Handle(aFlag: Boolean); overload;',
    '  public',
    '    Dep: TDepThing;',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Handle(aValue: Longint);',
    'begin',
    'end;',
    '',
    'procedure TThing.Handle(aFlag: Boolean);',
    'begin',
    'end;',
    '',
    'procedure TThing.Run;',
    'begin',
    '  Handle(1);',
    'end;',
    '',
    'end.');

  // The row-2 overload shape with no written uses clause at all: the only
  // entry in the closure is the implicit System import.
  cImplicitSystemOnly: array[0..29] of string = (
    'unit deltaimplicitsystem;',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    procedure Handle(aValue: Longint); overload;',
    '    procedure Handle(aFlag: Boolean); overload;',
    '  public',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TThing.Handle(aValue: Longint);',
    'begin',
    'end;',
    '',
    'procedure TThing.Handle(aFlag: Boolean);',
    'begin',
    'end;',
    '',
    'procedure TThing.Run;',
    'begin',
    '  Handle(1);',
    'end;',
    '',
    'end.');

procedure TRulesUnusedDeltaTest.RunPrefer(aRule: TRuleBase;
  const aFixture: string; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lConfig: TFpSonarConfig;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lConfig := TFpSonarConfig.Default;
    lConfig.UseTierResolution := utrPrefer;
    lEngine.Config := lConfig;
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.RunPreferWithIndex(aRule: TRuleBase;
  const aSubject: string; aIndex: TFpSonarProjectIndex;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;
  lConfig: TFpSonarConfig;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lConfig := TFpSonarConfig.Default;
    lConfig.UseTierResolution := utrPrefer;
    lEngine.Config := lConfig;
    lEngine.ProjectIndex := aIndex;
    lEngine.Analyze(aSubject, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesUnusedDeltaTest.BuildIndex(
  const aFiles: array of string): TFpSonarProjectIndex;

begin
  Result := BuildProjectIndex(aFiles, cMode, cDefines, [], []);
end;


function TRulesUnusedDeltaTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesUnusedDeltaTest.FirstById(
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


function TRulesUnusedDeltaTest.NewRule(const aId: string): TRuleBase;

var
  lMeta: TRuleMetadata;

begin
  // Metadata mirrors the unit's self-registration (rtUse / rfAst / Minor /
  // CodeSmell / cfHigh); empty key defaults to rule.<RuleId>.message.
  lMeta := TRuleMetadata.Make(aId, rtUse, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, '');
  if aId = cFieldId then
    Result := TRuleRemoveUnusedField.Create(lMeta)
  else if aId = cRoutineId then
    Result := TRuleRemoveUnusedRoutine.Create(lMeta)
  else if aId = cTypeId then
    Result := TRuleRemoveUnusedType.Create(lMeta)
  else
    Result := TRuleRemoveUnusedGlobalVariable.Create(lMeta);
end;


procedure TRulesUnusedDeltaTest.ShadowedFieldReportedOnce;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // The class-private FCount (line 9) is never referenced; the routine-local
  // FCount (line 22) is assigned. Only the field is flagged.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cFieldId),
        lFix.Add('deltashadow.pas', cShadowField), lc);
      AssertEquals('one field finding', 1, CountById(lc, cFieldId));
      k := FirstById(lc, cFieldId);
      AssertEquals('at the field declaration line', 9, lc.Issues[k].StartLine);
      AssertEquals('naming the field', 'FCount', lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.UncalledOverloadReportedOnce;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lRes: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lUnused: TPasElementArray;
  lFile: string;
  lListed: Boolean;
  i: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lFile := lFix.Add('deltaoverload.pas', cOverloadRoutine);
    { Handle(Longint) (line 9) is called from Run, Handle(Boolean) (line 10) is
      not: the precise engine separates the two, and the member enumeration
      descends into the TPasOverloadedProc the plain parse collapsed them
      into. }
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cRoutineId), lFile, lc);
      AssertEquals('one routine finding', 1, CountById(lc, cRoutineId));
      i := FirstById(lc, cRoutineId);
      AssertEquals('at the uncalled overload line', 10, lc.Issues[i].StartLine);
      AssertEquals('naming the routine', 'Handle', lc.Issues[i].MessageArgs[0]);
    finally
      lc.Free;
    end;

    // Given a complete closure the precise analysis does separate the overloads.
    lRes := TFpSonarResolver.Create;
    try
      AssertTrue('the overload fixture resolves',
        lRes.BuildFor(lFile, cMode, cDefines, [], [], lDiag));
      lUse := TFpSonarUseAnalysis.Create(lRes);
      try
        AssertTrue('the precise query answers on a complete closure',
          lUse.TryUnusedDeclarations(lUnused));
        lListed := False;
        for i := 0 to High(lUnused) do
          lListed := lListed or ((lUnused[i] is TPasProcedure)
            and SameText(lUnused[i].Name, 'Handle')
            and (lRes.SourceRow(lUnused[i]) = 10));
        AssertTrue('the uncalled overload is listed at line 10', lListed);
      finally
        lUse.Free;
      end;
    finally
      lRes.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.PublishedAccessorChainSilent;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lFile: string;

begin
  // A published property reaches its private getter and its backing field
  // through the read/write specifiers alone: both stay silent.
  lFix := TTempFixtures.Create;
  try
    lFile := lFix.Add('deltapublished.pas', cPublishedRtti);
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cRoutineId), lFile, lc);
      AssertEquals('published read accessor is not unused', 0,
        CountById(lc, cRoutineId));
    finally
      lc.Free;
    end;

    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cFieldId), lFile, lc);
      AssertEquals('published write target is not unused', 0,
        CountById(lc, cFieldId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.AttributeOnlyReferenceSilent;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  { TMarkAttribute (line 14) is named only by the attribute on TWorker. The
    fixture does not resolve (DW-405), so the name engine answers and this row
    ships untested against the precise engine. }
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cTypeId),
        lFix.Add('deltaattribute.pas', cAttributeOnly), lc);
      AssertEquals('an attribute-only reference keeps the type', 0,
        CountById(lc, cTypeId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.OverrideChainCallSilent;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // TDerived.Helper (line 14) is called only from TDerived.Run, an override.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cRoutineId),
        lFix.Add('deltaoverride.pas', cOverrideChain), lc);
      AssertEquals('a call from an override keeps the helper', 0,
        CountById(lc, cRoutineId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.GenericMemberUseSilent;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  // TBox.FValue (line 9) is assigned only inside the generic's own method,
  // and TBox is specialized as TIntBox, which Drive uses.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cFieldId),
        lFix.Add('deltageneric.pas', cGenericMember), lc);
      AssertEquals('a generic-body use keeps the member', 0,
        CountById(lc, cFieldId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.ProjectScopeGlobalAnsweredByIndex;

var
  lIndex: TFpSonarProjectIndex;
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  lLive, lDead: string;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    lLive := lFix.Add('deltaglobalsubject.pas', cProjectLive);
    lDead := lFix.Add('deltaglobaldead.pas', cProjectDead);
    lIndex := BuildIndex([lLive, lDead,
      lFix.Add('deltaglobaluser.pas', cProjectUser)]);
    try
      // GDeltaLive is referenced by a second unit, which only the project index
      // knows: the precise engine never sees an interface-section declaration.
      lc := TFpSonarIssueCollector.Create;
      try
        RunPreferWithIndex(NewRule(cGlobalId), lLive, lIndex, lc);
        AssertEquals('a cross-unit reference keeps the global', 0,
          CountById(lc, cGlobalId));
      finally
        lc.Free;
      end;

      // The same run over an unreferenced global still reports it, proving the
      // project scope is answered by the index.
      lc := TFpSonarIssueCollector.Create;
      try
        RunPreferWithIndex(NewRule(cGlobalId), lDead, lIndex, lc);
        AssertEquals('one global finding', 1, CountById(lc, cGlobalId));
        k := FirstById(lc, cGlobalId);
        AssertEquals('at its declaration line', 7, lc.Issues[k].StartLine);
        AssertEquals('naming the global', 'GDeltaDead',
          lc.Issues[k].MessageArgs[0]);
      finally
        lc.Free;
      end;
    finally
      lIndex.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.IncompleteClosureFallsBack;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    lFix.Add('deltaincompletedep.pas', cIncompleteDep);
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cRoutineId),
        lFix.Add('deltaincomplete.pas', cIncompleteSubject), lc);
      AssertEquals('the fallback answers on an incomplete closure', 0,
        CountById(lc, cRoutineId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesUnusedDeltaTest.ImplicitSystemOnlyUsesPreciseEngine;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  // Handle(Longint) (line 9) is called from Run, Handle(Boolean) (line 10) is
  // not: only the precise engine separates the two.
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunPrefer(NewRule(cRoutineId),
        lFix.Add('deltaimplicitsystem.pas', cImplicitSystemOnly), lc);
      AssertEquals('one routine finding', 1, CountById(lc, cRoutineId));
      k := FirstById(lc, cRoutineId);
      AssertEquals('at the uncalled overload line', 10, lc.Issues[k].StartLine);
      AssertEquals('naming the routine', 'Handle', lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


initialization
  RegisterTest(TRulesUnusedDeltaTest);

end.
