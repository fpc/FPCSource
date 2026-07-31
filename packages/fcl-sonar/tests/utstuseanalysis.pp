{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the precise use-analysis wrapper

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstUseAnalysis;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree,
  FpSonar.Types, FpSonar.Resolver, FpSonar.UseAnalysis,
  UtstFixtures;

type
  { Use-analysis wrapper tests. }
  TUseAnalysisTest = class(TTestCase)
  private
    FFix: TTempFixtures;
    // The named declaration in aModule's interface section, or nil.
    function FindDecl(aModule: TPasModule; const aName: string): TPasElement;
    // The named member of the named class in aModule's interface section, or nil.
    function FindMember(aModule: TPasModule;
      const aTypeName, aMemberName: string): TPasElement;
    // The named local of an implementation-section routine, or nil.
    function FindLocal(aModule: TPasModule;
      const aRoutineName, aName: string): TPasElement;
    // The named implementation-section routine itself, or nil.
    function FindImplRoutine(aModule: TPasModule;
      const aName: string): TPasElement;
    function Contains(const aElements: TPasElementArray;
      aElement: TPasElement): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure UnusedDeclarationsListedOnCompleteClosure;
    procedure NothingUnusedYieldsEmptyArray;
    procedure AccessCountsReportReadsAndWrites;
    procedure NeverAccessedDeclarationHasNoCounts;
    procedure ImplementationRoutineResolvesToItsDeclaration;
    procedure PartialClosureDegradesBothQueries;
    procedure NilOrFailedResolverDegradesBothQueries;
    procedure RepeatedCallsAnswerIdentically;
  end;

implementation

const
  cMode = 'OBJFPC';
  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

  // Self-contained fixtures: no uses clause.

  cUnusedFixture: array[0..38] of string = (
    'unit UseAnalysisUnused;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FSpare: Longint;',
    '    procedure Idle;',
    '  public',
    '    procedure Run;',
    '  end;',
    '',
    'function Drive: Longint;',
    '',
    'implementation',
    '',
    'procedure TThing.Idle;',
    '',
    'begin',
    'end;',
    '',
    'procedure TThing.Run;',
    '',
    'begin',
    'end;',
    '',
    'function Drive: Longint;',
    '',
    'var',
    '  lSpare: Longint;',
    '',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'end.');

  cUsedFixture: array[0..41] of string = (
    'unit UseAnalysisUsed;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FSpare: Longint;',
    '    procedure Idle;',
    '  public',
    '    procedure Run;',
    '  end;',
    '',
    'function Drive: Longint;',
    '',
    'implementation',
    '',
    'procedure TThing.Idle;',
    '',
    'begin',
    '  FSpare := FSpare + 1;',
    'end;',
    '',
    'procedure TThing.Run;',
    '',
    'begin',
    '  Idle;',
    'end;',
    '',
    'function Drive: Longint;',
    '',
    'var',
    '  lSpare: Longint;',
    '',
    'begin',
    '  lSpare := 1;',
    '  Result := lSpare;',
    'end;',
    '',
    'end.');

  cCountsFixture: array[0..21] of string = (
    'unit UseAnalysisCounts;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'function Total: Longint;',
    '',
    'implementation',
    '',
    'function Total: Longint;',
    '',
    'var',
    '  lValue: Longint;',
    '  lSpare: Longint;',
    '',
    'begin',
    '  lValue := 3;',
    '  Result := lValue + lValue;',
    'end;',
    '',
    'end.');

  cPartialDep: array[0..14] of string = (
    'unit UseAnalysisDep;',
    '',
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

  cPartialMain: array[0..14] of string = (
    'unit UseAnalysisMain;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  UseAnalysisDep;',
    '',
    'var',
    '  GThing: TDepThing;',
    '',
    'implementation',
    '',
    'end.');

  cBrokenFixture: array[0..11] of string = (
    'unit UseAnalysisBroken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'var',
    '  GBroken: TNoSuchTypeAtAll;',
    '',
    'implementation',
    '',
    'end.');

procedure TUseAnalysisTest.SetUp;

begin
  inherited SetUp;
  FFix := TTempFixtures.Create;
end;


procedure TUseAnalysisTest.TearDown;

begin
  FFix.Free;
  FFix := nil;
  inherited TearDown;
end;


function TUseAnalysisTest.FindDecl(aModule: TPasModule;
  const aName: string): TPasElement;

var
  lSection: TPasSection;
  i: Integer;
  lDecl: TPasElement;

begin
  Result := nil;
  if aModule = nil then
    Exit;
  lSection := aModule.InterfaceSection;
  if lSection = nil then
    Exit;
  for i := 0 to lSection.Declarations.Count - 1 do
    begin
      lDecl := TPasElement(lSection.Declarations[i]);
      if SameText(lDecl.Name, aName) then
        begin
          Result := lDecl;
          Exit;
        end;
    end;
end;


function TUseAnalysisTest.FindMember(aModule: TPasModule;
  const aTypeName, aMemberName: string): TPasElement;

var
  lType: TPasElement;
  i: Integer;
  lMember: TPasElement;

begin
  Result := nil;
  lType := FindDecl(aModule, aTypeName);
  if not (lType is TPasClassType) then
    Exit;
  for i := 0 to TPasClassType(lType).Members.Count - 1 do
    begin
      lMember := TPasElement(TPasClassType(lType).Members[i]);
      if SameText(lMember.Name, aMemberName) then
        begin
          Result := lMember;
          Exit;
        end;
    end;
end;


function TUseAnalysisTest.FindLocal(aModule: TPasModule;
  const aRoutineName, aName: string): TPasElement;

var
  lSection: TPasSection;
  i, j: Integer;
  lDecl: TPasElement;
  lProc: TPasProcedure;

begin
  Result := nil;
  if aModule = nil then
    Exit;
  lSection := aModule.ImplementationSection;
  if lSection = nil then
    Exit;
  for i := 0 to lSection.Declarations.Count - 1 do
    begin
      lDecl := TPasElement(lSection.Declarations[i]);
      if not (lDecl is TPasProcedure) then
        Continue;
      lProc := TPasProcedure(lDecl);
      if not SameText(lProc.Name, aRoutineName) or (lProc.Body = nil) then
        Continue;
      for j := 0 to lProc.Body.Declarations.Count - 1 do
        begin
          lDecl := TPasElement(lProc.Body.Declarations[j]);
          if SameText(lDecl.Name, aName) then
            begin
              Result := lDecl;
              Exit;
            end;
        end;
    end;
end;


function TUseAnalysisTest.FindImplRoutine(aModule: TPasModule;
  const aName: string): TPasElement;

var
  lSection: TPasSection;
  i: Integer;
  lDecl: TPasElement;

begin
  Result := nil;
  if aModule = nil then
    Exit;
  lSection := aModule.ImplementationSection;
  if lSection = nil then
    Exit;
  for i := 0 to lSection.Declarations.Count - 1 do
    begin
      lDecl := TPasElement(lSection.Declarations[i]);
      if (lDecl is TPasProcedure) and SameText(lDecl.Name, aName) then
        begin
          Result := lDecl;
          Exit;
        end;
    end;
end;


function TUseAnalysisTest.Contains(const aElements: TPasElementArray;
  aElement: TPasElement): Boolean;

var
  i: Integer;

begin
  Result := False;
  for i := 0 to High(aElements) do
    if aElements[i] = aElement then
      begin
        Result := True;
        Exit;
      end;
end;


procedure TUseAnalysisTest.UnusedDeclarationsListedOnCompleteClosure;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lUnused: TPasElementArray;
  lField, lMethod, lLocal: TPasElement;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('unused fixture builds',
      lResolver.BuildFor(FFix.Add('useanalysisunused.pas', cUnusedFixture),
        cMode, cDefines, [], [], lDiag));

    lField := FindMember(lResolver.ResolvedModule, 'TThing', 'FSpare');
    lMethod := FindMember(lResolver.ResolvedModule, 'TThing', 'Idle');
    lLocal := FindLocal(lResolver.ResolvedModule, 'Drive', 'lSpare');
    AssertNotNull('private field found', lField);
    AssertNotNull('private method found', lMethod);
    AssertNotNull('unused local found', lLocal);

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertTrue('the query answers on a complete closure',
        lUse.TryUnusedDeclarations(lUnused));
      AssertEquals('unused declaration count', 3, Length(lUnused));
      AssertTrue('the private field is listed', Contains(lUnused, lField));
      AssertTrue('the private method is listed', Contains(lUnused, lMethod));
      AssertTrue('the unused local is listed', Contains(lUnused, lLocal));
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


procedure TUseAnalysisTest.NothingUnusedYieldsEmptyArray;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lUnused: TPasElementArray;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('fully-used fixture builds',
      lResolver.BuildFor(FFix.Add('useanalysisused.pas', cUsedFixture),
        cMode, cDefines, [], [], lDiag));

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertTrue('the query answers on a complete closure',
        lUse.TryUnusedDeclarations(lUnused));
      AssertEquals('nothing is unused', 0, Length(lUnused));
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


procedure TUseAnalysisTest.AccessCountsReportReadsAndWrites;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lLocal: TPasElement;
  lReads, lWrites: Integer;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('counts fixture builds',
      lResolver.BuildFor(FFix.Add('useanalysiscounts.pas', cCountsFixture),
        cMode, cDefines, [], [], lDiag));

    lLocal := FindLocal(lResolver.ResolvedModule, 'Total', 'lValue');
    AssertNotNull('accessed local found', lLocal);

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertTrue('the query answers on a complete closure',
        lUse.TryAccessCounts(lLocal, lReads, lWrites));
      AssertEquals('read count', 2, lReads);
      AssertEquals('write count', 1, lWrites);
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


procedure TUseAnalysisTest.NeverAccessedDeclarationHasNoCounts;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lLocal: TPasElement;
  lReads, lWrites: Integer;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('unused fixture builds',
      lResolver.BuildFor(FFix.Add('useanalysisunused.pas', cUnusedFixture),
        cMode, cDefines, [], [], lDiag));

    lLocal := FindLocal(lResolver.ResolvedModule, 'Drive', 'lSpare');
    AssertNotNull('unused local found', lLocal);

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertFalse('no access was recorded',
        lUse.TryAccessCounts(lLocal, lReads, lWrites));
      AssertEquals('reads stay zero', 0, lReads);
      AssertEquals('writes stay zero', 0, lWrites);
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


procedure TUseAnalysisTest.ImplementationRoutineResolvesToItsDeclaration;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lImpl: TPasElement;
  lReads, lWrites: Integer;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('counts fixture builds',
      lResolver.BuildFor(FFix.Add('useanalysiscounts.pas', cCountsFixture),
        cMode, cDefines, [], [], lDiag));

    lImpl := FindImplRoutine(lResolver.ResolvedModule, 'Total');
    AssertNotNull('implementation routine found', lImpl);
    AssertTrue('it is not the interface declaration',
      lImpl <> FindDecl(lResolver.ResolvedModule, 'Total'));

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertTrue('the query answers for the implementation element',
        lUse.TryAccessCounts(lImpl, lReads, lWrites));
      AssertEquals('a routine records no reads', 0, lReads);
      AssertEquals('a routine records no writes', 0, lWrites);
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


procedure TUseAnalysisTest.PartialClosureDegradesBothQueries;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lMain, lDir: string;
  lUnused: TPasElementArray;
  lDecl: TPasElement;
  lReads, lWrites: Integer;

begin
  FFix.Add('useanalysisdep.pas', cPartialDep);
  lMain := FFix.Add('useanalysismain.pas', cPartialMain);
  lDir := ExtractFilePath(lMain);
  lResolver := TFpSonarResolver.Create;
  try
    lResolver.DependencyInterfaceOnly := True;
    AssertTrue('partial-closure fixture builds',
      lResolver.BuildFor(lMain, cMode, cDefines, [lDir], [], lDiag));

    lDecl := FindDecl(lResolver.ResolvedModule, 'GThing');
    AssertNotNull('dependency-typed var found', lDecl);

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertFalse('the unused query degrades',
        lUse.TryUnusedDeclarations(lUnused));
      AssertEquals('the array stays empty', 0, Length(lUnused));
      AssertFalse('the counts query degrades',
        lUse.TryAccessCounts(lDecl, lReads, lWrites));
      AssertEquals('reads stay zero', 0, lReads);
      AssertEquals('writes stay zero', 0, lWrites);
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


procedure TUseAnalysisTest.NilOrFailedResolverDegradesBothQueries;

var
  lGood, lBad: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lUnused: TPasElementArray;
  lLocal: TPasElement;
  lReads, lWrites: Integer;

begin
  lGood := TFpSonarResolver.Create;
  try
    AssertTrue('counts fixture builds',
      lGood.BuildFor(FFix.Add('useanalysiscounts.pas', cCountsFixture),
        cMode, cDefines, [], [], lDiag));
    lLocal := FindLocal(lGood.ResolvedModule, 'Total', 'lValue');
    AssertNotNull('accessed local found', lLocal);

    lUse := TFpSonarUseAnalysis.Create(nil);
    try
      AssertFalse('a nil resolver degrades the unused query',
        lUse.TryUnusedDeclarations(lUnused));
      AssertEquals('the array stays empty', 0, Length(lUnused));
      AssertFalse('a nil resolver degrades the counts query',
        lUse.TryAccessCounts(lLocal, lReads, lWrites));
      AssertEquals('reads stay zero', 0, lReads);
      AssertEquals('writes stay zero', 0, lWrites);
    finally
      lUse.Free;
    end;

    lBad := TFpSonarResolver.Create;
    try
      AssertFalse('unresolvable fixture fails to build',
        lBad.BuildFor(FFix.Add('useanalysisbroken.pas', cBrokenFixture),
          cMode, cDefines, [], [], lDiag));

      lUse := TFpSonarUseAnalysis.Create(lBad);
      try
        AssertFalse('a failed build degrades the unused query',
          lUse.TryUnusedDeclarations(lUnused));
        AssertEquals('the array stays empty', 0, Length(lUnused));
        AssertFalse('a failed build degrades the counts query',
          lUse.TryAccessCounts(lLocal, lReads, lWrites));
        AssertEquals('reads stay zero', 0, lReads);
        AssertEquals('writes stay zero', 0, lWrites);
      finally
        lUse.Free;
      end;
    finally
      lBad.Free;
    end;
  finally
    lGood.Free;
  end;
end;


procedure TUseAnalysisTest.RepeatedCallsAnswerIdentically;

var
  lResolver: TFpSonarResolver;
  lUse: TFpSonarUseAnalysis;
  lDiag: TFpSonarDiagnostic;
  lFirst, lSecond: TPasElementArray;
  lLocal: TPasElement;
  lReads, lWrites, lReadsAgain, lWritesAgain: Integer;
  i: Integer;

begin
  lResolver := TFpSonarResolver.Create;
  try
    AssertTrue('counts fixture builds',
      lResolver.BuildFor(FFix.Add('useanalysiscounts.pas', cCountsFixture),
        cMode, cDefines, [], [], lDiag));

    lLocal := FindLocal(lResolver.ResolvedModule, 'Total', 'lValue');
    AssertNotNull('accessed local found', lLocal);

    lUse := TFpSonarUseAnalysis.Create(lResolver);
    try
      AssertTrue('first unused query answers', lUse.TryUnusedDeclarations(lFirst));
      AssertTrue('first counts query answers',
        lUse.TryAccessCounts(lLocal, lReads, lWrites));
      AssertTrue('second unused query answers', lUse.TryUnusedDeclarations(lSecond));
      AssertTrue('second counts query answers',
        lUse.TryAccessCounts(lLocal, lReadsAgain, lWritesAgain));

      AssertTrue('the unused list is non-empty, so the comparison bites',
        Length(lFirst) > 0);
      AssertEquals('same unused count', Length(lFirst), Length(lSecond));
      for i := 0 to High(lFirst) do
        AssertTrue('same unused declarations', Contains(lSecond, lFirst[i]));
      AssertEquals('same read count', lReads, lReadsAgain);
      AssertEquals('same write count', lWrites, lWritesAgain);
    finally
      lUse.Free;
    end;
  finally
    lResolver.Free;
  end;
end;


initialization
  RegisterTest(TUseAnalysisTest);

end.
