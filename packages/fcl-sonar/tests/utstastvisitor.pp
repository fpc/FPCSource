{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the AST-traversal foundation (FpSonar.Traversal)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstAstVisitor;

{ The shared AST-traversal in FpSonar.Traversal. }


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PasTree, FpSonar.SourceFile, FpSonar.Traversal, UtstFixtures;

type
  { Routine-enumeration tests for the AST-visitor foundation. }
  TAstVisitorTest = class(TTestCase)
  private
    // Writes aSrc into a temp dir and analyses it through the
    // production source-file pipeline (mode/defines fixed as OBJFPC/Linux64).
    procedure AnalyzeSrc(aSource: TFpSonarSourceFile; const aName: string;
      const aSrc: array of string);
  published
    procedure EnumeratesRoutinesInDeclarationOrder;
    procedure NilModuleYieldsNoRoutines;
    procedure EnumeratesTypesInDeclarationOrder;
    procedure NilModuleYieldsNoTypes;
    procedure EnumeratesValueDeclsInDeclarationOrder;
    procedure NilModuleYieldsNoValueDecls;
    procedure EnumeratesStatementRootsInOrder;
    procedure NilModuleYieldsNoStatementRoots;
  end;


implementation

const
  // Embedded AST-visitor fixtures (Approach A rollout): line i+1 == [i].

  cAstVisitorFixture: array[0..48] of string = (
    'unit AstVisitorFixture;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    procedure DoIt(aX: Integer);',
    '    function Calc: Integer;',
    '  end;',
    '',
    '// A top-level procedure with a nested routine.',
    'procedure TopLevel(aN: Integer);',
    '',
    'implementation',
    '',
    'procedure TThing.DoIt(aX: Integer);',
    '',
    'begin',
    '  FValue := aX;',
    'end;',
    '',
    '',
    'function TThing.Calc: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    '',
    'procedure TopLevel(aN: Integer);',
    '',
    '  procedure Inner;',
    '',
    '  begin',
    '    Writeln(''inner'');',
    '  end;',
    '',
    'begin',
    '  Inner;',
    '  Writeln(aN);',
    'end;',
    '',
    '',
    'end.');

  cTypesFixture: array[0..35] of string = (
    'unit TypesFixture;',
    '',
    '{ Foundation fixture for EnumerateTypes. Mixes a forward',
    '  class (skipped), an enum, a record, an interface, the real declaration of the',
    '  forward class with one NESTED type, and a class helper, so the foundation test',
    '  can assert declaration order, nested-type inclusion, and forward-decl skipping.',
    '  The nested type lives in a CLASS (records need the advancedrecords modeswitch',
    '  to host a nested type section in this FPC snapshot). }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFwd = class;                       // forward — EnumerateTypes SKIPS this',
    '',
    '  TColorEnum = (ceRed, ceGreen);',
    '',
    '  TPointRec = record',
    '    X: Integer;',
    '  end;',
    '',
    '  IThing = interface',
    '  end;',
    '',
    '  TFwd = class                        // the real declaration of TFwd',
    '    type',
    '      TNestedKind = (nkA, nkB);       // NESTED type — EnumerateTypes INCLUDES this',
    '  end;',
    '',
    '  TThingHelper = class helper for TObject',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cValuesFixture: array[0..31] of string = (
    'unit ValuesFixture;',
    '',
    '{ Foundation fixture for EnumerateValueDecls. A top-level',
    '  const, a top-level var, a class with a field AND a property (the property must',
    '  be EXCLUDED, never miscounted as a field), and an implementation-section const,',
    '  so the foundation test can assert declaration order, the field-as-vkField',
    '  invariant and the property exclusion. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '  cTopConst = 1;',
    '',
    'var',
    '  GlobalVar: Integer;',
    '',
    'type',
    '  TThing = class',
    '  private',
    '    FField: Integer;',
    '  public',
    '    property Value: Integer read FField;',
    '  end;',
    '',
    'implementation',
    '',
    'const',
    '  cImplConst = 2;',
    '',
    'end.');

  cStatementsFixture: array[0..22] of string = (
    'unit StatementsFixture;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'procedure Run;',
    '',
    'implementation',
    '',
    'procedure Run;',
    '',
    'begin',
    '  WriteLn(''run'');',
    'end;',
    '',
    'initialization',
    '  WriteLn(''init'');',
    '',
    'finalization',
    '  WriteLn(''done'');',
    '',
    'end.');


procedure TAstVisitorTest.AnalyzeSrc(aSource: TFpSonarSourceFile;
  const aName: string; const aSrc: array of string);

var
  lFix: TTempFixtures;

begin
  lFix := TTempFixtures.Create;
  try
    aSource.Analyze(lFix.Add(aName, aSrc), 'OBJFPC',
      ['FPC', 'CPUX86_64', 'UNIX', 'LINUX']);
  finally
    lFix.Free;
  end;
end;


procedure TAstVisitorTest.EnumeratesRoutinesInDeclarationOrder;

var
  lSource: TFpSonarSourceFile;
  lRoutines: TAstRoutineArray;

begin
  lSource := TFpSonarSourceFile.Create;
  try
    AnalyzeSrc(lSource, 'astvisitorfixture.pas', cAstVisitorFixture);
    AssertTrue('fixture parsed', lSource.Module <> nil);

    lRoutines := EnumerateRoutines(lSource.Module);

    { The interface forward of TopLevel (no body) is skipped; the two method
      implementations, the top-level routine and its nested routine remain, in
      declaration order. }
    AssertEquals('four body-bearing routines', 4, Length(lRoutines));

    AssertEquals('1st is the DoIt method impl', 'TThing.DoIt',
      lRoutines[0].Decl.Name);
    AssertEquals('1st decl line', 21, lRoutines[0].Decl.SourceLinenumber);

    AssertEquals('2nd is the Calc method impl', 'TThing.Calc',
      lRoutines[1].Decl.Name);
    AssertEquals('2nd decl line', 28, lRoutines[1].Decl.SourceLinenumber);

    AssertEquals('3rd is the top-level routine', 'TopLevel',
      lRoutines[2].Decl.Name);
    AssertEquals('3rd decl line', 35, lRoutines[2].Decl.SourceLinenumber);

    AssertEquals('4th is the nested routine', 'Inner',
      lRoutines[3].Decl.Name);
    AssertEquals('4th decl line', 37, lRoutines[3].Decl.SourceLinenumber);

    // Every enumerated routine exposes a non-nil statement block.
    AssertTrue('DoIt has a block', lRoutines[0].Block <> nil);
    AssertTrue('Inner has a block', lRoutines[3].Block <> nil);
  finally
    lSource.Free;
  end;
end;


procedure TAstVisitorTest.NilModuleYieldsNoRoutines;

var
  lRoutines: TAstRoutineArray;

begin
  // A failed parse leaves Module nil; enumeration must degrade to empty,
  // never dereference nil.
  lRoutines := EnumerateRoutines(nil);
  AssertEquals('no routines for a nil module', 0, Length(lRoutines));
end;


procedure TAstVisitorTest.EnumeratesTypesInDeclarationOrder;

var
  lSource: TFpSonarSourceFile;
  lTypes: TPasTypeArray;

begin
  lSource := TFpSonarSourceFile.Create;
  try
    AnalyzeSrc(lSource, 'typesfixture.pas', cTypesFixture);
    AssertTrue('fixture parsed', lSource.Module <> nil);

    lTypes := EnumerateTypes(lSource.Module);

    { The forward TFwd (no body) is skipped; the enum, the record, the
      interface, the real TFwd, its NESTED type (appended right after its
      enclosing class, per the Members recursion) and the helper remain, in
      declaration order. The invariants that MUST hold: forward TFwd absent,
      real TFwd and nested TNestedKind present, count = 6. }
    AssertEquals('six named types', 6, Length(lTypes));
    AssertEquals('1st is the enum', 'TColorEnum', lTypes[0].Name);
    AssertEquals('2nd is the record', 'TPointRec', lTypes[1].Name);
    AssertEquals('3rd is the interface', 'IThing', lTypes[2].Name);
    AssertEquals('4th is the real class', 'TFwd', lTypes[3].Name);
    AssertEquals('5th is the nested enum', 'TNestedKind', lTypes[4].Name);
    AssertEquals('6th is the helper', 'TThingHelper', lTypes[5].Name);
  finally
    lSource.Free;
  end;
end;


procedure TAstVisitorTest.NilModuleYieldsNoTypes;

var
  lTypes: TPasTypeArray;

begin
  // A failed parse leaves Module nil; enumeration must degrade to empty,
  // never dereference nil.
  lTypes := EnumerateTypes(nil);
  AssertEquals('no types for a nil module', 0, Length(lTypes));
end;


procedure TAstVisitorTest.EnumeratesValueDeclsInDeclarationOrder;

var
  lSource: TFpSonarSourceFile;
  lVals: TPasValueDeclArray;

begin
  lSource := TFpSonarSourceFile.Create;
  try
    AnalyzeSrc(lSource, 'valuesfixture.pas', cValuesFixture);
    AssertTrue('fixture parsed', lSource.Module <> nil);

    lVals := EnumerateValueDecls(lSource.Module);

    { The property Value is EXCLUDED; the two consts, the global var and the
      class field FField remain, in declaration order. The invariants that MUST
      hold: property absent, FField present as vkField, both consts vkConst,
      GlobalVar vkVar, count = 4. }
    AssertEquals('four value declarations', 4, Length(lVals));

    AssertEquals('1st is the top-level const', 'cTopConst', lVals[0].Decl.Name);
    AssertTrue('1st is vkConst', lVals[0].Kind = vkConst);

    AssertEquals('2nd is the global var', 'GlobalVar', lVals[1].Decl.Name);
    AssertTrue('2nd is vkVar', lVals[1].Kind = vkVar);

    AssertEquals('3rd is the class field', 'FField', lVals[2].Decl.Name);
    AssertTrue('3rd is vkField', lVals[2].Kind = vkField);

    AssertEquals('4th is the impl-section const', 'cImplConst',
      lVals[3].Decl.Name);
    AssertTrue('4th is vkConst', lVals[3].Kind = vkConst);
  finally
    lSource.Free;
  end;
end;


procedure TAstVisitorTest.NilModuleYieldsNoValueDecls;

var
  lVals: TPasValueDeclArray;

begin
  // A failed parse leaves Module nil; enumeration must degrade to empty.
  lVals := EnumerateValueDecls(nil);
  AssertEquals('no value decls for a nil module', 0, Length(lVals));
end;


procedure TAstVisitorTest.EnumeratesStatementRootsInOrder;

var
  lSource: TFpSonarSourceFile;
  lRoots: TPasImplElementArray;

begin
  lSource := TFpSonarSourceFile.Create;
  try
    AnalyzeSrc(lSource, 'statementsfixture.pas', cStatementsFixture);
    AssertTrue('fixture parsed', lSource.Module <> nil);

    lRoots := EnumerateStatementRoots(lSource.Module);

    // Exactly three roots in deterministic order: the single routine's block,
    // then the initialization section, then the finalization section.
    AssertEquals('three statement roots', 3, Length(lRoots));
    AssertTrue('1st is the routine begin-block',
      lRoots[0] is TPasImplBeginBlock);
    AssertTrue('2nd is the initialization section',
      lRoots[1] is TInitializationSection);
    AssertTrue('3rd is the finalization section',
      lRoots[2] is TFinalizationSection);
  finally
    lSource.Free;
  end;
end;


procedure TAstVisitorTest.NilModuleYieldsNoStatementRoots;

var
  lRoots: TPasImplElementArray;

begin
  // A failed parse leaves Module nil; enumeration must degrade to empty.
  lRoots := EnumerateStatementRoots(nil);
  AssertEquals('no statement roots for a nil module', 0, Length(lRoots));
end;


initialization
  RegisterTest(TAstVisitorTest);

end.
