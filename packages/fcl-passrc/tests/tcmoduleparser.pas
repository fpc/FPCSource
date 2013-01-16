unit tcmoduleparser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, pscanner, pparser,
  tcbaseparser, testregistry;

Type
  { TTestModuleParser }

  TTestModuleParser = class(TTestParser)
  private
    function GetIf: TInterfaceSection;
    function GetIm: TImplementationSection;
    function CheckUnit(AIndex: Integer; const AName: String; AList: TFPList): TPasUnresolvedUnitRef;
  Protected
    Procedure ParseUnit;
    Procedure ParseProgram;
    Procedure ParseLibrary;
    Procedure AssertProgramError;
    Property ImplSection : TImplementationSection Read GetIm;
    Property IntfSection : TInterfaceSection Read GetIf;
  Published
    Procedure TestEmptyUnit;
    Procedure TestUnitOneUses;
    Procedure TestUnitTwoUses;
    Procedure TestUnitOneImplUses;
    Procedure TestUnitTwoImplUses;
    Procedure TestEmptyUnitInitialization;
    Procedure TestEmptyUnitFinalization;
    Procedure TestEmptyUnitInitializationFinalization;
    Procedure TestEmptyUnitBegin;
    Procedure TestEmptyProgram;
    Procedure TestEmptyProgramInputOUtput;
    Procedure TestEmptyProgramNoInitialization;
    Procedure TestEmptyProgramNoFinalization;
    Procedure TestEmptyProgramMissingBegin;
    Procedure TestEmptyProgramNoheader;
    Procedure TestEmptyProgramUses;
    Procedure TestEmptyProgramUsesTwoUnits;
    Procedure TestEmptyProgramUsesUnitIn;
    Procedure TestEmptyLibrary;
    Procedure TestEmptyLibraryUses;
    Procedure TestEmptyLibraryExports;
    Procedure TestEmptyLibraryExportsAlias;
    Procedure TestEmptyLibraryExportsIndex;
    Procedure TestEmptyLibraryExportsTwo;
    Procedure TestEmptyLibraryExportsTwoAlias;
    Procedure TestEmptyLibraryExportsTwoIndex;
  end;

implementation
{ TTestModuleParser }

function TTestModuleParser.GetIf: TInterfaceSection;
begin
  Result:=Module.InterfaceSection;
end;

function TTestModuleParser.GetIm: TImplementationSection;
begin
  Result:=Module.ImplementationSection;
end;

procedure TTestModuleParser.ParseUnit;
begin
  EndSource;
  ParseModule;
  AssertNotNull('Have interface',Module.InterfaceSection);
  Declarations:=Module.InterfaceSection;
  AssertEquals('Interface section',TInterfaceSection,Declarations.ClassType);
  AssertNotNull('Have implmeentation',Module.ImplementationSection);
  AssertEquals('implementation section',TImplementationSection,Module.ImplementationSection.ClassType);
  AssertNotNull('Have interface units',IntfSection.UsesList);
  AssertNotNull('Have implementation units',ImplSection.UsesList);
end;

procedure TTestModuleParser.ParseProgram;
begin
  EndSource;
  ParseModule;
  AssertEquals('Is program',TPasProgram,Module.ClassType);
end;

procedure TTestModuleParser.ParseLibrary;
begin
  EndSource;
  ParseModule;
  AssertEquals('Is library',TPasLibrary,Module.ClassType);
end;

procedure TTestModuleParser.AssertProgramError;
begin
  AssertException(EParserError,@ParseProgram)
end;

function TTestModuleParser.CheckUnit(AIndex: Integer; const AName: String;
  AList: TFPList) : TPasUnresolvedUnitRef;

Var
  C : string;

begin
  C:='Unit '+IntTostr(AIndex)+' ';
  if (AIndex>=AList.Count) then
    Fail(Format('Index %d larger than unit list count %d',[AIndex,AList.Count ]));
  AssertNotNull('Have pascal element',AList[AIndex]);
  AssertEquals(C+'Correct class',TPasUnresolvedUnitRef,TObject(AList[AIndex]).CLassType);
  Result:=TPasUnresolvedUnitRef(AList[AIndex]);
  AssertEquals(C+'Unit name correct',AName,Result.Name);
end;

procedure TTestModuleParser.TestEmptyUnit;
begin
  StartUnit('unit1');
  StartImplementation;
  ParseUnit;
  AssertEquals('No interface units',0,IntfSection.UsesList.Count);
  AssertEquals('No implementation units',0,ImplSection.UsesList.Count);
end;

procedure TTestModuleParser.TestUnitOneUses;
begin
  StartUnit('unit1');
  UsesClause(['a']);
  StartImplementation;
  ParseUnit;
  AssertEquals('Two interface units',2,IntfSection.UsesList.Count);
  CheckUnit(0,'System',IntfSection.UsesList);
  CheckUnit(1,'a',IntfSection.UsesList);
  AssertEquals('No implementation units',0,ImplSection.UsesList.Count);
end;

procedure TTestModuleParser.TestUnitTwoUses;
begin
  StartUnit('unit1');
  UsesClause(['a','b']);
  StartImplementation;
  ParseUnit;
  AssertEquals('Two interface units',3,IntfSection.UsesList.Count);
  CheckUnit(0,'System',IntfSection.UsesList);
  CheckUnit(1,'a',IntfSection.UsesList);
  CheckUnit(2,'b',IntfSection.UsesList);
  AssertEquals('No implementation units',0,ImplSection.UsesList.Count);
end;

procedure TTestModuleParser.TestUnitOneImplUses;
begin
  StartUnit('unit1');
  StartImplementation;
  UsesClause(['a']);
  ParseUnit;
  AssertEquals('One implementation units',1,ImplSection.UsesList.Count);
  CheckUnit(0,'a',ImplSection.UsesList);
  AssertEquals('No interface units',0,IntfSection.UsesList.Count);
end;

procedure TTestModuleParser.TestUnitTwoImplUses;
begin
  StartUnit('unit1');
  StartImplementation;
  UsesClause(['a','b']);
  ParseUnit;
  AssertEquals('Two implementation units',2,ImplSection.UsesList.Count);
  CheckUnit(0,'a',ImplSection.UsesList);
  CheckUnit(1,'b',ImplSection.UsesList);
  AssertEquals('No interface units',0,IntfSection.UsesList.Count);
end;

procedure TTestModuleParser.TestEmptyUnitInitialization;
begin
  StartUnit('unit1');
  StartImplementation;
  Add('initialization');
  ParseUnit;
  AssertNotNull('Have initialization section',Module.InitializationSection);
  AssertNull('Have no finalization section',Module.FinalizationSection)
end;

procedure TTestModuleParser.TestEmptyUnitFinalization;
begin
  StartUnit('unit1');
  StartImplementation;
  Add('finalization');
  ParseUnit;
  AssertNull('Have no initalization section',Module.InitializationSection);
  AssertNotNull('Have finalization section',Module.FinalizationSection)
end;

procedure TTestModuleParser.TestEmptyUnitInitializationFinalization;
begin
  StartUnit('unit1');
  StartImplementation;
  Add('initialization');
  Add('finalization');
  ParseUnit;
  AssertNotNull('Have finalization section',Module.InitializationSection);
  AssertNotNull('Have finalization section',Module.FinalizationSection);
end;

procedure TTestModuleParser.TestEmptyUnitBegin;
begin
  StartUnit('unit1');
  StartImplementation;
  Add('begin');
  ParseUnit;
  AssertNotNull('Have initialization section',Module.InitializationSection);
  AssertNull('Have no finalization section',Module.FinalizationSection)
end;

procedure TTestModuleParser.TestEmptyProgram;
begin
  StartProgram('something');
  Add('begin');
  ParseProgram;
end;

procedure TTestModuleParser.TestEmptyProgramInputOUtput;
begin
  StartProgram('something','input','output');
  Add('begin');
  ParseProgram;
end;

procedure TTestModuleParser.TestEmptyProgramNoInitialization;
begin
  StartProgram('something','input','output');
  Add('initialization');
  AssertProgramError;
end;

procedure TTestModuleParser.TestEmptyProgramNoFinalization;
begin
  StartProgram('something','input','output');
  Add('finalization');
  AssertProgramError;
end;

procedure TTestModuleParser.TestEmptyProgramMissingBegin;
begin
  StartProgram('something','input','output');
  AssertProgramError;
end;

procedure TTestModuleParser.TestEmptyProgramNoheader;
begin
  Add('begin');
  ParseProgram;
end;

procedure TTestModuleParser.TestEmptyProgramUses;
begin
  UsesClause(['a']);
  Add('begin');
  ParseProgram;
  AssertEquals('Two interface units',2, PasProgram.ProgramSection.UsesList.Count);
  CheckUnit(0,'System',PasProgram.ProgramSection.UsesList);
  CheckUnit(1,'a',PasProgram.ProgramSection.UsesList);
end;

procedure TTestModuleParser.TestEmptyProgramUsesTwoUnits;
begin
  UsesClause(['a','b']);
  Add('begin');
  ParseProgram;
  AssertEquals('Three interface units',3, PasProgram.ProgramSection.UsesList.Count);
  CheckUnit(0,'System',PasProgram.ProgramSection.UsesList);
  CheckUnit(1,'a',PasProgram.ProgramSection.UsesList);
  CheckUnit(2,'b',PasProgram.ProgramSection.UsesList);
end;

procedure TTestModuleParser.TestEmptyProgramUsesUnitIn;

Var
  U : TPasUnresolvedUnitRef;

begin
  UsesClause(['a in ''../a.pas''','b']);
  Add('begin');
  ParseProgram;
  AssertEquals('One interface unit',3, PasProgram.ProgramSection.UsesList.Count);
  CheckUnit(0,'System',PasProgram.ProgramSection.UsesList);
  U:=CheckUnit(1,'a',PasProgram.ProgramSection.UsesList);
  AssertEquals('Filename','''../a.pas''',U.FileName);
  CheckUnit(2,'b',PasProgram.ProgramSection.UsesList);
end;

procedure TTestModuleParser.TestEmptyLibrary;
begin
  StartLibrary('');
  ParseLibrary;
  AssertEquals('Correct class',TPasLibrary,Module.ClassType);
end;

procedure TTestModuleParser.TestEmptyLibraryUses;
begin
  StartLibrary('');
  UsesClause(['a']);
  ParseLibrary;
  AssertEquals('Correct class',TPasLibrary,Module.ClassType);
  AssertEquals('Two interface units',2, PasLibrary.LibrarySection.UsesList.Count);
  CheckUnit(0,'System',PasLibrary.LibrarySection.UsesList);
  CheckUnit(1,'a',PasLibrary.LibrarySection.UsesList);
end;

procedure TTestModuleParser.TestEmptyLibraryExports;
begin
  StartLibrary('');
  UsesClause(['b']);
  Add('exports A;');
  ParseLibrary;
  AssertEquals('1 export symbol',1,PasLibrary.LibrarySection.ExportSymbols.Count);
  AssertExportSymbol('Export symbol a',0,'A','',-1);
end;

procedure TTestModuleParser.TestEmptyLibraryExportsAlias;
begin
  StartLibrary('');
  UsesClause(['b']);
  Add('exports A name ''c'';');
  ParseLibrary;
  AssertEquals('1 export symbol',1,PasLibrary.LibrarySection.ExportSymbols.Count);
  AssertExportSymbol('Export symbol a',0,'A','c',-1);
end;

procedure TTestModuleParser.TestEmptyLibraryExportsIndex;
begin
  StartLibrary('');
  UsesClause(['b']);
  Add('exports A index 23;');
  ParseLibrary;
  AssertEquals('1 export symbol',1,PasLibrary.LibrarySection.ExportSymbols.Count);
  AssertExportSymbol('Export symbol a',0,'A','',23);
end;

procedure TTestModuleParser.TestEmptyLibraryExportsTwo;
begin
  StartLibrary('');
  UsesClause(['b']);
  Add('exports A , C;');
  ParseLibrary;
  AssertEquals('2 export symbol',2,PasLibrary.LibrarySection.ExportSymbols.Count);
  AssertExportSymbol('Export symbol a',0,'A','',-1);
  AssertExportSymbol('Export symbol C',1,'C','',-1);
end;

procedure TTestModuleParser.TestEmptyLibraryExportsTwoAlias;
begin
  StartLibrary('');
  UsesClause(['b']);
  Add('exports A name ''de'', C;');
  ParseLibrary;
  AssertEquals('2 export symbol',2,PasLibrary.LibrarySection.ExportSymbols.Count);
  AssertExportSymbol('Export symbol a',0,'A','de',-1);
  AssertExportSymbol('Export symbol C',1,'C','',-1);

end;

procedure TTestModuleParser.TestEmptyLibraryExportsTwoIndex;
begin
  StartLibrary('');
  UsesClause(['b']);
  Add('exports A index 23, C;');
  ParseLibrary;
  AssertEquals('2 export symbol',2,PasLibrary.LibrarySection.ExportSymbols.Count);
  AssertExportSymbol('Export symbol a',0,'A','',23);
  AssertExportSymbol('Export symbol C',1,'C','',-1);
end;

initialization
  RegisterTests([TTestModuleParser]);

end.

