unit tcgenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, testregistry, pscanner, pparser, tctypeparser;

Type

  { TTestGenerics }

  TTestGenerics = Class(TBaseTestTypeParser)
  Published
    Procedure TestObjectGenerics;
    Procedure TestSpecializationDelphi;
    Procedure TestDeclarationDelphi;
    Procedure TestDeclarationDelphiSpecialize;
  end;

implementation

procedure TTestGenerics.TestObjectGenerics;
begin
  Source.Add('Type');
  Source.Add('Generic TSomeClass<T> = Object');
  Source.Add('  b : T;');
  Source.Add('end;');
  ParseDeclarations;
end;

procedure TTestGenerics.TestSpecializationDelphi;
begin
  ParseType('TFPGList<integer>',TPasClassType,'');
end;

procedure TTestGenerics.TestDeclarationDelphi;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches ;
  Source.Add('Type');
  Source.Add('  TSomeClass<T,T2> = Class(TObject)');
  Source.Add('  b : T;');
  Source.Add('  b2 : T2;');
  Source.Add('end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);
end;

procedure TTestGenerics.TestDeclarationDelphiSpecialize;
Var
  T : TPasClassType;
begin
  Scanner.CurrentModeSwitches:=[msDelphi]+Scanner.CurrentModeSwitches ;
  Source.Add('Type');
  Source.Add('  TSomeClass<T,T2> = Class(TSomeGeneric<Integer,Integer>)');
  Source.Add('  b : T;');
  Source.Add('  b2 : T2;');
  Source.Add('end;');
  ParseDeclarations;
  AssertNotNull('have generic definition',Declarations.Classes);
  AssertEquals('have generic definition',1,Declarations.Classes.Count);
  AssertEquals('Pascal class',TPasClassType,TObject(Declarations.Classes[0]).ClassType);
  T:=TPasClassType(Declarations.Classes[0]);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);

end;

initialization
  RegisterTest(TTestGenerics);
end.

