unit tcgenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, testregistry, pscanner, tctypeparser;

Type

  { TTestGenerics }

  TTestGenerics = Class(TBaseTestTypeParser)
  Published
    Procedure TestObjectGenerics;
    Procedure TestRecordGenerics;
    Procedure TestArrayGenerics;
    Procedure TestSpecializationDelphi;
    Procedure TestDeclarationDelphi;
    Procedure TestDeclarationDelphiSpecialize;
    Procedure TestMethodImplementation;
    Procedure TestInlineSpecializationInArgument;
    Procedure TestSpecializeNested;
    Procedure TestInlineSpecializeInStatement;
  end;

implementation

procedure TTestGenerics.TestObjectGenerics;
begin
  Add([
    'Type',
    'Generic TSomeClass<T> = Object',
    '  b : T;',
    'end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestRecordGenerics;
begin
  Add([
    'Type',
    '  Generic TSome<T> = Record',
    '    b : T;',
    '  end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestArrayGenerics;
begin
  Add([
    'Type',
    '  Generic TSome<T> = array of T;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestSpecializationDelphi;
begin
  ParseType('TFPGList<integer>',TPasSpecializeType,'');
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
  AssertEquals('Name is correct','TSomeClass',T.Name);
  AssertNotNull('have generic templates',T.GenericTemplateTypes);
  AssertEquals('2 template types',2,T.GenericTemplateTypes.Count);
  AssertSame('Parent 0 is class',T,TPasElement(T.GenericTemplateTypes[0]).Parent);
  AssertSame('Parent 1 is class',T,TPasElement(T.GenericTemplateTypes[1]).Parent);
end;

procedure TTestGenerics.TestMethodImplementation;
begin
  With source do
    begin
    Add('unit afile;');
    Add('{$MODE DELPHI}');
    Add('interface');
    Add('type');
    Add('  TTest<T> =  object');
    Add('    procedure foo(v:T);');
    Add('  end;');
    Add('implementation');
    Add('procedure TTest<T>.foo;');
    Add('begin');
    Add('end;');
    end;
  ParseModule;
end;

procedure TTestGenerics.TestInlineSpecializationInArgument;
begin
  With source do
    begin
    Add('unit afile;');
    Add('{$MODE DELPHI}');
    Add('interface');
    Add('type');
    Add('  TFoo=class');
    Add('    procedure foo(var Node:TSomeGeneric<TBoundingBox>;const index:Integer);');
    Add('  end;');
    Add('implementation');
    end;
  ParseModule;
end;

procedure TTestGenerics.TestSpecializeNested;
begin
  Add([
    'Type',
    '  generic TSomeClass<A,B> = class(specialize TOther<A,specialize TAnother<B>>) end;',
    '']);
  ParseDeclarations;
end;

procedure TTestGenerics.TestInlineSpecializeInStatement;
begin
  Add([
  'begin',
  '  vec:=TVector<double>.create;',
  '  b:=a<b;',
  '  t:=a<b.c<d,e.f>>;',
  '']);
  ParseModule;
end;

initialization
  RegisterTest(TTestGenerics);
end.

