unit tcgenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pastree, testregistry, tctypeparser;

Type

  { TTestGenerics }

  TTestGenerics = Class(TBaseTestTypeParser)
  Published
    Procedure TestObjectGenerics;
    Procedure TestRecordGenerics;
    Procedure TestArrayGenerics;
    Procedure TestSpecializationDelphi;
    Procedure TestSpecializeNested;
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

procedure TTestGenerics.TestSpecializeNested;
begin
  Add([
    'Type',
    '  generic TSomeClass<A,B> = class(specialize TOther<A,specialize TAnother<B>>) end;',
    '']);
  ParseDeclarations;
end;

initialization
  RegisterTest(TTestGenerics);
end.

