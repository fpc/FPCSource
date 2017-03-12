unit tcgenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, pparser, pastree, testregistry, tctypeparser;

Type

  { TTestGenerics }

  TTestGenerics = Class(TBaseTestTypeParser)
  Published
    Procedure TestObjectGenerics;
    Procedure TestSpecializationDelphi;
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

initialization
  RegisterTest(TTestGenerics);
end.

