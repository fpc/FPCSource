unit tcresolvegenerics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testregistry, tcresolver;

type

  { TTestResolveGenerics }

  TTestResolveGenerics = Class(TCustomTestResolver)
  Published
    procedure TestGen_GenericFunction; // ToDo
  end;

implementation

{ TTestResolveGenerics }

procedure TTestResolveGenerics.TestGen_GenericFunction;
begin
  exit;
  StartProgram(false);
  Add([
  'generic function DoIt<T>(a: T): T;',
  'begin',
  '  Result:=a;',
  'end;',
  'var w: word;',
  'begin',
  '  w:=DoIt<word>(3);',
  '']);
  ParseProgram;
end;

initialization
  RegisterTests([TTestResolveGenerics]);

end.

