{ %FAIL }

{ access to methods must adhere to visibility rules }
program tchlp45;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

uses
  uchlp45;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test1;
end.
