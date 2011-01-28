{ %FAIL }

{ access to methods must adhere to visibility rules (here: private)}
program tchlp46;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp45;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test2;
end.
