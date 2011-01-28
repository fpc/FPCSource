{ %FAIL }

{ access to methods must adhere to visibility rules (here: protected)}
program tchlp48;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp45;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test4;
end.
