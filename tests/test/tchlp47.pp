{ %FAIL }

{ access to methods must adhere to visibility rules (here: strict protected)}
program tchlp47;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp45;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test3;
end.
