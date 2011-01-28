{ %NORUN }

{ access to methods must adhere to visibility rules (here: public)}
program tchlp49;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp45;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test5;
end.
