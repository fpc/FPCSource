{ %FAIL }

{ access to helper methods adheres to visibility rules (here: strict private) }
program thlp31;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp31;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test1;
end.
