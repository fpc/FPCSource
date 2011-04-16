{ %FAIL }

{ access to helper methods adheres to visibility rules (here: strict protected)}
program thlp33;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp31;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test3;
end.
