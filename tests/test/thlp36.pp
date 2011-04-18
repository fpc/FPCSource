{ %NORUN }

{ access to helper methods adheres to visibility rules (here: published) }
program thlp36;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp31;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test6;
end.
