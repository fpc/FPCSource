{ %FAIL }

{ access to helper methods adheres to visibility rules (here: protected) }
program thlp34;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp31;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test4;
end.
