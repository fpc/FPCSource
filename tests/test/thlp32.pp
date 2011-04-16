{ %FAIL }

{ access to helper methods adheres to visibility rules (here: private) }
program thlp32;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp31;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test2;
end.
