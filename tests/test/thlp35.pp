{ %NORUN }

{ access to helper methods adheres to visibility rules (here: public) }
program thlp35;

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses
  uhlp31;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test5;
end.
