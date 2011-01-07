{ %FAIL }

{ only the last available class helper for a class must be used - test 1 }
program tchlp32;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp32a, uchlp32b, uchlp32c;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Method1;
end.

