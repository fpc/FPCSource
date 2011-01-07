{ extensive scoping test - test 4 }
program tchlp30;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp27b, uchlp27a;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.Test;
  Writeln('f.Test: ', res);
  if res <> 2 then
    Halt(1);
end.

