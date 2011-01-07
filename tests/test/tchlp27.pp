{ extensive scoping test - test 1 }
program tchlp27;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp27a, uchlp27b;

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

