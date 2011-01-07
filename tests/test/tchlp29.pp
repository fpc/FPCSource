{ extensive scoping test - test 3 }
program tchlp29;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp27a, uchlp27c, uchlp27b;

var
  f: TFoo;
  b: TBar;
  res: Integer;
begin
  f := TBar.Create;
  res := f.Test;
  Writeln('f.Test: ', res);
  if res <> 2 then
    Halt(1);

  b := TBar.Create;
  res := b.Test;
  Writeln('b.Test: ', res);
  if res <> 3 then
    Halt(2);
end.

