{ extensive scoping test - test 2 }
program tchlp28;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp27a, uchlp27b, uchlp27c;

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

