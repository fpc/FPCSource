{ extensive scoping test - test 5 }
program tchlp31;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp27b, uchlp27c;

var
  b: TBar;
  res: Integer;
begin
  b := TBar.Create;
  res := b.Test;
  Writeln('b.Test: ', res);
  if res <> 3 then
    Halt(1);

  Writeln('ok');
end.

