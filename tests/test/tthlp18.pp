program tthlp18;

{$mode objfpc}
{$apptype console}

uses
  uthlp;

var
  i: Integer;
begin
  i := 42;
  i.Create(21);
  if i <> 21 then
    Halt(1);
  Writeln('OK');
end.
