{ %OPT=-O- -O1 }
program testbug;
{$mode objfpc}{$h+}
var
  i: Integer;
  b: Byte;
  w: Word;
begin
  i := 53;
  b := 0;
  w := abs(i-b);
  WriteLn(w);
  if w<>53 then
    halt(1);
  Writeln('ok');
end.
