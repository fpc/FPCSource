{ %OPT=-O3 }
{$mode objfpc} {$coperators on}
program tw41210;
var
  x, oops: uint32;
begin
  x := random(0) + $123456;
  oops := 0;
  x := x shr 8;
  if byte(x) <> $34 then oops += 1;
  x := x shr 8;
  if byte(x) <> $12 then oops += 2;
  if oops <> 0 then
    begin
      writeln('FAILED: oops = ', oops);
      Halt(1);
    end;
  Writeln('ok');
end.
