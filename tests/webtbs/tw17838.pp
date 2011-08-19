{$mode objfpc}
program calltest;
var
  count: integer;
  b : byte;
procedure nop;
begin
  end;
function f():cardinal;
begin
  inc(count);
  result:=count;
  end;
begin
  count:=0;
  b:=1;
  if f()=1 then
    nop;
  if f()=-1 then
    nop;
  if f()=2 then
    nop;
  if f()<>-1 then
    nop;
  if b<-1 then
    nop;
  if count<>4 then
    halt(1);
  writeln('ok');
end.
