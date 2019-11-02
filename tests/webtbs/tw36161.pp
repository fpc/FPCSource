{ %OPT=-O1 }
program Project1;
uses Math;
var
  t: Qword;
  i: int64;
begin
  i:=-12345687;
  t:=QWord($a000000000000000);
  if Min(sizeof(t), t)<>sizeof(t) then
    halt(1);
  if Min(t, sizeof(t))<>sizeof(t) then
    halt(2);
  if Min(t, i)<>i then
    halt(3);
  if Min(i, t)<>i then
    halt(4);
  if Min(sizeof(t),sizeof(t))<>sizeof(t) then
    halt(5);
  if Min(t,t)<>t then
    halt(6);
  if Min(i,i)<>i then
    halt(7);
  
  if Max(sizeof(t), t)<>t then
    halt(11);
  if Max(t, sizeof(t))<>t then
    halt(12);
  if Max(i, t)<>t then
    halt(13);
  if Max(t, i)<>t then
    halt(14);
  if Max(sizeof(t),sizeof(t))<>sizeof(t) then
    halt(15);
  if Max(t,t)<>t then
    halt(16);
  if Max(i,i)<>i then
    halt(17);
  writeln('ok');
end.
