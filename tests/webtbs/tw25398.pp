{$mode objfpc}{$h+}
uses classes;

var
  b: tbits;
  i: integer;

begin
  b:= TBits.Create;
  b.Size:= 128;
  b.SetOn(0);
  b.SetOn(13);
  b.SetOn(118);
  i:= b.FindFirstBit(True);
  if i<>0 then
    halt(1);
  i:= b.FindNextBit;
  if i<>13 then
    halt(2);
  i:= b.FindNextBit;
  if i<>118 then
    halt(3);
  i:= b.FindNextBit;
  if i<>-1 then
    halt(4);
end.