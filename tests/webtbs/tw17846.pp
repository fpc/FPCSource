program opoverload;

{$mode objfpc}

const
  IntValue1 = $1;
  IntValue2 = $2;
  IntValue3 = $4;

type
  TMyEnum = (Value1, Value2, Value3);
  TMySet = set of TMyEnum;

operator := (aRight: LongWord) aLeft: TMySet;
begin
  aLeft := [];
  if aRight and IntValue1 <> 0 then
    Include(aLeft, Value1);
  if aRight and IntValue2 <> 0 then
    Include(aLeft, Value2);
  if aRight and IntValue3 <> 0 then
    Include(aLeft, Value3);
end;

operator := (aRight: TMySet) aLeft: LongWord;
begin
  aLeft := 0;
  if Value1 in aRight then
    aLeft := aLeft or IntValue1;
  if Value2 in aRight then
    aLeft := aLeft or IntValue2;
  if Value3 in aRight then
    aLeft := aLeft or IntValue3;
end;

var
  i: LongWord;
  t: TMySet;
begin
  i := IntValue1 or IntValue3;
  t := i;
  if t<>[value1,value3] then
    halt(1);
  i:=0;
  i:=t;
  if i<>(IntValue1 or IntValue3) then
    halt(2);
end.
