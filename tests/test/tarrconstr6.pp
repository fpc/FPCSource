program tarrconstr6;

type
  TLongIntArray = array of LongInt;

function Test(aArr: array of LongInt): Integer;
begin
  Test := 1;
end;

function Test(aArr: TLongIntArray): Integer;
begin
  Test := 2;
end;

var
  la: TLongIntArray;
begin
  la := Nil;
  if Test([]) <> 1 then
    Halt(1);
  if Test([1, 2, 3]) <> 1 then
    Halt(2);
  if Test(TLongIntArray.Create(1, 2, 3)) <> 2 then
    Halt(3);
  if Test(la) <> 2 then
    Halt(4);
end.
