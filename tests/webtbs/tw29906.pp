{$mode objfpc}

type
  tsub = 1..19;

function test(s: tsub): longint;
begin
  { use different number of instructions for several cases so wrongly
    calculated jump table offsets are more likely to wreak havoc }
  case s of
    1:
     result:=1;
    2:
      begin
        writeln('two');
        result:=2;
      end;
    3:
      begin
        s:=4;
        result:=3;
      end;
    4:
      begin
        result:=4;
        s:=s*s+result;
      end;
    5:
      result:=5;
    6:
      result:=6;
    7:
      begin
        s:=s+s*s div s;
        result:=7;
      end;
    8:
      result:=8;
    9:
      result:=9;
    10:
      result:=10;
    11:
      result:=11;
    12:
      result:=12;
    13:
      result:=13;
    14:
      result:=14;
    15:
      result:=15;
    16:
      result:=16;
    17:
      result:=17;
    18:
      result:=18;
    19:
      result:=19;
  end;
end;

var
  i: tsub;
begin
  for i:=low(tsub) to high(tsub) do
    if test(i)<>i then
      halt(i);
end.
