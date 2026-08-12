{ Failure in multiple-array concatenation }
program tw41798;

{$mode objfpc}
{$modeswitch arrayoperators}

type
  TIntArr = array of PtrInt;

var
  x, y: TIntArr;
begin
  x := [1, 2];
  { the two literal temps must not alias the last one }
  y := x + [3, 4] + [5, 6];
  if (Length(y) <> 6) or
     (y[0] <> 1) or (y[1] <> 2) or (y[2] <> 3) or
     (y[3] <> 4) or (y[4] <> 5) or (y[5] <> 6) then
    halt(1);
end.
