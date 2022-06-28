program Project1;
{$mode objfpc}{$H+}
const
  ts=3;
type
  generic GTest<T>=record
    case byte of
    0:(ta:array [0..2] of T);
    1:(t1:T;t2:T;t3:T);
  end;

begin
end. 
