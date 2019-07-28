program random_test;
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}
const
   l: UInt64 = 6148914691236517205;
var
   s,n: UInt32;
   i,j: UInt32;
begin
  WriteLn('Experiment:', LineEnding);
  WriteLn(' Draw a natural number r from the intervall [0,l-1] and');
  WriteLn(' increment a counter s when r < l div 2 is satisfied.');
  WriteLn(' Repeat this step n times and calculate the ratio s/n.', LineEnding);

  WriteLn(' Expected ratio: ', (l div 2)/l:30, LineEnding);

  WriteLn('Input size n':16, 'Observed ratio s/n':30);
  l := 6148914691236517205;
  for j := 4 to 18 do
  begin
    n := (Int32(1) shl j);
    s := 0;
    for i := 0 to n-1 do
      if Random(Int64(l)) < l div 2 then
        Inc(s);
    WriteLn( (UInt64(1) shl j):16, s/n:30);
  end;
  if abs(0.5-(s/n))>0.1 then
    halt(1);
end.
