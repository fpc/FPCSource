Program Example40;

{ Program to demonstrate the stddev function. }

Uses Math;

Type
  TExArray = Array[1..10000] of Float;

Var
  I : Integer;
  ExArray : TExArray;

begin
  Randomize;
  for I:=1 to 10000 do
    ExArray[i]:=Randg(1,0.2);
  Writeln('StdDev     : ',StdDev(ExArray):8:4);
  Writeln('StdDev (b) : ',StdDev(@ExArray[0],10000):8:4);
end.
