Program Example40;

{ Program to demonstrate the randg function. }

Uses Math;

Type
  TExArray = Array[1..10000] of Float;

Var
  I : Integer;
  ExArray : TExArray;
  Mean,stddev : Float;

begin
  Randomize;
  for I:=1 to 10000 do
    ExArray[i]:=Randg(1,0.2);
  MeanAndStdDev(ExArray,Mean,StdDev);
  Writeln('Mean       : ',Mean:8:4);
  Writeln('StdDev     : ',StdDev:8:4);
end.
