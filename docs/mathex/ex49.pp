Program Example49;

{ Program to demonstrate the TotalVariance function. }

Uses math;

Type
  TExArray = Array[1..100] of Float;

Var
  I : Integer;
  ExArray : TExArray;
  TV : float;

begin
  Randomize;
  for I:=1 to 100 do
    ExArray[i]:=(Random-Random)*100;
  TV:=TotalVariance(ExArray);
  Writeln('Total variance     : ',TV:8:4);
  TV:=TotalVariance(@ExArray[1],100);
  Writeln('Total Variance (b) : ',TV:8:4);
end.
