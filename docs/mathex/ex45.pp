Program Example45;

{ Program to demonstrate the SumOfSquares function. }

Uses math;

Type
  TExArray = Array[1..100] of Float;

Var
  I : Integer;
  ExArray : TExArray;

begin
  Randomize;
  for I:=1 to 100 do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max             : ',MaxValue(ExArray):8:4);
  Writeln('Min             : ',MinValue(ExArray):8:4);
  Writeln('Sum squares     : ',SumOfSquares(ExArray):8:4);
  Writeln('Sum squares (b) : ',SumOfSquares(@ExArray[1],100):8:4);
end.
