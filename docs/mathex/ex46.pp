Program Example45;

{ Program to demonstrate the SumOfSquares function. }

Uses math;

Type
  TExArray = Array[1..100] of Float;

Var
  I : Integer;
  ExArray : TExArray;
  s,ss : float;

begin
  Randomize;
  for I:=1 to 100 do
    ExArray[i]:=(Random-Random)*100;
  Writeln('Max             : ',MaxValue(ExArray):8:4);
  Writeln('Min             : ',MinValue(ExArray):8:4);
  SumsAndSquares(ExArray,S,SS);
  Writeln('Sum             : ',S:8:4);
  Writeln('Sum squares     : ',SS:8:4);
  SumsAndSquares(@ExArray[1],100,S,SS);
  Writeln('Sum (b)         : ',S:8:4);
  Writeln('Sum squares (b) : ',SS:8:4);
end.
