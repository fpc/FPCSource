Program Example30;

{ Program to demonstrate the MinIntValue function. }

{ Make sore integer is 32 bit}
{$mode objfpc}

Uses math;

Type
  TExArray = Array[1..100] of Integer;

Var
  I : Integer;
  ExArray : TExArray;

begin
  Randomize;
  for I:=1 to 100 do
    ExArray[i]:=Random(I)-Random(100);
  Writeln(MinIntValue(ExArray));
end.
