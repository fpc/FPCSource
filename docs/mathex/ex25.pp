Program Example25;

{ Program to demonstrate the MaxIntValue function. }

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
  Writeln(MaxIntValue(ExArray));
end.
