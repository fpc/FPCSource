Program Example26;

{ Program to demonstrate the MinValue function. }

{ Make sore integer is 32 bit}
{$mode objfpc}

Uses math;

Type
  TExFloatArray = Array[1..100] of Float;
  TExIntArray = Array[1..100] of Integer;

Var
  I : Integer;
  ExFloatArray : TExFloatArray;
  AFloatArray : PFloat;
  ExIntArray : TExIntArray;
  AintArray : PInteger;

begin
  Randomize;
  AFloatArray:=@ExFloatArray[0];
  AIntArray:=@ExIntArray[0];
  for I:=1 to 100 do
    ExFloatArray[i]:=(Random-Random)*100;
  for I:=1 to 100 do
    ExIntArray[i]:=Random(I)-Random(100);
  Writeln('Min Float       : ',MinValue(ExFloatArray):8:4);
  Writeln('Min Float   (b) : ',MinValue(AFloatArray,100):8:4);
  Writeln('Min Integer     : ',MinValue(ExIntArray):8);
  Writeln('Min Integer (b) : ',MinValue(AintArray,100):8);
end.
