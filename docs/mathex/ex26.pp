Program Example26;

{ Program to demonstrate the MaxValue function. }

{ Make sore integer is 32 bit}
{$mode objfpc}

Uses math;

Type
  TExFloatArray = Array[1..100] of Float;
  TExIntArray = Array[1..100] of Integer;

Var
  I : Integer;
  ExFloatArray : TExFloatArray;
  ExIntArray : TExIntArray;
  AFLoatArray : PFLoat;
  AIntArray : PInteger;
begin
  Randomize;
  AFloatArray:=@ExFloatArray[1];
  AIntArray:=@ExIntArray[1];
  for I:=1 to 100 do
    ExFloatArray[i]:=(Random-Random)*100;
  for I:=1 to 100 do
    ExIntArray[i]:=Random(I)-Random(100);
  Writeln('Max Float       : ',MaxValue(ExFloatArray):8:4);
  Writeln('Max Float   (b) : ',MaxValue(AFloatArray,100):8:4);
  Writeln('Max Integer     : ',MaxValue(ExIntArray):8);
  Writeln('Max Integer (b) : ',MaxValue(AIntArray,100):8);
end.
