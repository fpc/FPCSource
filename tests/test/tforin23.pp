{$mode objfpc}
{$apptype console}

// test enumerating multidimensional arrays with and without conversion

type
  TSingleDimArray = array[0..1] of integer;
  TMultyDimArray = array[0..1] of TSingleDimArray;
var
  MultyDimArray: TMultyDimArray;
  a: TSingleDimArray;
  i: integer;
begin
  MultyDimArray[0,0]:=1;
  MultyDimArray[0,1]:=2;
  MultyDimArray[1,0]:=3;
  MultyDimArray[1,1]:=4;
  for a in MultyDimArray do
    for i in a do
      WriteLn(i);

  for i in MultyDimArray do
    Writeln(i);
end.
