Program Example100;

{ Program to demonstrate the CompareChar function. }

Const
  ArraySize     = 100;
  HalfArraySize = ArraySize Div 2;

Var
  Buf1,Buf2 : Array[1..ArraySize] of char;
  I : longint;

  Procedure CheckPos(Len : Longint);

  Begin
    Write('First ',Len,' characters are ');
    if CompareChar(Buf1,Buf2,Len)<>0 then
      Write('NOT ');
    Writeln('equal');
  end;

  Procedure CheckNullPos(Len : Longint);

  Begin
    Write('First ',Len,' non-null characters are ');
    if CompareChar0(Buf1,Buf2,Len)<>0 then
      Write('NOT ');
    Writeln('equal');
  end;

begin
  For I:=1 to ArraySize do
    begin
    Buf1[i]:=chr(I);
    If I<=HalfArraySize Then
      Buf2[I]:=chr(I)
    else
      Buf2[i]:=chr(HalfArraySize-I);
    end;
  CheckPos(HalfArraySize div 2);
  CheckPos(HalfArraySize);
  CheckPos(HalfArraySize+1);
  CheckPos(HalfArraySize + HalfArraySize Div 2);
  For I:=1 to 4 do
    begin
    buf1[Random(ArraySize)+1]:=Chr(0);
    buf2[Random(ArraySize)+1]:=Chr(0);
    end;
  Randomize;
  CheckNullPos(HalfArraySize div 2);
  CheckNullPos(HalfArraySize);
  CheckNullPos(HalfArraySize+1);
  CheckNullPos(HalfArraySize + HalfArraySize Div 2);
end.
