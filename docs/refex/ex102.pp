Program Example102;

{ Program to demonstrate the CompareWord function. }

Const
  ArraySize     = 100;
  HalfArraySize = ArraySize Div 2;

Var
  Buf1,Buf2 : Array[1..ArraySize] of Word;
  I : longint;

  Procedure CheckPos(Len : Longint);

  Begin
    Write('First ',Len,' words are ');
    if CompareWord(Buf1,Buf2,Len)<>0 then
      Write('NOT ');
    Writeln('equal');
  end;


begin
  For I:=1 to ArraySize do
    begin
    Buf1[i]:=I;
    If I<=HalfArraySize Then
      Buf2[I]:=I
    else
      Buf2[i]:=HalfArraySize-I;
    end;
  CheckPos(HalfArraySize div 2);
  CheckPos(HalfArraySize);
  CheckPos(HalfArraySize+1);
  CheckPos(HalfArraySize + HalfArraySize Div 2);
end.
