Program Example104;

{ Program to demonstrate the FillDWord function. }

Const
  ArraySize = 1000;

Var
  S : Array [1..ArraySize] of DWord;
  I : longint;

begin
  FillDWord(S,ArraySize,0);
  For I:=1 to ArraySize do
    If S[i]<>0 then
      Writeln('Position ',i,' not zeroed out');
end.
