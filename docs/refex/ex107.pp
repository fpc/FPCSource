Program Example107;

{ Program to demonstrate the IndexWord function. }

Const
  ArraySize = 1000;
  MaxValue = 1000;

Var
  Buffer : Array[1..ArraySize] of Word;
  I,J : longint;
  K : Word;

begin
  Randomize;
  For I:=1 To ArraySize do
    Buffer[I]:=Random(MaxValue);
  For I:=1 to 10 do
    begin
    K:=Random(MaxValue);
    J:=IndexWord(Buffer,ArraySize,K);
    if J=-1 then
      Writeln('Value ',K,' was not found in buffer.')
    else
      Writeln('Found ',K,' at position ',J,' in buffer');
    end;
end.
