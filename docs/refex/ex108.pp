Program Example108;

{ Program to demonstrate the IndexChar function. }

Const
  ArraySize = 1000;
  MaxValue = 26;

Var
  Buffer : Array[1..ArraySize] of Char;
  I,J : longint;
  K : Char;

begin
  Randomize;
  For I:=1 To ArraySize do
    Buffer[I]:=chr(Ord('A')+Random(MaxValue));
  For I:=1 to 10 do
    begin
    K:=chr(Ord('A')+Random(MaxValue));
    J:=IndexChar(Buffer,ArraySize,K);
    if J=-1 then
      Writeln('Value ',K,' was not found in buffer.')
    else
      Writeln('Found ',K,' at position ',J,' in buffer');
    end;
end.
