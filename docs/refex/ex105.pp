Program Example105;

{ Program to demonstrate the IndexByte function. }

Const
  ArraySize = 256;
  MaxValue = 256;

Var
  Buffer : Array[1..ArraySize] of Byte;
  I,J : longint;
  K : Byte;

begin
  Randomize;
  For I:=1 To ArraySize do
    Buffer[I]:=Random(MaxValue);
  For I:=1 to 10 do
    begin
    K:=Random(MaxValue);
    J:=IndexByte(Buffer,ArraySize,K);
    if J=-1 then
      Writeln('Value ',K,' was not found in buffer.')
    else
      Writeln('Found ',K,' at position ',J,' in buffer');
    end;
end.
