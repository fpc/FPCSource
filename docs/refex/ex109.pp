Program Example109;

{ Program to demonstrate the MoveChar0 function. }

Var
  Buf1,Buf2 : Array[1..80] of char;
  I : longint;

begin
  Randomize;
  For I:=1 to 80 do
    Buf1[i]:=chr(Random(16)+Ord('A'));
  Writeln('Original buffer');
  writeln(Buf1);
  Buf1[Random(80)+1]:=#0;
  MoveChar0(Buf1,Buf2,80);
  Writeln('Randomly zero-terminated Buffer');
  Writeln(Buf2);
end.
