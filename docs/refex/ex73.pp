Program Example73;

{ Program to demonstrate the Lowercase function. }

Var I : Longint;

begin
  For i:=ord('A') to ord('Z') do
    write (lowercase(chr(i)));
  Writeln;
  Writeln (Lowercase('ABCDEFGHIJKLMNOPQRSTUVWXYZ'));
end.
