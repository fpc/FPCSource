Program Example72;

{ Program to demonstrate the Upcase function. }

Var I : Longint;

begin
  For i:=ord('a') to ord('z') do
    write (upcase(chr(i)));
  Writeln;
  { This doesn't work in TP, but it does in Free Pascal }
  Writeln (Upcase('abcdefghijklmnopqrstuvwxyz'));
end.
