Program Example6;

Uses strings;

{ Program to demonstrate the StrEnd function. }

Const P : PChar = 'This is a PCHAR string.';

begin
  If Longint(StrEnd(P))-Longint(P)<>StrLen(P) then
    Writeln('Something is wrong here !')
  else
    Writeln ('All is well..');
end.
