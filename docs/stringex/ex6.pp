Program Example6;

Uses strings;

{ Program to demonstrate the StrECopy function. }

Const P : PChar = 'This is a PCHAR string.';

Var PP : PChar;

begin
  PP:=StrAlloc (StrLen(P)+1);
  If Longint(StrECopy(PP,P))-Longint(PP)<>StrLen(P) then
    Writeln('Something is wrong here !')
  else
    Writeln ('PP= ',PP);
end.
