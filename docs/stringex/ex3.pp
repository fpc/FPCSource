Program Example3;

Uses strings;

{ Program to demonstrate the StrPas function. }

Const P : PChar = 'This is a PCHAR string';

var S : string;

begin
  S:=StrPas (P);
  Writeln ('S : ',S);
end.
