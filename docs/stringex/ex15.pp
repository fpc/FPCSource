Program Example15;

Uses strings;

{ Program to demonstrate the StrPos function. }

Const P : PChar = 'This is a PChar string.';
      S : Pchar = 'is';
begin
  Writeln ('Position of ''is'' in P : ',longint(StrPos(P,S))-Longint(P));
end.
