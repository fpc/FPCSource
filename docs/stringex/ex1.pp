Program Example1;

Uses strings;

{ Program to demonstrate the StrLen function. }

Const P : PChar = 'This is a constant pchar string';

begin
  Writeln ('P         : ',p);
  Writeln ('length(P) : ',StrLen(P));
end.
