Program Example14;

Uses strings;

{ Program to demonstrate the StrLower and StrUpper functions. }

Const
    P1 : PChar = 'THIS IS AN UPPERCASE PCHAR STRING';
    P2 : PChar = 'this is a lowercase string';

begin
  Writeln ('Uppercase : ',StrUpper(P2));
  StrLower (P1);
  Writeln ('Lowercase : ',P1);
end.
