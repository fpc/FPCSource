Program Example13;

Uses strings;

{ Program to demonstrate the StrScan and StrRScan functions. }

Const P : PChar = 'This is a PCHAR string.';
      S : Char = 's' ;

begin
  Writeln ('P, starting from first ''s'' : ',StrScan(P,s));
  Writeln ('P, starting from last ''s'' : ',StrRScan(P,s));
end.
