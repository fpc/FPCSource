Program Example17;

Uses strings;

{ Program to demonstrate the StrDispose function. }

Const P1 : PChar = 'This is a PChar string';

var P2 : PChar;

begin
  Writeln ('Before StnNew : Memory available : ',MemAvail);
  P2:=StrNew (P1);
  Writeln ('After StrNew : Memory available : ',MemAvail);
  Writeln ('P2 : ',P2);
  StrDispose(P2);
  Writeln ('After StrDispose : Memory available : ',MemAvail);
end.
