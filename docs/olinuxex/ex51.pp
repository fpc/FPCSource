Program Example51;

{ Program to demonstrate the StringToPPChar function. }

Uses oldlinux;

var P : PPChar;
    S : String;
begin
  S:='/bin/ls -l -F';
  P:=StringToPPChar(S);
  Writeln ('Name     : ',p^); inc(longint(p),4);
  writeln ('Option 1 : ',p^); inc(longint(p),4);
  writeln ('Option 2 : ',p^);
  end.
