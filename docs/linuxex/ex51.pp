Program Example51;

{ Program to demonstrate the StringToPPChar function.
  This function is pretty obsolete }

Uses UnixUtil;

var P : PPChar;
    S : String;
begin
  S:='/bin/ls -l -F';
  P:=StringToPPChar(S,0);
  Writeln ('Name     : ',p^); inc(longint(p),4);
  writeln ('Option 1 : ',p^); inc(longint(p),4);
  writeln ('Option 2 : ',p^);
end.
