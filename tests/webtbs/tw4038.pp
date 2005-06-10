{ Source provided for Free Pascal Bug Report 4038 }
{ Submitted by "Antonio Marti" on  2005-06-01 }
{ e-mail: windowze2000@yahoo.es }
uses sysutils;
var b: byte;
begin
  writeln(executeprocess('echo','works1 works2 works3'));
  writeln(executeprocess('echo','works1 works2 works3'));
  writeln;
  for b:=1 to 2 do
    writeln(executeprocess('echo','fails1 fails2 fails3'));
end.
