{ this is allowed in BP !!!
  but its complete nonsense because
  this code sets parameter test
  so the return value can not be set at all !!!!!
  of course in Delphi you can use result so there it
  makes sense to allow this ! PM }
function test(var test:longint):longint;
begin
  test:=1;
end;

var t : longint;

begin
  t:=2;
  { here you get garbage value with BP ! }
  Writeln('test(t=2) = ',test(t));
  Writeln('t after test = ',t);
end.
