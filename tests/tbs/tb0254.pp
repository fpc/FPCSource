{ Old file: tbs0294.pp }
{ parameter with the same name as function is allowed in tp7/delphi Yes, but in BP this leads to being unable to set the return value ! }

{$mode tp}
{ this is allowed in BP !!!
  but its complete nonsense because
  this code sets parameter test
  so the return value can not be set at all !!!!!
  of course in Delphi you can use result so there it
  makes sense to allow this ! PM }
function test(var test:longint):longint;
var
  x : longint;
begin
  { in BP the arg is change here !! }
  test:=1;
  x:=3;
end;

function st(var st : string) : string;
begin
  st:='OK';
end;

var t : longint;
    myst : string;
begin
  t:=2;
  myst:='Before';
  test(t);
  st(myst);
  if (t<>1) then
    begin
       writeln('Test arg in Test function is not handled like in BP');
       halt(1);
    end;
  if (myst<>'OK') then
    begin
       writeln('St arg in St string function is not handled like in BP');
       halt(1);
    end;
end.
