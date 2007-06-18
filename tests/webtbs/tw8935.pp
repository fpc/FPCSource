{%cpu=x86_64,i386,arm}
{%result=229}

procedure DoTest1; safecall;
var
  i: integer;
begin
  i:=-1;
  i:=i - 1;
end;

function DoTest2: longint; safecall;
begin
  DoTest2:=$12345678;
end;

procedure DoTest3; safecall;
begin
  PChar(nil)^:='A';
end;

begin
  DoTest1;
  if DoTest2 <> $12345678 then
    Halt(1);
  DoTest3;
end.
