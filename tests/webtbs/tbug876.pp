{$OPT=-pg}

program test1;
var
 i,j:longint;
 a,b:double;

procedure test;
begin
 b:=1.0;
 i:=2;
 a:=b+3;
 j:=i div 2;
end;

procedure test2;
begin
  test;
end;

begin
  test;
  test2;
  test;
end.