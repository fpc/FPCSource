{ %VERSION=1.1 }
{ %FAIL }
{ %OPT=-Sew -vw }

uses ub0149;



procedure testdef1(b: tdefinition);
begin
  b:=12;
end;


type
  tdefinition = 1..10;

procedure testdef2(b : tdefinition);
begin
  b:=10;
end;


Begin
  testdef1(0);
  testdef2(0);
end.
