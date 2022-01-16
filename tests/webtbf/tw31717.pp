{ %fail }
{ %OPT=-vn -Sen }
{$mode objfpc}
program Project1;

type

 TObj = Class

  procedure proc1;
  procedure proc2; inline;

 End;

procedure TObj.proc1;
begin
 proc2;
end;

procedure TObj.proc2;
begin
  writeln('2');
end;

begin

 tobj.create.proc1;

end.
