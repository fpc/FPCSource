{ %version=1.1 }
{ %recompile }

uses ub0421a;

var
  c : cl3;
begin
  c:=cl3.create;
  writeln(c.f);
  if (c.f<>10) then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
