program bug1;

function f : tobject;
begin
  f := tobject.create;
end;

var
  s : single;
begin
  s := single(f);
end.
