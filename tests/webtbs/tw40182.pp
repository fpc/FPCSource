{ %opt=-CO }

program test;
function somenumber: int64;
begin
  exit(2);
end;

function foo: int64;
begin
  exit (0*somenumber); //Internal error 2014011604
end;

begin
end.