{ Old file: tbs0122.pp }
{ exit() gives a warning that the result is not set     OK 0.99.6 (FK) }


function f:longint;
begin
  exit(1);
end;

begin
  writeln(f);
end.
