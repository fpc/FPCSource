{ %FAIL }
{ Old file: tbf0242.pp }
{ Crash when passing a procedure to formal parameter   OK 0.99.11 (PM) }

procedure p;
begin
end;

procedure p1(var x);
begin
end;

begin
  p1(p);
end.
