program bug;

function Func(a:longint): longint;
begin
  if (a >= 0) then Func:=Trunc(1.0*a) else Func:=-Trunc(1.0*a);
end;

begin
  if Func(100) <> 100 then
    halt(1);
end.
