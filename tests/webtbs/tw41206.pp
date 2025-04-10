{ %OPT=-O3 }
procedure f(constref x: int32); noinline;
begin
end;

var
	i: int32;

begin
	for i := 0 to 9 do f(i);
end.
