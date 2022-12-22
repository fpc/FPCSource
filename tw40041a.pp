{$mode objfpc}
function Shift(x: single): single; noinline;
begin
	result := x + 10.0 - 1.0;
end;

begin
	if Shift(5.0) <> 14.0 then
	begin
		writeln('Got ', Shift(5.0):0:1, ', expected 14.0.');
		halt(1);
	end;
end.
