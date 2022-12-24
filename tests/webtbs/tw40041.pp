{ %OPT=-O4 }
{$mode objfpc}
function ShiftPM(x: single): single; noinline;
begin
	result := x + 10.0 - 1.0;
end;

function ShiftMP(x: single): single; noinline;
begin
	result := x - 1.0 + 10.0;
end;

begin
	if ShiftPM(5.0) <> 14.0 then
	begin
		writeln('ShiftPM: got ', ShiftPM(5.0):0:1, ', expected 14.0.');
		halt(1);
	end;

	if ShiftMP(5.0) <> 14.0 then
	begin
		writeln('ShiftMP: got ', ShiftMP(5.0):0:1, ', expected 14.0.');
		halt(2);
	end;
end.
