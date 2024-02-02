program tw40618;

var
	HiTimes: int32 = 0;

function Hi: int32;
begin
	writeln('Hi called.');
	inc(HiTimes);
	Hi := 0;
end;

begin
	case Hi of
		0 .. 2: ;
		else halt(1);
	end;
	if HiTimes <> 1 then
	begin
		writeln('Hi called ', HiTimes, ' times, expected 1.');
		halt(2);
	end
	else
		writeln('OK');
end.

