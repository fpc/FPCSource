procedure FillSomeQWords;
var
	q: array[0 .. 9] of uint64;
	rep: int32;
begin
	for rep := 1 to 128 * 1024 * 1024 do
		FillQWord(q, length(q), 1234);
end;

begin
	FillSomeQWords;
end.
