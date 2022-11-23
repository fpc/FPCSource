{$define unsigned}
const
	Divisor = 17;
	TestRange = 3 * Divisor;
var
	i, x, r1, r2: {$ifdef unsigned} uint32 {$else} int32 {$endif};
	ok: boolean;
begin
	ok := true;
	for i := {$ifdef unsigned} 0 {$else} -2 * TestRange {$endif} to 2 * TestRange do
	begin
	{$ifndef unsigned}
		if i < -TestRange then
			x := Low(x) + (i - (-2 * TestRange)) // test [Low(x); Low(x) + TestRange)
		else
	{$endif}
		if i <= TestRange then x := i            // test [-TestRange; TestRange] or [0; TestRange]
		else x := High(x) - (i - TestRange - 1); // test (High(x) - TestRange; High(x)]

		r1 := x - x mod Divisor;
		r2 := x div Divisor * Divisor;

		if r1 <> r2 then
		begin
			writeln('FAIL: x=', x, ', x - x mod ', Divisor, ' = ', r1, ', x div ', Divisor, ' * ', Divisor, ' = ', r2);
			ok := false;
		end;
	end;
	if ok then 
	  writeln('ok')
	else
	  halt(1);
end.
