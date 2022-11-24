{$overflowchecks on}
var
	n: SizeUint;
begin
	n := 14 + random(0);
	n := n - n mod 4;
	writeln(n);
end.
