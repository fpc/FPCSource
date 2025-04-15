{ %opt=-O2 }
var
	a, b: int32;
	x, y: byte;
begin
	x := PtrUint(@a) mod 32;
	y := PtrUint(@b) mod 32;
	writeln(x, ' ', y);
end.
