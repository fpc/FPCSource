{ %FAIL }

{$mode objfpc}
generic function Size<T>: SizeUint;
begin
	result := sizeof(T);
end;

var
	a: array[0 .. specialize Size<double> - 1] of byte;

begin
end.

