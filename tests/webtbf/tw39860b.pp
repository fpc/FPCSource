{ %FAIL }

{$mode delphi}
function Size<T>: SizeUint;
begin
	result := sizeof(T);
end;

var
	a: array[0 .. Size<double> - 1] of byte;

begin
end.

