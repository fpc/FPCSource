{ %opt=-O2 }
{ %norun }
procedure DoSomething(index, cellSize: SizeUint);
var
	buf: array[0 .. 127] of byte;
begin
	(pByte(buf) + index * cellSize)^ := 5;
end;

begin
end.
