{ %opt=-O2 }
{ %norun }

procedure Analyze(buf: pByte);
var
	i: SizeInt;
begin
	for i := 0 to 9 do write(buf[i], ' ');
end;

procedure DoSomething(index, cellSize: SizeUint);
var
	buf: array[0 .. 127] of byte;
begin
	(pByte(buf) + index * cellSize)^ := 5;
	Analyze(buf);
end;

procedure DoSomething2(index, cellSize: SizeUint);
var
	buf: array[0 .. 127] of byte;
begin
	(pByte(buf) + index * cellSize)^ := 5;
end;

begin
end.
