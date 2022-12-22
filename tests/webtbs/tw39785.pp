{ %OPT=-O4 -Sg }
{$mode objfpc} {$h+}
label A0, A1, B0, B1, C0, C1, D0, D1;

var
	x, y: single;
    size: ptrint;

begin
	x := random(0);
A0:
	y := x + 1 + 2 + 3 + 4 + 5 + 6; // folds
A1:
    size:=CodePointer(@A1) - CodePointer(@A0);
	writeln('x + 1 + 2 + 3 + 4 + 5 + 6 = ', y, ' ', CodePointer(@A1) - CodePointer(@A0), ' b');
	if y <> 21 then
	begin
		writeln('Expected 21!');
		halt(1);
	end;

B0:
	y := x + 1 - 2 + 3 - 4 + 5 - 6; // doesn’t fold
B1:
    if CodePointer(@B1) - CodePointer(@B0) > size then
      halt(1);
	writeln('x + 1 - 2 + 3 - 4 + 5 - 6 = ', y, ' ', CodePointer(@B1) - CodePointer(@B0), ' b');
	if y <> -3 then
	begin
		writeln('Expected -3!');
		halt(2);
	end;

C0:
	y := x - 1 - 2 - 3 - 4 - 5 - 6; // didn't fold at the time of making the issue, now folds
C1:
    if CodePointer(@C1) - CodePointer(@C0) > size then
      halt(1);
	writeln('x - 1 - 2 - 3 - 4 - 5 - 6 = ', y, ' ', CodePointer(@C1) - CodePointer(@C0), ' b');
	if y <> -21 then
	begin
		writeln('Expected -21!');
		halt(3);
	end;

D0:
	y := x - 1 + 2 - 3 + 4 - 5 + 6; // didn't fold at the time of making the issue, now folds
D1:
    if CodePointer(@D1) - CodePointer(@D0) > size then
      halt(1);
	writeln('x - 1 + 2 - 3 + 4 - 5 + 6 = ', y, ' ', CodePointer(@D1) - CodePointer(@D0), ' b');
	if y <> 3 then
	begin
		writeln('Expected 3!');
		halt(4);
	end;
end.
