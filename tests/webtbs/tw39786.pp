{ %OPT=-O4 -Sg }
{$mode objfpc} {$h+}
label A0, A1, B0, B1, C0, C1;

var
	x, y: single;
	dontDrop: string;
	size: ptrint;

begin
	x := random;
A0:
	y := x * 1 * 2 * 3 * 4 * 5 * 6; // folds
A1:
	writestr(dontDrop, y);
	size := CodePointer(@A1) - CodePointer(@A0);
	writeln('x * 1 * 2 * 3 * 4 * 5 * 6: ', CodePointer(@A1) - CodePointer(@A0), ' b');

B0:
	y := x * 1 / 2 * 3 / 4 * 5 / 6; // doesn’t fold
B1:
	writestr(dontDrop, y);
	if CodePointer(@B1) - CodePointer(@B0) > size then
	  halt(1);
	writeln('x * 1 / 2 * 3 / 4 * 5 / 6: ', CodePointer(@B1) - CodePointer(@B0), ' b');

C0:
	y := x / 1 / 2 / 3 / 4 / 5 / 6; // doesn’t fold
C1:
	writestr(dontDrop, y);
	if CodePointer(@C1) - CodePointer(@C0) > size then
	  halt(1);
	writeln('x / 1 / 2 / 3 / 4 / 5 / 6: ', CodePointer(@C1) - CodePointer(@C0), ' b');
end.
