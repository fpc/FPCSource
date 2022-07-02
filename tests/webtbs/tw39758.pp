{ %OPT=-O4 -Sg }
{$mode objfpc} {$h+}
label A0, A1, B0, B1, C0, C1, D0, D1;

var
	x, y: single;
	dontDrop: string;
    size: ptrint;

begin
	x := random;
A0:
	y := x + 1 + 2 + 3 + 4 + 5 + 6; // folds
A1:
	writestr(dontDrop, y);
    size:=CodePointer(@A1) - CodePointer(@A0);
	writeln('x + 1 + 2 + 3 + 4 + 5 + 6 = ', y, ' ', CodePointer(@A1) - CodePointer(@A0), ' b');

B0:
	y := x + 1 - 2 + 3 - 4 + 5 - 6; // doesn’t fold
B1:
	writestr(dontDrop, y);
    if CodePointer(@B1) - CodePointer(@B0) > size then
      halt(1);
	writeln('x + 1 - 2 + 3 - 4 + 5 - 6 = ', y, ' ', CodePointer(@B1) - CodePointer(@B0), ' b');

C0:
	y := x - 1 - 2 - 3 - 4 - 5 - 6; // didn't fold at the time of making the issue, now folds
C1:
	writestr(dontDrop, y);
    if CodePointer(@C1) - CodePointer(@C0) > size then
      halt(1);
	writeln('x - 1 - 2 - 3 - 4 - 5 - 6 = ', y, ' ', CodePointer(@C1) - CodePointer(@C0), ' b');
D0:
	y := x - 1 + 2 - 3 + 4 - 5 + 6; // didn't fold at the time of making the issue, now folds
D1:
	writestr(dontDrop, y);
    if CodePointer(@D1) - CodePointer(@D0) > size then
      halt(1);
	writeln('x - 1 + 2 - 3 + 4 - 5 + 6 = ', y, ' ', CodePointer(@D1) - CodePointer(@D0), ' b');
end.
