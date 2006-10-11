{$mode macpas}
program ptest;

type
	Point = record
		v: integer;
		h: integer;
	end;

procedure Test
(
	arg1	: longint;
	arg2	: integer;
	arg3	: boolean;
	arg4	: longint;
	arg5	: longint;
	arg6	: longint;
	arg7	: longint;
	arg8	: longint;
	arg9	: longint;
	arg10: longint
);

label
	ErrExit;
	
var
	i, n: integer;
	p1, p2, unused1: Point;
	dx, dy: Single;
	unused2, unused3: integer;
	unused4: longint;
begin
	arg4:= 1;
	n:= 0;
	dx:= 1.0;
	dy:= 1.0;
	p2.h:= 1;
	p2.v:= 2;

	for i:=0 TO arg4-1 do begin
		p1.h:=p2.h + round(3*dx);
		p1.v:=p2.v + round(3*dy);
		writeln('p1.h=', p1.h, ', p1.v=', p1.v);
                if (p1.h <> 4) then
                  halt(1);
	end;

	if n<>0 then goto ErrExit;

	ErrExit:
end;

	
begin
	Test(0, 0, false, 1, 0, 0, 0, 0, 0, 0);	
end.
