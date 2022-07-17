{ %fail }
{$mode objfpc}

type
	PRec = ^Rec;
	Rec = record
		x: array[2 .. 4] of integer;
	end;

const
	OffsetOfX1 = PtrUint(@PRec(nil)^.x[1]);

begin
end.
