{ %fail }
{$mode objfpc}

type
	NestedRec = record
		x, y: integer;
	end;

	PRec = ^Rec;
	Rec = record
		items: array of NestedRec;
	end;

const
	OffsetOf2Y = PtrUint(@PRec(nil)^.items[2].y);

begin
end.
