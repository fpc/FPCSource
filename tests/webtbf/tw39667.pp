{ %fail }
{$mode objfpc}
type
	Indirect = class
		hasNoOffsetInMyObj: int32;
	end;

	PMyObj = ^MyObj;
	MyObj = record
		c: Indirect;
	end;

const
	WrongOffset = PtrUint(@PMyObj(nil)^.c.hasNoOffsetInMyObj); // 8 on x64

begin
end.
