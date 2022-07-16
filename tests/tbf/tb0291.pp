{ %fail }
{$mode objfpc}
type
	InnerClass = class
		x: integer;
	end;

	OuterClass = class
		inner: InnerClass;
	end;

const
	OffsetOfXInOuterClass = PtrUint(@OuterClass(nil).inner.x);

begin
end.
