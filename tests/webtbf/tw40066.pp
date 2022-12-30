{ %fail }
const
	Outer: array[0 .. 1] of record
		inner: array[0 .. 1] of uint32;
	end =
	(
		(inner: ()),
		(inner: (1))
	);

begin
end.
