{$mode objfpc} {$longstrings on} {$coperators on} {$modeswitch implicitfunctionspecialization}
{$warn 5092 off: Variable "ref" of a managed type does not seem to be initialized }

generic function ToString<T>(const a: array of T): string;
var
	i: SizeInt;
	es: string;
begin
	result := '(';
	for i := 0 to High(a) do
	begin
		WriteStr(es, a[i]);
		if i > 0 then result += ', ';
		result += es;
	end;
	result += ')';
end;

const
	WithoutWith: array[boolean] of string = ('without', 'with');

var
	somethingFailed: boolean = false;
	a, ref: array of int32;
	aBefore: pointer;
	i: SizeInt;
	tries: int32;
	withRealloc: boolean;

begin
	// withRealloc = false:
	// Catch a bug when inserting an array element into the same array to the left does NOT reallocate the array, shifts the element away, and uses the old pointer.
	// Reproducible without particular prerequisites.
	//
	// withRealloc = true:
	// Catch a bug when inserting an array element into the same array reallocates the array and uses the old pointer.
	// In practice requires -gh (heaptrc) or custom memory manager that would fill the old ReallocMem area with garbage to reproduce.
	for withRealloc in boolean do
	begin
		tries := 0;
		repeat
			if tries >= 99 then
			begin
				writeln('Cannot trigger an insert ' + WithoutWith[withRealloc] + ' reallocation.');
				halt(2);
			end;
			SetLength(a, 5 + tries);
			for i := 0 to High(a) do a[i] := i; // [0, 1, 2, 3, 4, ...]
			aBefore := pointer(a);
			Insert(a[3], a, 2); // a[3] is 3, so it must insert another 3 *before* 2: [0, 1, 3, 2, 3, 4, ...]
			inc(tries);
		until pointer(a) <> aBefore = withRealloc; // (:

		SetLength(ref, length(a));
		for i := 0 to High(ref) do
			ref[i] := i + ord(i = 2) - ord(i = 3) - ord(i > 3);
		if CompareByte(pointer(a)^, pointer(ref)^, length(a) * sizeof(a[0])) <> 0 then
		begin
			writeln('After inserting a[3] = 3 at position #2 ' + WithoutWith[withRealloc] + ' reallocation:' + LineEnding +
				'a = ' + ToString(a) + ',' + LineEnding +
				'expected:' + LineEnding +
				'a = ' + ToString(ref) + '.' + LineEnding);
			somethingFailed := true;
		end;
	end;

	if somethingFailed then halt(1);
	writeln('ok');
end.
