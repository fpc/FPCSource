{$mode objfpc} {$longstrings on} {$coperators on} {$modeswitch arrayoperators}
generic procedure SetArray<T>(out a: specialize TArray<T>; const data: array of T);
begin
	a := Copy(data);
end;

generic function ArraysEqual<T>(const a, b: array of T): boolean;
var
	i: SizeInt;
begin
	if length(a) <> length(b) then exit(false);
	for i := 0 to High(a) do
		if a[i] <> b[i] then exit(false);
	result := true;
end;

generic function ToString<T>(const a: array of T): string;
var
	i: SizeInt;
	es: string;
begin
	result := '(';
	for i := 0 to High(a) do
	begin
		if i > 0 then result += ', ';
		WriteStr(es, a[i]);
		result += es;
	end;
	result += ')';
end;

var
	somethingFailed: boolean = false;

generic function Verify<T>(const a, ref: array of T; const what: string): boolean;
begin
	result := specialize ArraysEqual<T>(a, ref);
	if not result then
	begin
		writeln(what + ':' + LineEnding +
			specialize ToString<T>(a) + ',' + LineEnding +
			'expected:' + LineEnding +
			specialize ToString<T>(ref) + '.' + LineEnding);
		somethingFailed := true;
	end;
end;

generic procedure SetConcat<T>(var a: specialize TArray<T>; const p0, p1: specialize TArray<T>);
begin
	a := p0 + p1;
end;

generic procedure SetConcat<T>(var a: specialize TArray<T>; const p0, p1, p2: specialize TArray<T>);
begin
	a := p0 + p1 + p2;
end;

generic procedure Test<T>(const vs: array{[0 .. 5]} of T; const typename: string);
var
	a, b, c, d, ref: array of T;
	preva: pointer;
	i, tries: int32;
begin
	// Back in the day, delete() with huge count could crash, or corrupt some memory and produce an invalid array with negative length.
	specialize SetArray<T>(a, vs[0 .. 3]);
	delete(a, 2, High(SizeInt));
	if length(a) = 2 then
		specialize Verify<T>(a, vs[0 .. 1], 'delete(' + specialize ToString<T>(vs[0 .. 3]) + ', start = 2, count = High(SizeInt))')
	else
	begin
		writeln('Length after delete(' + specialize ToString<T>(vs[0 .. 3]) + ', start = 2, count = High(SizeInt)) is ', length(a), ', expected 2.', LineEnding);
		somethingFailed := true;
	end;

	specialize SetArray<T>(a, vs[0 .. 3]);
	specialize SetArray<T>(b, [vs[4]]);
	specialize SetArray<T>(c, [vs[5]]);
	tries := 0;
	repeat
		if tries >= 100 then
		begin
			writeln('dynarray_concat_multi(' + typename + ') has no append optimization.', LineEnding);
			somethingFailed := true;
			break;
		end;
		preva := pointer(a);
		specialize SetConcat<T>(a, a, b, c);

		ref := Copy(vs, 0, 4);
		SetLength(ref, 4 + 2 * (1 + tries));
		for i := 4 to High(ref) do ref[i] := vs[4 + i and 1];
		inc(tries);
	until not specialize Verify<T>(a, ref, 'dynarray_concat_multi(' + typename + ')') or (pointer(a) = preva);

	specialize SetArray<T>(a, vs[0 .. 3]);
	specialize SetArray<T>(b, [vs[4]]);
	tries := 0;
	repeat
		if tries >= 100 then
		begin
			writeln('dynarray_concat(' + typename + ') has no append optimization.', LineEnding);
			somethingFailed := true;
			break;
		end;
		preva := pointer(a);
		specialize SetConcat<T>(a, a, b);

		ref := Copy(vs, 0, 4);
		SetLength(ref, 4 + (1 + tries));
		for i := 4 to High(ref) do ref[i] := vs[4];
		inc(tries);
	until not specialize Verify<T>(a, ref, 'dynarray_concat(' + typename + ')') or (pointer(a) = preva);

	specialize SetArray<T>(a, [vs[0]]);
	specialize SetArray<T>(b, []);
	specialize SetArray<T>(c, vs[1 .. 2]);
	specialize SetArray<T>(d, []);
	specialize SetConcat<T>(a, b, c, d);
	if specialize Verify<T>(a, vs[1 .. 2], '() + ' + specialize ToString<T>(vs[1 .. 2]) + ' + ()') and (pointer(a) <> pointer(c)) then
	begin
		writeln('dynarray_concat_multi(' + typename + ') does not reuse the only nonempty input.', LineEnding);
		somethingFailed := true;
	end;

	specialize SetArray<T>(a, [vs[0]]);
	specialize SetArray<T>(b, vs[1 .. 2]);
	specialize SetArray<T>(c, []);
	specialize SetConcat<T>(a, b, c);
	if specialize Verify<T>(a, vs[1 .. 2], specialize ToString<T>(vs[1 .. 2]) + ' + ()') and (pointer(a) <> pointer(b)) then
	begin
		writeln('dynarray_concat(' + typename + ') does not reuse the only nonempty input #1.', LineEnding);
		somethingFailed := true;
	end;

	specialize SetArray<T>(a, [vs[0]]);
	specialize SetArray<T>(b, []);
	specialize SetArray<T>(c, vs[1 .. 2]);
	specialize SetConcat<T>(a, b, c);
	if specialize Verify<T>(a, vs[1 .. 2], '() + ' + specialize ToString<T>(vs[1 .. 2])) and (pointer(a) <> pointer(c)) then
	begin
		writeln('dynarray_concat(' + typename + ') does not reuse the only nonempty input #2.', LineEnding);
		somethingFailed := true;
	end;
end;

function CopyStr(const src: string): string;
begin
	result := System.Copy(src, 1, length(src));
end;

begin
	specialize Test<int32>([1, 2, 3, 4, 5, 6], 'int32');
	specialize Test<string>([CopyStr('S1'), CopyStr('S2'), CopyStr('S3'), CopyStr('S4'), CopyStr('S5'), CopyStr('S6')], 'string');

	if not somethingFailed then writeln('ok');
	if somethingFailed then halt(1);
end.
