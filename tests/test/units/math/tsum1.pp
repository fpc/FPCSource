{ %opt=-Sc }
{$mode objfpc} {$longstrings on} {$typedaddress on} {$modeswitch advancedrecords} {$modeswitch duplicatelocals}
uses
	Math, SysUtils;

var
	anythingFailed: boolean = false;

type
	TestContext = record
		typeName: string;
		procedure Expect(const got, expected: float; const what: string);
		procedure Expect(const got, expected: array of float; const what: string);
		class function Human(const f: array of float; highlight: SizeInt): string; static;
	end;

	procedure TestContext.Expect(const got, expected: float; const what: string);
	begin
		Expect([got], [expected], what);
	end;

	procedure TestContext.Expect(const got, expected: array of float; const what: string);
	const
		DZeroResolution = 1e-12;
	var
		i: SizeInt;
		sep: string;
	begin
		for i := 0 to High(got) do
			if not SameValue(got[i], expected[i], max(max(abs(got[i]), abs(expected[i])) * DZeroResolution, DZeroResolution)) then
			begin
				if length(got) > 1 then sep := LineEnding else sep := ' ';
				writeln(what, '<', typeName, '>:', sep, 'got', sep, Human(got, i), ',', sep, 'expected', sep, Human(expected, i), LineEnding);
				anythingFailed := true;
				exit;
			end;
	end;

	class function TestContext.Human(const f: array of float; highlight: SizeInt): string;
	var
		i: SizeInt;
		piece: string;
	begin
		result := '';
		if length(f) <> 1 then result += '(';
		for i := 0 to High(f) do
		begin
			WriteStr(piece, f[i]);
			if i > 0 then result += ', ';
			if i = highlight then result += '--> ';
			result += Trim(piece);
			if i = highlight then result += ' <--';
		end;
		if length(f) <> 1 then result += ')';
	end;

	generic procedure TestSums<FloatType>(const typeName: string);
	const
		SrcSrc: array[0 .. 54] of float =
		(
			1,
			2, 2,
			3, 3, 3,
			4, 4, 4, 4,
			5, 5, 5, 5, 5,
			6, 6, 6, 6, 6, 6,
			7, 7, 7, 7, 7, 7, 7,
			8, 8, 8, 8, 8, 8, 8, 8,
			9, 9, 9, 9, 9, 9, 9, 9, 9,
			10, 10, 10, 10, 10, 10, 10, 10, 10, 10
		);
	var
		c: TestContext;
		src: array of FloatType;
		i: SizeInt;
		r: array[0 .. 5] of float;
	begin
		c.typeName := typeName;
		SetLength((@src)^, length(SrcSrc));
		for i := 0 to High(SrcSrc) do src[i] := SrcSrc[i];

		c.Expect(Sum(src), 385, 'Sum');
		c.Expect(SumOfSquares(src), 3025, 'SumOfSquares');

		SumsAndSquares(src, (@r)^[0], (@r)^[1]);
		c.Expect(Slice(r, 2), [385, 3025], 'SumsAndSquares');

		MeanAndStdDev(src, r[0], r[1]);
		c.Expect(Slice(r, 2), [7, 2.4720661623652209829], 'MeanAndStdDev');

		MomentSkewKurtosis(src, r[0], r[1], r[2], r[3], r[4], r[5]);
		c.Expect(Slice(r, 6), [7, 6, -8.4, 85.2, -0.571547606649408222919,  2.3 + 2/30], 'MomentSkewKurtosis');
	end;

begin
	specialize TestSums<single>('single');
	specialize TestSums<double>('double');
{$if sizeof(extended) <> sizeof(double)}
	specialize TestSums<extended>('extended');
{$endif}
	if anythingFailed then halt(1);
	writeln('ok');
end.
