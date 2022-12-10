{$mode objfpc} {$longstrings on} {$coperators on} {$modeswitch advancedrecords} {$typedaddress on}
uses
	Math, SysUtils;

procedure OldFrexp(X: float; var Mantissa: float; var Exponent: integer);
begin
	Exponent:=0;
	if (X<>0) then
		if (abs(X)<0.5) then
			repeat
				X:=X*2;
				Dec(Exponent);
			until (abs(X)>=0.5)
		else
			while (abs(X)>=1) do
				begin
				X:=X/2;
				Inc(Exponent);
				end;
	Mantissa:=X;
end;

function OldLdexp(x : float;const p : Integer) : float;
begin
	OldLdexp:=x*intpower(2.0,p);
end;

// Correction for "rounding to nearest, ties to even".
// RoundToNearestTieToEven(QWE.RTYUIOP) = QWE + TieToEven(ER, TYUIOP <> 0).
function TieToEven(AB: cardinal; somethingAfter: boolean): cardinal;
begin
	result := AB and 1;
	if (result <> 0) and not somethingAfter then
		result := AB shr 1;
end;

procedure NewFrexp(X: single; out Mantissa: single; out Exponent: integer);
var
	M: uint32;
	E, ExtraE: int32;
begin
	Mantissa := X;
	E := TSingleRec(X).Exp;
	if (E > 0) and (E < 2 * TSingleRec.Bias + 1) then
	begin
		// Normal.
		TSingleRec(Mantissa).Exp := TSingleRec.Bias - 1;
		Exponent := E - (TSingleRec.Bias - 1);
		exit;
	end;
	if E = 0 then
	begin
		M := TSingleRec(X).Frac;
		if M <> 0 then
		begin
			// Subnormal.
			ExtraE := 23 - BsrDWord(M);
			TSingleRec(Mantissa).Frac := M shl ExtraE; // "and (1 shl 23 - 1)" required to remove starting 1, but .SetFrac already does it.
			TSingleRec(Mantissa).Exp	:= TSingleRec.Bias - 1;
			Exponent := -TSingleRec.Bias + 2 - ExtraE;
			exit;
		end;
	end;
	// ±0, ±Inf, NaN.
	Exponent := 0;
end;

function NewLdexp(X: single; p: integer): single;
var
	M, E: uint32;
	xp, sh: integer;
begin
	E := TSingleRec(X).Exp;
	if (E = 0) and (TSingleRec(X).Frac = 0) or (E = 2 * TSingleRec.Bias + 1) then
		// ±0, ±Inf, NaN.
		exit(X);

	NewFrexp(X, result, xp);
	inc(xp, p);
	if (xp >= -TSingleRec.Bias + 2) and (xp <= TSingleRec.Bias + 1) then
		// Normalized.
		TSingleRec(result).Exp := xp + (TSingleRec.Bias - 1)
	else if xp > TSingleRec.Bias + 1 then
	begin
		// Overflow.
		TSingleRec(result).Exp := 2 * TSingleRec.Bias + 1;
		TSingleRec(result).Frac := 0;
	end else
	begin
		TSingleRec(result).Exp := 0;
		if xp >= -TSingleRec.Bias + 2 - 23 then
		begin
			// Denormalized.
			M := TSingleRec(result).Frac or uint32(1) shl 23;
			sh := -TSingleRec.Bias + 1 - xp;
			TSingleRec(result).Frac := M shr (sh + 1) + TieToEven(M shr sh and 3, M and (uint32(1) shl sh - 1) <> 0);
		end else
			// Underflow.
			TSingleRec(result).Frac := 0;
	end;
end;

procedure NewFrexp(X: double; out Mantissa: double; out Exponent: integer);
var
	M: uint64;
	E, ExtraE: int32;
begin
	Mantissa := X;
	E := TDoubleRec(X).Exp;
	if (E > 0) and (E < 2 * TDoubleRec.Bias + 1) then
	begin
		// Normal.
		TDoubleRec(Mantissa).Exp := TDoubleRec.Bias - 1;
		Exponent := E - (TDoubleRec.Bias - 1);
		exit;
	end;
	if E = 0 then
	begin
		M := TDoubleRec(X).Frac;
		if M <> 0 then
		begin
			// Subnormal.
			ExtraE := 52 - BsrQWord(M);
			TDoubleRec(Mantissa).Frac := M shl ExtraE; // "and (1 shl 52 - 1)" required to remove starting 1, but .SetFrac already does it.
			TDoubleRec(Mantissa).Exp	:= TDoubleRec.Bias - 1;
			Exponent := -TDoubleRec.Bias + 2 - ExtraE;
			exit;
		end;
	end;
	// ±0, ±Inf, NaN.
	Exponent := 0;
end;

function NewLdexp(X: double; p: integer): double;
var
	M: uint64;
	E: uint32;
	xp, sh: integer;
begin
	E := TDoubleRec(X).Exp;
	if (E = 0) and (TDoubleRec(X).Frac = 0) or (E = 2 * TDoubleRec.Bias + 1) then
		// ±0, ±Inf, NaN.
		exit(X);

	NewFrexp(X, result, xp);
	inc(xp, p);
	if (xp >= -TDoubleRec.Bias + 2) and (xp <= TDoubleRec.Bias + 1) then
		// Normalized.
		TDoubleRec(result).Exp := xp + (TDoubleRec.Bias - 1)
	else if xp > TDoubleRec.Bias + 1 then
	begin
		// Overflow.
		TDoubleRec(result).Exp := 2 * TDoubleRec.Bias + 1;
		TDoubleRec(result).Frac := 0;
	end else
	begin
		TDoubleRec(result).Exp := 0;
		if xp >= -TDoubleRec.Bias + 2 - 52 then
		begin
			// Denormalized.
			M := TDoubleRec(result).Frac or uint64(1) shl 52;
			sh := -TSingleRec.Bias + 1 - xp;
			TDoubleRec(result).Frac := M shr (sh + 1) + TieToEven(M shr sh and 3, M and (uint64(1) shl sh - 1) <> 0);
		end else
			// Underflow.
			TDoubleRec(result).Frac := 0;
	end;
end;

{$if sizeof(extended) <> sizeof(double)}
procedure NewFrexp(X: extended; out Mantissa: extended; out Exponent: integer);
var
	M: uint64;
	E, ExtraE: int32;
begin
	Mantissa := X;
	E := TExtended80Rec(X).Exp;
	if (E > 0) and (E < 2 * TExtended80Rec.Bias + 1) then
	begin
		// Normal.
		TExtended80Rec(Mantissa).Exp := TExtended80Rec.Bias - 1;
		Exponent := E - (TExtended80Rec.Bias - 1);
		exit;
	end;
	if E = 0 then
	begin
		M := TExtended80Rec(X).Frac;
		if M <> 0 then
		begin
			// Subnormal. Extended has explicit starting 1.
			ExtraE := 63 - BsrQWord(M);
			TExtended80Rec(Mantissa).Frac := M shl ExtraE;
			TExtended80Rec(Mantissa).Exp	:= TExtended80Rec.Bias - 1;
			Exponent := -TExtended80Rec.Bias + 2 - ExtraE;
			exit;
		end;
	end;
	// ±0, ±Inf, NaN.
	Exponent := 0;
end;

function NewLdexp(X: extended; p: integer): extended;
var
	M: uint64;
	E: uint32;
	xp, sh: integer;
begin
	E := TExtended80Rec(X).Exp;
	if (E = 0) and (TExtended80Rec(X).Frac = 0) or (E = 2 * TExtended80Rec.Bias + 1) then
		// ±0, ±Inf, NaN.
		exit(X);

	NewFrexp(X, result, xp);
	inc(xp, p);
	if (xp >= -TExtended80Rec.Bias + 2) and (xp <= TExtended80Rec.Bias + 1) then
		// Normalized.
		TExtended80Rec(result).Exp := xp + (TExtended80Rec.Bias - 1)
	else if xp > TExtended80Rec.Bias + 1 then
	begin
		// Overflow.
		TExtended80Rec(result).Exp := 2 * TExtended80Rec.Bias + 1;
		TExtended80Rec(result).Frac := uint64(1) shl 63;
	end
	else if xp >= -TExtended80Rec.Bias + 2 - 63 then
	begin
		// Denormalized... usually.
		// Mantissa of subnormal 'extended' (Exp = 0) must always start with 0.
		// If the calculated mantissa starts with 1, extended instead becomes normalized with Exp = 1.
		M := TExtended80Rec(result).Frac;
		sh := -TExtended80Rec.Bias + 1 - xp;
		M := M shr (sh + 1) + TieToEven(M shr sh and 3, M and (uint64(1) shl sh - 1) <> 0);
		TExtended80Rec(result).Exp := M shr 63;
		TExtended80Rec(result).Frac := M;
	end else
	begin
		// Underflow.
		TExtended80Rec(result).Exp := 0;
		TExtended80Rec(result).Frac := 0;
	end;
end;
{$endif}

var
	anythingFailed: boolean = false;

type
	generic TestFor<FloatType> = record
		class procedure Perform(const name: string; const minSubnorm, maxSubnorm, minNorm, maxNorm: FloatType); static;
	end;

	class procedure TestFor.Perform(const name: string; const minSubnorm, maxSubnorm, minNorm, maxNorm: FloatType);
	const
		ExpFuzz: array[0 .. 4] of integer = (-10, -1, 0, 1, 10);
	var
		m, le: float;
		srcv, m2, le2: FloatType;
		e, e2, eFuzz: integer;
	begin
		writeln(name);
		for srcv in [
			NaN, Infinity, NegInfinity,
			minSubnorm, 2 * minSubnorm, 3 * minSubnorm, 5 * minSubnorm,
			maxSubnorm / 5, maxSubnorm / 1.5, maxSubnorm,
			-Pi, Exp(6),
			minNorm, 3 * minNorm,
			maxNorm / 3, maxNorm] do
		begin
			m2 := -12345; e2 := -12345;
			NewFrexp(srcv, m2, e2);
			writeln('     New Frexp(', srcv, '): m = ', m2, ', e = ', e2);
			if (srcv <> Infinity) and (srcv <> NegInfinity) then
			begin
				m := -12345; e := -12345;
				OldFrexp(srcv, m, e);
				writeln('Old Math.Frexp(', srcv, '): m = ', FloatType(m), ', e = ', e);
				if (IsNaN(m) <> IsNaN(m2)) or not IsNaN(m) and not SameValue(m, m2, 1e-4) or (e <> e2) then
				begin
					writeln('FAIL');
					anythingFailed := true;
				end;
			end else
				writeln('Old Math.Frexp would freeze, skipped.');

			writeln;
			for eFuzz in ExpFuzz do
			begin
				le := FloatType(OldLdexp(srcv, eFuzz));
				writeln('Old Math.Ldexp(', srcv, ', ', eFuzz:3, ') = ', FloatType(le));

				le2 := NewLdexp(srcv, eFuzz);
				writeln('     New Ldexp(', srcv, ', ', eFuzz:3, ') = ', le2);
				if (IsNaN(le) <> IsNaN(le2)) or not IsNaN(le) and (le <> le2) then
				begin
					writeln('FAIL');
					anythingFailed := true;
				end;
			end;
			writeln;
		end;
		writeln;
	end;

{$if sizeof(extended) <> sizeof(double)}
	function IsGoodExtended(const x: extended): boolean;
	begin
		result := (TExtended80Rec(x).Exp = 0) = (TExtended80Rec(x).Frac shr 63 = 0);
	end;

	function ExtendedRepr(const x: extended): string;
	begin
		result := BinStr(TExtended80Rec(x)._Exp, 16) + BinStr(TExtended80Rec(x).Frac, 64);
		Insert(' ', result, 18); Insert(' ', result, 17); Insert(' ', result, 2);
	end;

	procedure TryTrickLdExpIntoReturningMalformedExtended;
	const
		Srcs: array[0 .. 0] of extended = (6.72420628622418701216e-4932);
	var
		x, xld: extended;
		pow: int32;
	begin
		for x in Srcs do
			for pow := -3 to 3 do
			begin
				xld := NewLdExp(x, pow);
				if not IsGoodExtended(xld) then
				begin
					writeln('BAD EXTENDED: LdExp(', x, ', ', pow, ') = ', xld, ' = ', LineEnding, ExtendedRepr(xld), LineEnding);
					anythingFailed := true;
				end;
			end;
	end;
{$endif}

type
	generic BenchmarkFor<FloatType> = record
		class procedure Perform(const name: string); static;
	end;

	class procedure BenchmarkFor.Perform(const name: string); static;
	type
		ProcEnum = (DoOldFrexp, DoNewFrexp, DoOldLdexp, DoNewLdexp);
	const
		ProcName: array[ProcEnum] of string = ('Old Math.Frexp', '     New Frexp', 'Old Math.Ldexp', '     New Ldexp');
		Powers: array[0 .. 5] of cardinal = (0, 1, 10, 30, 300, 3000);
	var
		start: TDateTime;
		time: double;
		reps, amplify, pow: cardinal;
		proc: ProcEnum;
		om: float;
		x, m: FloatType;
		oe, e: integer;
	begin
		writeln(name);
		x := 1;
		for pow in Powers do
		begin
			x := Power(10, pow);
			if IsInfinite(x) then break;
			for proc in ProcEnum do
			begin
				start := Now;
				reps := 0;
				repeat
					reps += 1;
					case proc of
						DoOldFrexp: for amplify := 1 to 100 do OldFrexp(x, (@om)^, (@oe)^);
						DoNewFrexp: for amplify := 1 to 100 do NewFrexp(x, m, e);
						DoOldLdexp: for amplify := 1 to 100 do OldLdexp((@om)^, (@oe)^);
						DoNewLdexp: for amplify := 1 to 100 do NewLdexp((@m)^, (@e)^);
					end;
					time := (Now - start) * SecsPerDay;
				until not (time < 0.3);
				writeln(ProcName[proc], '(', x, '): ', time / reps / 100 * 1e9:0:1, ' ns/call');
			end;
			writeln;
		end;
	end;

begin
	SetExceptionMask([Low(TFPUException) .. High(TFPUException)]);

{$if sizeof(extended) <> sizeof(double)}
	TryTrickLdExpIntoReturningMalformedExtended;
{$endif}

	specialize TestFor<single>.Perform('single', 1.4012984643e-45, 1.1754942107e-38, MinSingle, MaxSingle);
	specialize TestFor<double>.Perform('double', 4.9406564584124654e-324, 2.2250738585072009e-308, MinDouble, MaxDouble);
{$if sizeof(extended) <> sizeof(double)}
	specialize TestFor<extended>.Perform('extended',
		3.64519953188247460253e-4951, 3.36210314311209350590e-4932, {MinExtended} 3.36210314311209350626e-4932, {MaxExtended} 1.18973149535723176502e+4932);
{$endif}
	if anythingFailed then
	begin
		writeln('Something failed, see above.');
		halt(1);
	end else
		writeln('Everything OK.');

	writeln;
	specialize BenchmarkFor<single>.Perform('single');
	specialize BenchmarkFor<double>.Perform('double');
{$if sizeof(extended) <> sizeof(double)}
	specialize BenchmarkFor<extended>.Perform('extended');
{$endif}
end.
