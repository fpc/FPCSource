{$mode objfpc} {$longstrings on} {$coperators on} {$modeswitch advancedrecords} {$typedaddress on}
uses
	Math, SysUtils;

procedure ReferenceFrexp(X: float; var Mantissa: float; var Exponent: integer);
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

function ReferenceLdexp(x : float;const p : Integer) : float;
begin
	ReferenceLdexp:=x*intpower(2.0,p);
end;

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
			Math.Frexp(srcv, m2, e2);
			writeln('     Math.Frexp(', srcv, '): m = ', m2, ', e = ', e2);
			if (srcv <> Infinity) and (srcv <> NegInfinity) then
			begin
				m := -12345; e := -12345;
				ReferenceFrexp(srcv, m, e);
				writeln('Reference Frexp(', srcv, '): m = ', FloatType(m), ', e = ', e);
				if (IsNaN(m) <> IsNaN(m2)) or not IsNaN(m) and not SameValue(m, m2, 1e-4) or (e <> e2) then
				begin
					writeln('FAIL');
					anythingFailed := true;
				end;
			end else
				writeln('Reference Frexp would freeze, skipped.');

			writeln;
			for eFuzz in ExpFuzz do
			begin
				le := FloatType(ReferenceLdexp(srcv, eFuzz));
				writeln('Reference Ldexp(', srcv, ', ', eFuzz:3, ') = ', FloatType(le));

				le2 := Math.Ldexp(srcv, eFuzz);
				writeln('     Math.Ldexp(', srcv, ', ', eFuzz:3, ') = ', le2);
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
		Insert('(.)', result, 18); Insert(' ', result, 17); Insert(' ', result, 2);
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
				xld := Math.LdExp(x, pow);
				if not IsGoodExtended(xld) then
				begin
					writeln('BAD EXTENDED: LdExp(', x, ', ', pow, ') = ', xld, ' = ', LineEnding, ExtendedRepr(xld), LineEnding);
					anythingFailed := true;
				end;
			end;
	end;
{$endif}

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
end.
