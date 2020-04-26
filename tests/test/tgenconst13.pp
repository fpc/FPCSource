{$mode objfpc}
{$modeswitch advancedrecords}
{
	test advanced record constants assigned from generic constant values
}
program tgenconst13;

type
	kNames = set of (Blaise,Pascal);
	kChars = set of char;
type
	generic TBoolean<const U: boolean> = record const value = U; end;
	generic TString<const U: string> = record const value = U; end;
	generic TFloat<const U: single> = record const value = U; end;
	generic TInteger<const U: integer> = record const value = U; end;
	generic TByte<const U: byte> = record const value = U; end;
	generic TChar<const U: char> = record const value = U; end;
	generic TQWord<const U: QWord> = record const value = U; end;
	generic TNames<const U: kNames> = record const value = U; end;
	generic TChars<const U: kChars> = record const value = U; end;

procedure Test(failed: boolean); inline;
begin
	if failed then
		begin
			writeln('failed!');
			halt(-1);
		end;
end;

var
	g0: specialize TBoolean<true>;
	g1: specialize TString<'string'>;
	g2: specialize TFloat<10.5>;
	g3: specialize TInteger<10>;
	g4: specialize TByte<255>;
	g5: specialize TChar<'a'>;
	g6: specialize TQWord<1000000000>;
	g7: specialize TNames<[Blaise,Pascal]>;
	g8: specialize TChars<['a','b']>;
begin
	Test(g0.value <> true);
	Test(g1.value <> 'string');
	Test(g2.value <> 10.5);
	Test(g3.value <> 10);
	Test(g4.value <> 255);
	Test(g5.value <> 'a');
	Test(g6.value <> 1000000000);
	Test(g7.value <> [Blaise,Pascal]);
	Test(g8.value <> ['a','b']);
end.
