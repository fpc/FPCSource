{$mode objfpc} {$longstrings on}
uses
	StrUtils;

var
	somethingFailed: boolean = false;

procedure TestRPos(const needle, haystack: string; ofs, expect: SizeInt; const what: string = '');
var
	got: SizeInt;
begin
	if length(needle) = 1 then
		got := RPosEx(needle[1], haystack, ofs)
	else
		got := RPosEx(needle, haystack, ofs);
	if got <> expect then
	begin
		writeln('RPosEx(', IfThen(what <> '', what, 'needle = ' + needle + ', haystack = ' + haystack) + ', ofs = ', ofs, ') = ', got, ', expected ', expect);
		somethingFailed := true;
	end;
end;

var
	haystack: string;

begin
	//           1       9      16     23        33 36  40      48     55     62     69     76
	//           v       v      v      v         v  v   v       v      v      v      v      v
	haystack := 'Hitotsu kotoba ari... Esoragoto de ii. Hitotsu karada ari... Tsugou yokute ii.';

	TestRPos('e ii', haystack, High(SizeInt), 74);
	TestRPos('e ii', haystack, 999, 74);
	TestRPos('e ii', haystack, 78, 74);
	TestRPos('e ii', haystack, 77, 74);
	TestRPos('e ii', haystack, 76, 34);
	TestRPos('e ii', haystack, 37, 34);
	TestRPos('e ii', haystack, 36, 0);
	TestRPos('e ii', haystack, -999, 0);
	TestRPos('e ii', haystack, Low(SizeInt), 0);

	TestRPos('i', haystack, High(SizeInt), 0); // Single-character version supposedly behaves like this, unlike (string, string) that clamps to length(haystack).
	TestRPos('i', haystack, 999, 0);
	TestRPos('i', haystack, 78, 77);
	TestRPos('i', haystack, 77, 77);
	TestRPos('i', haystack, 76, 76);
	TestRPos('i', haystack, 75, 57);
	TestRPos('i', haystack, 3, 2);
	TestRPos('i', haystack, 2, 2);
	TestRPos('i', haystack, 1, 0);
	TestRPos('i', haystack, -999, 0);
	TestRPos('i', haystack, Low(SizeInt), 0);

	// Does not actually catch the error because wrong "match" at position 0 (character before first) will return the same 0 as if there was no match.
	TestRPos(PChar(pointer(haystack))[-1] + 'Hitotsu', haystack, 78, 0, 'character-before-first + Hitotsu');

	if somethingFailed then halt(1);
	writeln('ok');
end.
