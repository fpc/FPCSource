{$mode objfpc} {$longstrings on} {$coperators on}
uses
	SysUtils, StrUtils;

var
	anythingFailed: boolean = false;

	procedure TestWords(const src: string; const delims: TSysCharSet; const whatdelims: string; const wps: array of SizeInt; const words: array of string);
	var
		i, got, wp: SizeInt;
		sgot, word: string;
	begin
		got := WordCount(src, delims);
		if got <> length(wps) then
		begin
			writeln('Wrong WordCount(', src, ', ', whatdelims, '): got ', got, ', expected ', length(wps), '.', LineEnding);
			anythingFailed := true;
		end;

		for i := -1 to length(wps) do
		begin
			wp := 0;
			word := '';
			if (i >= 0) and (i <= High(wps)) then
			begin
				wp := wps[i];
				word := words[i];
			end;
			got := WordPosition(1 + i, src, delims);
			if got <> wp then
			begin
				writeln('Wrong WordPosition(', 1 + i, ', ', src, ', ', whatdelims, '): got ', got, ', expected ', wp, '.', LineEnding);
				anythingFailed := true;
			end;

			sgot := ExtractWord(1 + i, src, delims);
			if sgot <> word then
			begin
				writeln('Wrong ExtractWord(', 1 + i, ', ', src, ', ', whatdelims, '): got "', sgot, '", expected "', word, '".', LineEnding);
				anythingFailed := true;
			end;

			sgot := ExtractWordPos(1 + i, src, delims, got);
			if sgot <> word then
			begin
				writeln('Wrong ExtractWordPos(', 1 + i, ', ', src, ', ', whatdelims, '): got "', sgot, '", expected "', word, '".', LineEnding);
				anythingFailed := true;
			end;
			if got <> wp then
			begin
				writeln('Wrong ExtractWordPos(', 1 + i, ', ', src, ', ', whatdelims, '): got ', got, ', expected ', wp, '.', LineEnding);
				anythingFailed := true;
			end;

			if (word <> '') and not IsWordPresent(word, src, delims) then
			begin
				writeln('IsWordPresent("', words[i], '", ', src, ', ', whatdelims, ') = false.', LineEnding);
				anythingFailed := true;
			end;
		end;
	end;

	procedure TestStringsReplace(const s: string; const find, repl: array of string; flags: TReplaceFlags; const expect: string);
	var
		got, what: string;
		i: SizeInt;
	begin
		got := StringsReplace(s, find, repl, flags);
		if got <> expect then
		begin
			what := 'StringsReplace(' + s + ', [';
			for i := 0 to High(find) do
				what += IfThen(i > 0, ', ') + find[i];
			what += '], [';
			for i := 0 to High(repl) do
				what += IfThen(i > 0, ', ') + find[i];
			what += '], [';
			if rfReplaceAll in flags then what += IfThen(what[length(what)] <> '[', ', ') + 'rfReplaceAll';
			if rfIgnoreCase in flags then what += IfThen(what[length(what)] <> '[', ', ') + 'rfIgnoreCase';
			what += '])';
			writeln(what + ' =', LineEnding, got, LineEnding, 'expected:', LineEnding, expect, LineEnding);
			anythingFailed := true;
		end;
	end;

	procedure TestExtractDelimited(const s: string; const delims: TSysCharSet; const expect: array of string);
	var
		i: SizeInt;
		got, nowExp: string;
	begin
		for i := -1 to length(expect) do
		begin
			nowExp := '';
			if (i >= 0) and (i <= High(expect)) then nowExp := expect[i];
			got := ExtractDelimited(1 + i, s, delims);
			if got <> nowExp then
			begin
				writeln('ExtractDelimited(', 1 + i, ', ', s, ') = ', got, ', expected ', nowExp, '.', LineEnding);
				anythingFailed := true;
				break;
			end;
		end;
	end;

	procedure TestFindPart(const wilds, s: string; expect: SizeInt);
	var
		got: SizeInt;
	begin
		got := FindPart(wilds, s);
		if got <> expect then
		begin
			writeln('FindPart', wilds, ', ', s, ') = ', got, ', expected ', expect, '.', LineEnding);
			anythingFailed := true;
		end;
	end;

	procedure TestDelChars(const s: string; const chars: TSysCharSet; const whatchars, expect: string);
	var
		got: string;
	begin
		if (chars = []) and (length(whatchars) = 1) then
			got := DelChars(s, whatchars[1])
		else
			got := DelChars(s, chars);
		if got <> expect then
		begin
			writeln('DelChars(', s, ', ', whatchars, ') = "', got, '", expected "', expect, '".', LineEnding);
			anythingFailed := true;
		end;
	end;

	procedure TestDelSpace1(const s, expect: string);
	var
		got: string;
	begin
		got := DelSpace1(s);
		if got <> expect then
		begin
			writeln('DelSpace1(', s, ') = "', got, '", expected "', expect, '.', LineEnding);
			anythingFailed := true;
		end;
	end;

	procedure TestNPos(const sub, s: string; const expect: array of SizeInt);
	var
		i, got, nowExp: SizeInt;
	begin
		for i := -1 to length(expect) do
		begin
			if (i >= 0) and (i <= High(expect)) then nowExp := expect[i] else nowExp := 0;
			got := NPos(sub, s, 1 + i);
			if got <> nowExp then
			begin
				writeln('NPos(', sub, ', ', s, ', ', 1 + i, ') = ', got, ', expected ', nowExp, '.', LineEnding);
				anythingFailed := true;
				break;
			end;
		end;
	end;

	procedure TestRemoveLeadingTrailingPadChars(const s: string; const c: TSysCharSet; const whatc: string; const expect: array of string);
	const
		FuncName: array[0 .. 2] of string = ('RemoveLeadingChars', 'RemoveTrailingChars', 'RemovePadChars');
	var
		got, whats: string;
		u: unicodestring;
		unicode: boolean;
		iFunc: SizeInt;
	begin
		for unicode in boolean do
			for iFunc := 0 to 2 do
			begin
				whats := s;
				if unicode then whats := 'unicodestring(' + whats + ')';

				got := s; UniqueString(got);
				if unicode then
				begin
					u := unicodestring(got);
					case iFunc of
						0: RemoveLeadingChars(u, c);
						1: RemoveTrailingChars(u, c);
						2: RemovePadChars(u, c);
					end;
					got := string(u);
				end else
					case iFunc of
						0: RemoveLeadingChars(got, c);
						1: RemoveTrailingChars(got, c);
						2: RemovePadChars(got, c);
					end;
				if got <> expect[iFunc] then
				begin
					writeln(FuncName[iFunc], '(', whats, ', ', whatc, ') = "', got, '", expected "', expect[iFunc], '".', LineEnding);
					anythingFailed := true;
				end;
			end;
	end;

begin
	TestWords(' w1_wo2_word3 ', ['_'], '[_]', [1, 5, 9], [' w1', 'wo2', 'word3 ']);
	TestWords(' w1_wo2 _word3 ', [' ', '_'], '[space, _]', [2, 5, 10], ['w1', 'wo2', 'word3']);
	TestStringsReplace('aaa', ['a', 'a', 'a'], ['b', 'c', 'd'], [rfReplaceAll], 'bbb');
	TestStringsReplace('aaa', ['a', 'a', 'a'], ['b', 'c', 'd'], [], 'baa');
	TestStringsReplace('abcdefgh', ['ab', 'd', 'gh'], ['R1', 'Rr2', 'Rrr3'], [rfReplaceAll], 'R1cRr2efRrr3');
	TestStringsReplace('sabcdefghe', ['ab', 'd', 'gh'], ['R1', 'Rr2', 'Rrr3'], [rfReplaceAll], 'sR1cRr2efRrr3e');
	TestStringsReplace('sAbcDefgHe', ['aB', 'd', 'Gh'], ['R1', 'Rr2', 'Rrr3'], [rfReplaceAll], 'sAbcDefgHe');
	TestStringsReplace('sAbcDefgHe', ['aB', 'd', 'Gh'], ['R1', 'Rr2', 'Rrr3'], [rfReplaceAll, rfIgnoreCase], 'sR1cRr2efRrr3e');
	TestStringsReplace('sabcdefghe', ['ab', 'd', 'gh'], ['R1', 'Rr2', 'Rrr3'], [], 'sR1cdefghe');
	TestStringsReplace('ababab', ['a', 'b'], ['b', 'a'], [rfReplaceAll], 'bababa');
	TestExtractDelimited(',,,a,bc,def,,', [','], ['', '', '', 'a', 'bc', 'def', '', '']);
	TestExtractDelimited('a,bc,,def,gh', [','], ['a', 'bc', '', 'def', 'gh']);
	TestFindPart('a??a', 'bbbaabaaaa', 4);
	TestDelChars('aabcdaaabcdaaaa', [], 'a', 'bcdbcd');
	TestDelChars('aabcdaaabcdaaaa', ['a'], '[a]', 'bcdbcd');
	TestDelChars('bcdaabcdaaabcd', [], 'a', 'bcdbcdbcd');
	TestDelChars('bcdaabcdaaabcd', ['a'], '[a]', 'bcdbcdbcd');
	TestDelSpace1('  a   bcd   efg  ', ' a bcd efg ');
	TestDelSpace1('a  bcd   efg', 'a bcd efg');
	TestNPos('aa', 'aaabaaabbaaa', [1, 2, 5, 6, 10, 11]);
	TestRemoveLeadingTrailingPadChars('abcde_aj_fghij', ['a', 'b', 'h', 'i', 'j'], '[a, b, h, i, j]', ['cde_aj_fghij', 'abcde_aj_fg', 'cde_aj_fg']);
	TestRemoveLeadingTrailingPadChars('abcde_aj_fghij', ['a', 'j'],  '[a, j]', ['bcde_aj_fghij', 'abcde_aj_fghi', 'bcde_aj_fghi']);
	if not anythingFailed then writeln('ok');
	if anythingFailed then halt(1);
end.

