{$codepage utf8}
{$mode objfpc} {$h+} {$coperators on}
uses
	SysUtils, DateUtils;

const
	Sample = widestring('Examples' + LineEnding +
		'Appointments: Аппойнтменты (Appojntmenty)[2]' + LineEnding +
		'Iced Coffee: Аисд кофе (Aisd kofe)[2]' + LineEnding +
		'Would you like that sliced or in one piece?: Вам наслайсовать или писом? (Vam naslajsovatj ili pisom?)[4]' + LineEnding +
		'Driving upstate on the highways: Драйвуем в апстейт по хайвеям (Drajvujem v apstejt po hajwejam)[5]' + LineEnding +
		'Sliced Cheese: Слайсающий чиз (Slajsajuçij čiz)[5]' + LineEnding +
		'To merge branches: Смержить бранчи (Smeržitj branči)[2]' + LineEnding +
		'To manage: Сменеджить (Smenedžitj)' + LineEnding +
		'I sent you message with attached request: Я засендил тебе месседж с приаттаченым реквестом (Ya zasendil tebe messedž s priattachenim rekvestom)');
	Iterations = 880000;

type
	CodepointFlag = (Letter, Vowel, Consonant, Digit, Whitespace, Newline, Punctuation, Cyrillic, Latin, Diacritic);
	CodepointClassification = set of CodepointFlag;

	function Classify_Case(const cp: widechar): CodepointClassification;
	begin
		case cp of
			' ', #$9 {tab}, #$a0 {nbsp}, '　' {ideographic space}: result := [Whitespace];
			#$a {lf}, #$d {cr}: result := [Whitespace, Newline];
			'0' .. '9': result := [Digit];
			'a', 'e', 'i', 'o', 'u', 'y', 'A', 'E', 'I', 'O', 'U', 'Y': result := [Letter, Latin, Vowel];
			'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z',
			'B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Z': result := [Letter, Latin, Consonant];
			'.', '!', ',', '?', ':', ';', '-', '—': result := [Punctuation];
			'А', 'Е', 'И', 'О', 'У', 'Ы', 'Э', 'Ю', 'Я', 'а', 'е', 'и', 'о', 'у', 'ы', 'э', 'ю', 'я', 'Ё', 'ё': result := [Letter, Cyrillic, Vowel];
			'б', 'в', 'г', 'д', 'ж', 'з', 'й', 'к', 'л', 'м', 'н', 'п', 'р', 'с', 'т', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',
			'Б', 'В', 'Г', 'Д', 'Ж', 'З', 'Й', 'К', 'Л', 'М', 'Н', 'П', 'Р', 'С', 'Т', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ': result := [Letter, Cyrillic, Consonant];
			'ъ', 'ь', 'Ъ', 'Ь': result := [Letter, Cyrillic];
			#$300 {first diacritic} .. #$36f {last diacritic}: result := [Diacritic];
			else result := [];
		end;
	end;

	// I have built this tree from sorted starts of subsequences:
	//  (1)       9   tab
	//  (2)       A   lf
	//  (3)       D   cr
	//  (4)      20   space
	//  (5)      21   !
	//  (6)      2C   ,
	//  (7)      2D   -
	//  (8)      2E   .
	//  (9)   30–39   0-9
	// (10)      3A   :
	// (11)      3B   ;
	// (12)      3F   ?
	// (13)   41-5a   A-Z
	// (14)   61-7a   a-z
	// (15)      A0   nbsp
	// (16) 300–36f   diacritics
	// (17)     401   Ё
	// (18) 410–44f   А-я
	// (19)     451   ё
	// (20)    2014   — (em dash)
	// (21)    3000   ideographic space
	function Classify_Tree(const cp: widechar): CodepointClassification;
	begin
		if cp < #$3F then // (1) ~ (11)
			if cp < #$2D then // (1) ~ (6) — small set without subsequences, resort to predictor-friendly :) case..of
				case cp of
					' ', #$9 {tab}: exit([Whitespace]);
					#$a {lf}, #$d {cr}: exit([Whitespace, Newline]);
					'!', ',': exit([Punctuation]);
				end
			else // (7) ~ (11)
				if cp < #$3A then // (7) ~ (9)
					if cp < #$30 then // (7) ~ (8)
						case cp of
							'-', '.': exit([Punctuation]);
						end
					else // (9)
						case cp of
							'0' .. '9': exit([Digit]);
						end
				else // (10) ~ (11)
					case cp of
						':', ';': exit([Punctuation]);
					end
		else // (12) ~ (21)
			if cp < #$401 then // (12) ~ (16)
				if cp < #$A0 then // (12) ~ (14)
					if cp < #$61 then // (12) ~ (13)
						if cp < #$41 then // (12)
							case cp of
								'?': exit([Punctuation]);
							end
						else // (13)
							case cp of
								'A', 'E', 'I', 'O', 'U', 'Y': exit([Letter, Latin, Vowel]);
								'B', 'C', 'D', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Z': exit([Letter, Latin, Consonant]);
							end
					else //(14)
						case cp of
							'a', 'e', 'i', 'o', 'u', 'y': exit([Letter, Latin, Vowel]);
							'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z': exit([Letter, Latin, Consonant]);
						end
				else // (15) ~ (16)
					if cp < #$300 then // (15)
						case cp of
							#$a0 {nbsp}: exit([Whitespace]);
						end
					else // (16)
						case cp of
							#$300 {first diacritic} .. #$36f {last diacritic}: exit([Diacritic]);
						end
			else // (17) ~ (21)
				if cp < #$2014 then // (17) ~ (19)
					if cp < #$451 then // (17) ~ (18)
						if cp < #$410 then // (17)
							case cp of
								'Ё': exit([Letter, Cyrillic, Vowel]);
							end
						else // (18)
							case cp of
								'А', 'Е', 'И', 'О', 'У', 'Ы', 'Э', 'Ю', 'Я', 'а', 'е', 'и', 'о', 'у', 'ы', 'э', 'ю', 'я': exit([Letter, Cyrillic, Vowel]);
								'б', 'в', 'г', 'д', 'ж', 'з', 'й', 'к', 'л', 'м', 'н', 'п', 'р', 'с', 'т', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',
								'Б', 'В', 'Г', 'Д', 'Ж', 'З', 'Й', 'К', 'Л', 'М', 'Н', 'П', 'Р', 'С', 'Т', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ': exit([Letter, Cyrillic, Consonant]);
								'ъ', 'ь', 'Ъ', 'Ь': exit([Letter, Cyrillic]);
							end
					else // (19)
						case cp of
							'ё': exit([Letter, Cyrillic, Vowel]);
						end
				else // (20) ~ (21)
					case cp of
						'—': exit([Punctuation]);
						'　': exit([Whitespace]);
					end;

		// 'else'
		result := [];
	end;

	procedure Test;
		function ToString(const cls: CodepointClassification): string;
		var
			f: CodepointFlag;
		begin
			result := '';
			for f in CodepointFlag do
				if f in cls then
				begin
					if result <> '' then result += ', ';
					WriteStr(result, f);
				end;
			result := '[' + result + ']';
		end;
	var
		cp: widechar;
	begin
		write('Sanity check... ');
		for cp := #0 to #$3001 do
			if Classify_Case(cp) <> Classify_Tree(cp) then
			begin
				writeln(
					'Failed. Classify_Case isn''t equivalent to Classify_Tree.', LineEnding,
					'Symbol code: $', HexStr(cardinal(cp), bitsizeof(widechar) div 4), LineEnding,
					'Classify_Case: ', ToString(Classify_Case(cp)), LineEnding,
					'Classify_Tree: ', ToString(Classify_Tree(cp)), LineEnding);
				readln;
				halt(1);
			end;

		writeln('Passed. Classify_Case and Classify_Tree are equivalent.',
			LineEnding);
	end;

	procedure Benchmark;
	var
		caseTime, treeTime: TDateTime;
		vow, lat: cardinal;
		iteration: cardinal;
		i: SizeInt;

		procedure ResetWork(out vow, lat: cardinal);
		begin
			vow := 0; lat := 0;
		end;

		procedure Note(const cls: CodepointClassification);
		begin
			if Vowel in cls then inc(vow);
			if Latin in cls then inc(lat);
		end;

	begin
		write('Benchmarking Classify_Tree... ');
		ResetWork(vow, lat);
		treeTime := Now;
		for iteration := 1 to Iterations do
		begin
			for i := 1 to length(Sample) do
				Note(Classify_Tree(Sample[i]));
			if iteration < Iterations then ResetWork(vow, lat);
		end;
		treeTime := SecondSpan(Now, treeTime);
		writeln(treeTime:0:1, ' s (', treeTime/Iterations*1e6:0:1, ' mcs/it), vow. ', vow, ', lat. ', lat, '.');

		write('Benchmarking Classify_Case... ');
		ResetWork(vow, lat);
		caseTime := Now;
		for iteration := 1 to Iterations do
		begin
			for i := 1 to length(Sample) do
				Note(Classify_Case(Sample[i]));
			if iteration < Iterations then ResetWork(vow, lat);
		end;
		caseTime := SecondSpan(Now, caseTime);
		writeln(caseTime:0:1, ' s (', caseTime/Iterations*1e6:0:1, ' mcs/it), vow. ', vow, ', lat. ', lat, '.');

		if (treeTime < 0.5) or (caseTime < 0.5) then
			writeln('Could not deduce anything useful.')
		else
			if (treeTime <= 1.05*caseTime) and (caseTime <= 1.05*treeTime) then
				writeln('There was no observable difference.')
			else
				if treeTime > caseTime then
					writeln('Classify_Case was faster by ', (treeTime / caseTime - 1)*100:0:0, '% (', treeTime/caseTime:0:1, 'x).')
				else
					writeln('Classify_Tree was faster by ', (caseTime / treeTime - 1)*100:0:0, '% (', caseTime/treeTime:0:1, 'x).')
	end;

begin
	Test;
//        Benchmark;
end.
