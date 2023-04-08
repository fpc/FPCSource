{$mode objfpc} {$longstrings on} {$coperators on}
uses
	SysUtils;

var
	somethingFailed: boolean = false;

function Repr(const s: string): string;
var
	i: SizeInt;
begin
	result := '';
	for i := 1 to length(s) do
		if (s[i] >= #32) and (s[i] <= #127) then
			result += s[i]
		else
			result += '#' + IntToStr(ord(s[i])) +
				specialize IfThen<string>((i < length(s)) and ((s[i] = #10) or (s[i] = #13) and (pChar(pointer(s))[i] <> #10)), LineEnding, '');
end;

procedure TestAdjustLineBreaks(const src: string; style: TTextLineBreakStyle; expect: string);
var
	got, styleName: string;
begin
	got := AdjustLineBreaks(src, style);
	if got <> expect then
	begin
		WriteStr(styleName, style);
		writeln('AdjustLineBreaks(' + LineEnding +
			LineEnding +
			Repr(src) + ',' + LineEnding +
			LineEnding +
			styleName + ')' + LineEnding +
			LineEnding +
			'=' + LineEnding +
			LineEnding +
			Repr(got) + LineEnding +
			LineEnding +
			'expected' + LineEnding +
			LineEnding +
			Repr(expect) + LineEnding);
		somethingFailed := true;
	end;
end;

const
	D1 = 'Drinking the soup in the Dining Room will poison Viola and cause her to lose HP with each step.';
	D2 = 'The Chef will chop Viola''s hands off if she chooses to lend the chef a hand in the Kitchen.';
	D3 = 'Upon entering the Spider Room, If Viola takes the Butterfly without placing the Butterfly Model in the web, trying to exit the room will make a spider decapitate her.';
	D4 = 'Reading the Book of Death will cause Viola to violently and uncontrollably bleed to death.';
	D5 = 'Entering the Snake Room without feeding the Frog to the Snake will cause the Snake to eat Viola.';
	D6 = 'If Viola visits the Frog Room after the Frog was killed, and Viola reads the note, a black hand will emerge from the black pit and grab her.';
	LEs: array[TTextLineBreakStyle] of string = (#10, #13#10, #13);

var
	style: TTextLineBreakStyle;

begin
	for style in TTextLineBreakStyle do
	begin
		TestAdjustLineBreaks(
			#10#13 + D1 + #13#10 + D2 + #10 + D3 + #13 + D4 + #13#10#10, style,
			LEs[style] + LEs[style] + D1 + LEs[style] + D2 + LEs[style] + D3 + LEs[style] + D4 + LEs[style] + LEs[style]);

		TestAdjustLineBreaks(
			D5 + #13 + D6, style,
			D5 + LEs[style] + D6);
	end;

	if somethingFailed then halt(1);
	writeln('ok');
end.
