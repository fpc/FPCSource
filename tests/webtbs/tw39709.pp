{$mode delphi}
uses
	SysUtils, StrUtils;

	procedure Test(const s, find, repl, expect: string; flags: TReplaceFlags; algo: TStringReplaceAlgorithm);
	begin
		write((s + ',').PadRight(27), ' ', find.PadRight(5), ' -> ', (repl + IfThen(rfIgnoreCase in flags, ' [I]') + ':').PadRight(12), ' ');
		writeln(StrUtils.StringReplace(s, find, repl, flags, algo));
        if StrUtils.StringReplace(s, find, repl, flags, algo)<>expect then
          halt(1);
	end;

var
	algo: TStringReplaceAlgorithm;

begin
	for algo in TStringReplaceAlgorithm do
	begin
		writeln(algo);
		Test('This works', 'works', 'only works', 'This only works', [rfReplaceAll], algo);
		Test('Hello World', 'hello', 'goodbye', 'goodbye World', [rfReplaceAll, rfIgnoreCase], algo);
		Test('ababab', 'a', 'z', 'zbzbzb', [rfReplaceAll], algo);
		Test('Nani wo nasu tame umareta?', 'a', '-', 'N-ni wo n-su t-me um-ret-?', [rfReplaceAll], algo);
		writeln;
	end;
end.
