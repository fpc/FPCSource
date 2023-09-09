{ %opt=-O2 }
{$mode objfpc} {$longstrings on}
var
	somethingFailed: boolean = false;

	procedure Expect(got, expected: boolean; const what: string);
	begin
		if got <> expected then
		begin
			writeln(what, ' is ', got, ', expected ', expected, '.');
			somethingFailed := true;
		end;
	end;

var
	s: string;
	sideeffectOk: boolean;

	function GetS: string; noinline;
	begin
		result := 'a';
		sideeffectOk := true;
	end;

{$ifdef windows}
	function SysAllocStringLen(psz: pointer; len: dword): pointer; stdcall; external 'oleaut32.dll' name 'SysAllocStringLen';
var
	ws: widestring;
{$endif}

begin
	s := ''; if random(0) = 1 then s := 'a';
	Expect(length(s) > 0, false, 'length('''') > 0');
	Expect(length(s) <= 0, true, 'length('''') <= 0');
	Expect(length(s) = 0, true, 'length('''') = 0');
	Expect(length(s) <> 0, false, 'length('''') <> 0');
	Expect(0 < length(s), false, '0 < length('''')');
	Expect(0 >= length(s), true, '0 >= length('''')');
	Expect(0 = length(s), true, '0 = length('''')');
	Expect(0 <> length(s), false, '0 <> length('''')');

	if random(0) = 0 then s := 'a';
	Expect(length(s) > 0, true, 'length(''a'') > 0');
	Expect(length(s) <= 0, false, 'length(''a'') <= 0');
	Expect(length(s) = 0, false, 'length(''a'') = 0');
	Expect(length(s) <> 0, true, 'length(''a'') <> 0');
	Expect(0 < length(s), true, '0 < length(''a'')');
	Expect(0 >= length(s), false, '0 >= length(''a'')');
	Expect(0 = length(s), false, '0 = length(''a'')');
	Expect(0 <> length(s), true, '0 <> length(''a'')');

	Expect(length(s) >= 0, true, 'length(''a'') >= 0');
	Expect(0 <= length(s), true, '0 <= length(''a'')');
	Expect(length(s) < 0, false, 'length(''a'') < 0');
	Expect(0 > length(s), false, '0 > length(''a'')');

	Expect(IsConstValue(length(s) > 0), false, 'IsConstValue(length(''a'') > 0)');
	Expect(IsConstValue(length(s) >= 0), true, 'IsConstValue(length(''a'') >= 0)');
	Expect(IsConstValue(length(s) < 0), true, 'IsConstValue(length(''a'') < 0)');
	Expect(IsConstValue(0 < length(s)), false, 'IsConstValue(0 < length(''a''))');
	Expect(IsConstValue(0 <= length(s)), true, 'IsConstValue(0 <= length(''a''))');
	Expect(IsConstValue(0 > length(s)), true, 'IsConstValue(0 > length(''a''))');

	sideeffectOk := false;
	if (Length(GetS) < 0) or not sideeffectOk then
	begin
		writeln('Length(GetS) is either < 0 or, more likely, ignored the side effect of GetS.');
		somethingFailed := true;
	end;

{$ifdef windows}
	ws := '';
	pointer(ws) := SysAllocStringLen(nil, 0);
	Expect(length(ws) = 0, true, 'length(allocated but empty COM widestring) = 0');
{$endif}

	if somethingFailed then halt(1);
end.
