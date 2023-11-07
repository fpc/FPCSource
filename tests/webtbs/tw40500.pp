program tw40500;

{$mode objfpc} {$h+} {$coperators+} {$zerobasedstrings+}
uses
	SysUtils;

var
	s: string;
	c: char;

begin
	s := '';
	for c in string('share this to instantly die') do
		if (c >= #32) and (c <= #127) then s += c else s += '#' + IntToStr(ord(c));
	writeln(s);
        if s <> 'share this to instantly die' then
                Halt(1);
end.

