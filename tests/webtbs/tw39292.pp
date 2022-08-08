{$mode objfpc} {$modeswitch typehelpers}
type
	BoolHelper = type helper for boolean
		function ToChar: char;
	end;

	function BoolHelper.ToChar: char;
	begin
		result := pChar('-+')[ord(self)];
	end;

var
	x: uint32;

begin
	writeln((@x = @x).ToChar);
    if (@x = @x).ToChar<>'+' then
      halt(1);
end.
