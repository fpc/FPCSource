{$macro on}
var
	i: int32;

begin
	i := 0;
	{$include iw39912.inc} {$include iw39912.inc} {$include iw39912.inc}

	{$define debugln := writeln}
	debugln('i = ', i, ' (will be printed; must be 3).');
	if i<>3 then
	  halt(1);

	{$define debugln := //}
	debugln('i = ', i, ' (will not be printed).');
	debugln halt(1);
end.
