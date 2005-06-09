{$q+}
program IntCalcBug;

var i, j1, j2 : LongInt;

begin
	i := 52000;
	j1 := 1440 - round (i/60);
	j2 := -round (i/60) + 1440;
        if (j1 <> j2) then
          halt(1);
end.
