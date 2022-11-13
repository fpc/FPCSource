{ %OPT=-O3 -Oodeadstore }

{$mode objfpc}
function ChooseMinus10: SizeInt;
var
	chosen: SizeInt;
begin
	chosen := -10;
	repeat
		if random(1) = 0 then else break;
		exit(chosen);
	until false;
	result := -20;
end;

begin
  if ChooseMinus10<>-10 then
    halt(1);
end.
