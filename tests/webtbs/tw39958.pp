{ %opt=-O4 -Oodeadstore}
var
	data: array[0 .. 1] of single = (8, 7);
	minData, maxData: single;

begin
	minData := data[0];
	maxData := data[0];
	if data[1] < minData then minData := data[1];
	if data[1] > maxData then maxData := data[1];
	writeln('min = ', minData:0:2, ' (must be 7), max = ', maxData:0:2, ' (must be 8).');
    if minData<>7 then
      halt(1);
    if maxData<>8 then
      halt(1);
    writeln('ok');
end.
