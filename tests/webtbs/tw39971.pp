{ %opt=-O4 -Oodeadstore -S2 }
function FileSizeFractionalPart(sz: uint64): uint32;
var
	fr: uint32;
begin
	fr := 0;
	while sz > 1000 do
	begin
		fr := sz mod 1024;
		sz := sz div 1024;
	end;
	result := fr;
end;

begin
	writeln(FileSizeFractionalPart(12));
    if FileSizeFractionalPart(12)<>0 then
      halt(1);
end.
