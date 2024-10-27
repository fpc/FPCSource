var
	anythingWrong: boolean = false;

	procedure Test(size: SizeUint);
	var
		p: pointer;
		memSizeResult, freeMemResult: SizeUint;
	begin
		p := GetMem(size);
		memSizeResult := MemSize(p);
		freeMemResult := FreeMem(p);
		if memSizeResult <> freeMemResult then
		begin
			writeln('GetMem(', size, '): MemSize returned ', memSizeResult, ', FreeMem returned ', freeMemResult, '.');
			anythingWrong := true;
		end;
	end;

begin
	Test(128);
	Test(16384);
	if anythingWrong then halt(1);
end.
