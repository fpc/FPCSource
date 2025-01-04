program tw39841;

{$mode objfpc} {$modeswitch typehelpers}
{$coperators on}
{$warn 4055 off - Conversion between ordinals and pointers is not portable}

type
	PointerHelper = type helper for pointer
		function ToUintAndIncr: PtrUint;
	end;

	function PointerHelper.ToUintAndIncr: PtrUint;
	begin
		result := PtrUint(self);
		PtrUint(self) += 1;
	end;

var
	p: pointer;
	pp: PPointer;
	ip, ipThroughPp: PtrUint;
	exitCode: uint32 = 0;

begin
	p := pointer(16);
	pp := @p;
	ip := p.ToUintAndIncr;
	ipThroughPp := pp^.ToUintAndIncr;

	if ip <> 16 then
	begin
		writeln('ip = $', HexStr(ip, 2 * sizeof(PtrUint)), ', expected $', HexStr(16, 2));
		exitCode := 1;
	end;

	if ipThroughPp <> 17 then
	begin
		writeln('ipThroughPp = $', HexStr(ipThroughPp, 2 * sizeof(PtrUint)), ', expected $', HexStr(17, 2));
		exitCode := 2;
	end;

	if p <> pointer(18) then
	begin
		writeln('p = $', HexStr(p), ', expected $', HexStr(18, 2));
		exitCode := 3;
	end;

	if pp <> @p then
	begin
		writeln('pp unexpectedly changed from $', HexStr(@p), ' to $', HexStr(pp));
		exitCode := 4;
	end;

	if exitCode = 0 then writeln('ok');
	if exitCode <> 0 then halt(exitCode);
end.

