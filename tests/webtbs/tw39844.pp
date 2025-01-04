program tw39844;
{$mode objfpc} {$typedaddress on} {$modeswitch typehelpers} {$coperators on}
type
	XType = double;

	XTypeHelper = type helper for XType
		procedure Add(x: XType); inline;
	end;

	procedure XTypeHelper.Add(x: XType);
	begin
		self += x;
	end;

var
	x: XType;

begin
	x := 0;
	(@x)^.Add(1);
	if x <> 1 then begin writeln('x = ', x, ', expected 1'); halt(1); end;
	writeln('ok');
end.

