{ %OPT=-O3 }
{$mode objfpc} {$h+} {$typedaddress on}
type
	pBaseType = ^BaseType;
	BaseType = uint32; // can be replaced with an arbitrary-sized array or record

procedure Check(pstart, px: pBaseType; refIx: SizeInt; const desc: string);
var
	ix: SizeInt;
begin
	ix := px - pstart;
	writeln(desc, ' points at element #', ix);
	if ix = refIx then
		writeln('ok')
	else
      begin
		writeln('WRONG, must be #', refIx);
        halt(1);
      end;
	writeln;
end;

var
	x: array[0 .. 19] of BaseType;
	p: pBaseType;

begin
	p := pBaseType(x);
	Check(p, p + 2,         2,  'p + 2');
	Check(p, p + 2 + 3,     5,  'p + 2 + 3');
	Check(p, p + 2 + 3 + 5, 10, 'p + 2 + 3 + 5');

	// These casts don't help.
	Check(p, pBaseType(pBaseType(p + 2) + 3) + 5, 10, 'pBaseType(pBaseType(p + 2) + 3) + 5');

	// These work, but prevent constant folding.
	Check(p, pBaseType(pointer(pBaseType(pointer(p + 2)) + 3)) + 5, 10, 'pBaseType(pointer(pBaseType(pointer(p + 2)) + 3)) + 5');

	Check(p, p + (2 + 3 + 5),   10, 'p + (2 + 3 + 5)');
	Check(p, p + 2 + 3 + 5 + 7, 17, 'p + 2 + 3 + 5 + 7');
end.
