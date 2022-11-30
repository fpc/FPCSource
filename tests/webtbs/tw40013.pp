{ %CPU=x86_64 }
{ %opt=-Cpcoreavx2 -O4 }
uses
  cpu;

procedure Verify(const op: string; got, expected: SizeUint);
begin
	write('%10101010 ' + op + ' %111 = %' + BinStr(got, 1 + BsrQWord(got or 1)));
	if got = expected then
		writeln(' - ok')
	else
	  begin
		writeln(' - FAIL, must be %' + BinStr(expected, 1 + BsrQWord(expected or 1)));
		halt(1);
	  end;
end;

var
	b, nbits: SizeUint;


begin
    if BMI2Support then
	  begin
		nbits := 3 + random(0);

		b := %10101010 + random(0);
		b := b or (SizeUint(1) shl nbits - 1);
		Verify('or', b, %10101111);

		b := %10101010 + random(0);
		b := b xor (SizeUint(1) shl nbits - 1);
		Verify('xor', b, %10101101);

		b := %10101010 + random(0);
		b := b and (SizeUint(1) shl nbits - 1);
		Verify('and', b, %10);
	  end;
end.
