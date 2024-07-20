var
	e: (ab, abcd);
	code: int32;
begin
	val('ab'#4'd', e, code);
	if code = 0 then writeln('matches ', e) else writeln('matches nothing, code ', code);
    if code <> 3 then
      halt(1);
end.
