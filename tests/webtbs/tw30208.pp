var R: bitpacked record
    A, B: boolean;
    end;
begin
    R.A := true;
    R.B := false;    
    if not R.B then
      writeln('ok')
    else
      halt(1);
end.
