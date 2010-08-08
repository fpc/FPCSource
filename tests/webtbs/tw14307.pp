var
  buf: array[0..3] of widechar;
  s: string;

begin
  buf[0]:='A';
  buf[1]:='B';
  buf[2]:='C';
  buf[3]:=#0;

  s:=Copy(buf, 2, MaxInt);
  if s = 'BC' then
    writeln('OK')
  else begin
    writeln('FAILED: ', s);
    Halt(1);
  end;
end.
