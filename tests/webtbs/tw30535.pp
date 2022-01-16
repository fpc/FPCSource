{$ifdef unix}
uses
  cthreads;
{$endif}

begin
  if IsMultiThread then
    halt(1);
  Writeln('ok');
end.
