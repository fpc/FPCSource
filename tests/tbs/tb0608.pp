const
    c   = {$IF Declared(o) And (o<>Integer(0))}Succ{$IFEND}(False);

  begin
    if c then
      halt(1);
    writeln('ok');
  end.

