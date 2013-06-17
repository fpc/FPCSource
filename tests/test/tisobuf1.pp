{$mode iso}
program test(input, output);

  var
    t : text;

  begin
    assign(t,'tisobuf1.tmp');
    rewrite(t);
    writeln(t,'{Test}');
    close(t);
    reset(t);
    if t^<>'{' then
      halt(1);
    close(t);
    erase(t);
    writeln('ok');
  end.


