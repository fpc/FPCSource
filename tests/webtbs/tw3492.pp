{$mode fpc}

resourcestring
  s = 'OK';

var t:ansistring;

begin
  t:=s;
  if t<>'OK' then
    begin
      writeln('Resourcestring error!');
      halt(1);
    end
  else
    writeln(s);
end.
