var
  i : longint;

begin
  i:=5;
  i:=i*$80010;
  if i<>2621520 then
    halt(1);

  i:=5;
  i:=i*$18000010;
  if i<>2013266000 then
    halt(1);

  i:=5;
  i:=i*$18ffffef;
  if i<>2097151915 then
    halt(1);

  i:=5;
  i:=i*$7ffef;
  if i<>2621355 then
    halt(1);

  i:=5;
  i:=i*$6fffffcf;
  if i<>805306123 then
    halt(1);

  i:=5;
  i:=i*10;
  i:=i*62;
  i:=i*-10;
  i:=i*-62;
  i:=i*87;
  if i<>167214000 then
    halt(1);
  writeln('ok');
end.
