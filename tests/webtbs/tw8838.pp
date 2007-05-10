var
  c,u: char;
  s,t: string[6];
begin
  c := 'x';
  u := UpCase(c);
  s := UpCase(c);
  t := u;
  writeln('c = "',c,'"');
  writeln('u = "',u,'"');
  writeln('s = "',s,'"');
  writeln('t = "',t,'"');
  if (s='') or (t='') then
    halt(1);
end.
