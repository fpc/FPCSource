type
  days = (mon,tue,wed,thu,fri,sat,sun);
  weekend = sat..sun;

procedure t2(day: weekend);
begin
  if day = sat then
    writeln('ok')
  else writeln('error');
end;

begin
  t2(sat);
end.
