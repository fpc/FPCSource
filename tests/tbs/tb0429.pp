
procedure lowercase(c:char);overload;
begin
  writeln('char');
end;
procedure lowercase(c:shortstring);overload;
begin
  writeln('short');
end;
procedure lowercase(c:ansistring);overload;
begin
  writeln('ansi');
end;

var
  w : widestring;
begin
  { this should choosse the shortstring version }
  lowercase(w);
end.

