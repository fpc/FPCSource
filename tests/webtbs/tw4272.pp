procedure go(const w: widestring);overload;
begin
writeln('wide: ',w);
end;

procedure go(const w: shortstring);overload;
begin
writeln('short: ',w);
end;

var
  s: ansistring;
begin
  s:='test';
  go(s); //-->compiler can not determine whitch overloaded function to call
end.
