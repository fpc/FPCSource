program try;

procedure check(s: string);
var
  i: longint;
begin
  delete(s,pos(' ',s),1);
  delete(s,pos('E',s),1);
  delete(s,pos('.',s),1);
  delete(s,pos('+',s),1);
  for i := 1 to length(s) do
    if (s[i]<>'0') then
      halt(1);
end;

var
  foo: single;
  d: double;
  e: extended;
  s: string;
begin
   foo:=0.0;
   d:=0.0;
   e:=0.0;
   str(foo,s);
   writeln(s);
   check(s);
   str(d,s);
   writeln(s);
   check(s);
   str(e,s);
   writeln(s);
   check(s);
end.
