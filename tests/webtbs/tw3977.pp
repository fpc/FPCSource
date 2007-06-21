{ Source provided for Free Pascal Bug Report 3977 }
{ Submitted by "lqs" on  2005-05-17 }
{ e-mail: lqs@users.sf.net }
var
  n:integer;
  f : text;
begin
  assign(f,'tw3977.tmp');
  rewrite(f);
  write(f,'1');
  close(f);

  assign(input,'tw3977.tmp');
  reset(input);
  readln(n);
  close(input);
  erase(input);
end.
