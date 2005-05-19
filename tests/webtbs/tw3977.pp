{ Source provided for Free Pascal Bug Report 3977 }
{ Submitted by "lqs" on  2005-05-17 }
{ e-mail: lqs@users.sf.net }
var
  n:integer;
begin
  assign(input,'tw3977.txt');
  reset(input);
  readln(n);
  close(output);
end.
