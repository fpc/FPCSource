{ Source provided for Free Pascal Bug Report 1677 }
{ Submitted by "Anders Lindeberg" on  2001-11-10 }
{ e-mail: anders.lindeberg@telia.com }
program test;
type trec = record i:integer; s:ansistring end;
procedure p1(const r:trec);
  begin
  end;

procedure p2(r:trec); 
  begin
  end;

procedure p3(const a:ansistring);
  begin
  end;

procedure p4(a:ansistring);
  begin
  end;

var r:trec; s:ansistring;
begin
  s:=chr(ord('A')+random(26));
  r.s:=s;
  writeln('init');
  if plongint(pointer(s)-4)^<>3 then
    halt(1);
  writeln('p1()');
  p1(r);
  if plongint(pointer(s)-4)^<>3 then
    halt(1);
  writeln('p2()');
  p2(r);
  if plongint(pointer(s)-4)^<>3 then
    halt(1);
  writeln('ok');
end.


