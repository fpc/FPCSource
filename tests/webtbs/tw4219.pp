{ Source provided for Free Pascal Bug Report 4219 }
{ Submitted by "Marijn Kruisselbrink" on  2005-07-25 }
{ e-mail: mkruisselbrink@hexis.nl }

{$mode objfpc}
program test;

procedure f1(const p: array of const);
begin
  write('f1:');
  writeln(p[0].VType);
  if p[0].VType<>vtInteger then
    halt(1);
end;

procedure f2(const p: array of TVarRec);
begin
  write('f2:');
  writeln(p[0].VType);
  if p[0].VType<>vtInteger then
    halt(1);
end;

var
  p: array of TVarRec;
begin
  setlength(p, 1);
  p[0].VType := vtInteger;
  p[0].VInteger := 0;

  f1(p);
  f2(p);
  writeln('ok');
end.
