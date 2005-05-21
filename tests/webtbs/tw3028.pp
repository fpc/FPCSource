{ Source provided for Free Pascal Bug Report 3028 }
{ Submitted by "Vincent Snijders" on  2004-03-28 }
{ e-mail: vslist@zonnet.nl }
program caseconstant;

{$mode objfpc}{$H+}

const
  c1 = -(551);
  c2 = -551;

begin
  writeln(c1,' $', hexstr(c1, 8));
  writeln(c2,' $', hexstr(c2, 8));
  if (c1<>c2) then
  begin
    writeln('Failed');
    halt(1);
  end
  else writeln('OK');
end.
