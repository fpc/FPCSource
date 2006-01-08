{ Source provided for Free Pascal Bug Report 4669 }
{ Submitted by "C.Stolk" on  2006-01-07 }
{ e-mail: C.stolk@cos.rotterdam.nl }
program test;

const
  nul1:array[boolean] of byte=(0,1);
var
  b,j,h:byte;
begin
b:=0;
for j:=1 to 2 do
  begin
  h:=
    nul1[(b=2) and (b=j)] or
    nul1[(b=6)or(b=j)];
  end;
end.
