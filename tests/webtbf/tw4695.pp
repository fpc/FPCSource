{ %fail }
{ Source provided for Free Pascal Bug Report 4695 }
{ Submitted by "Ales Katona" on  2006-01-13 }
{ e-mail: almindor@gmail.com }
program test;

var
  i: Integer;
begin
  for i:=0 to 5 do
    for i:=5 downto 0 do
      Writeln('LOOPY');
end.
