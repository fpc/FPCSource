{ Source provided for Free Pascal Bug Report 4902 }
{ Submitted by "Yu Hang" on  2006-03-14 }
{ e-mail: cosechy@gmail.com }
function x:real;
begin
end;

begin
x;
{anything here}
 writeln(1);
 writeln(2);
 writeln(3);
{crash here}
writeln(1.0)
end.
