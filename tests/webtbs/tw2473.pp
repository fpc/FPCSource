{ Source provided for Free Pascal Bug Report 2473 }
{ Submitted by "julien" on  2003-04-25 }
{ e-mail: jfiiz@aol.com }

function afunc (line : ansistring; var i : qword)  : qword;
begin
  if (i>length(line[i])) then
    writeln('hello');
  afunc:=0;
end;

begin
end.
