{ %OPT=-Oodfa -Sew -vw -S2}
{ %NORUN}

program tdfa5;

function f(i : longint) : longint;
begin
  fillchar(i,sizeof(i),0);
  result:=i;
end;

begin
end.
