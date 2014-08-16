{ %OPT=-Oodfa -Sew -vw}
{ %NORUN}

program tdfa1;

function f(i : longint) : longint;
begin
  repeat
    if i>1 then
      exit(3);
    if i<=1 then
      exit(4);
  until false;
end;

begin
end.
