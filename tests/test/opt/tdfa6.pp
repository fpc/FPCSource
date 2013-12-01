{ %OPT=-Oodfa -Sew -vw -S2}
{ %NORUN}

program tdfa5;

function getmeandinc(var i : longint) : longint;
  begin
    result:=i;
    inc(i);
  end;


function f(i : longint) : longint;
var
  j : longint;
begin
  if getmeandinc(j)>i then
    repeat
      if j>-1 then
        inc(j);
    until j>i;
  result:=i+j;
end;

begin
end.
