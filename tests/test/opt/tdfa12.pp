{ %OPT=-Oodfa -Sew -vw -S2 }
function f(var i : longint) : boolean;
  begin
    result:=true;
    i:=0;
  end;

var
  i : longint;

begin
  if f(i) or (i=0) then
    halt(i)
  else
    halt(1);
end.
