program bug;

var l,h:longint;
    r:single;

begin
  l:=-2;
  h:=1;
  r:=0;
  r:=r+(single(h)-single(l))-1;
  if trunc(r) <> 2 then
    halt(1);
end.
