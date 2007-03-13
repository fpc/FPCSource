{$inline on}

function f(const a,b: longint): longint; inline;
begin
  f:=a*b;
end;

begin
  if (f(f(f(2,5),3),4) <> 120) then
    halt(1);
end.
