var
  target : longint;

begin
  target:=1234;
  InterLockedCompareExchange(target,4321,1234);
  if target<>4321 then
    halt(1);
  writeln('ok');
end.
