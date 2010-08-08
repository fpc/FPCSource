var
  target : longint;
  ctarget : cardinal;

begin
  target:=1234;
  InterLockedCompareExchange(target,4321,1234);
  if target<>4321 then
    halt(1);
  ctarget:=1234;
  InterLockedCompareExchange(ctarget,4321,1234);
  if ctarget<>4321 then
    halt(1);
  writeln('ok');
end.
