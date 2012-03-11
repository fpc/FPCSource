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
    halt(2);

  // Test what happens if we use a comparend which is NOT currently set
  target := 12345;
  if(InterLockedCompareExchange(target, 54321, 123) <> 12345) then
    halt(3);
  if target<>12345 then
    halt(4);

  writeln('ok');
end.
