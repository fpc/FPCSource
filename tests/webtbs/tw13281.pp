{$ifdef cpu64}
procedure test64;
var
  c: qword;
begin
  c:=1;
  if interlockeddecrement64(c)<>0 then
    halt(10);
  if interlockedincrement64(c)<>1 then
    halt(20);
  if interlockedexchange64(c,1234)<>1 then
    halt(30);
  if InterLockedExchangeAdd64(c,5)<>1234 then
    halt(40);
  if InterlockedCompareExchange64(c,qword($8000000000000035),1239)<>1239 then
    halt(50);
  if (c<>qword($8000000000000035)) then
    halt(60);
end;
{$endif cpu64}

var
  c: cardinal;
begin
  c:=1;
  if interlockeddecrement(c)<>0 then
    halt(1);
  if (c<>0) then
    halt(11);
  if interlockedincrement(c)<>1 then
    halt(2);
  if (c<>1) then
    halt(12);
  if interlockedexchange(c,1234)<>1 then
    halt(3);
  if (c<>1234) then
    halt(13);
  if InterLockedExchangeAdd(c,5)<>1234 then
    halt(4);
  if (c<>1239) then
    halt(14);
  if InterlockedCompareExchange(c,$80000000,1239)<>1239 then
    halt(5);
  if (c<>$80000000) then
    halt(6);
end.
