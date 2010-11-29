var
  l: longint;
{$ifdef cpu64}
  i: int64;
{$endif}

begin
  l:=-123;
  if interlockedcompareexchange(l,-1,124)<>-123 then
    halt(1);
  if l<>-123 then
    halt(2);
  if interlockedcompareexchange(l,-1,-123)<>-123 then
    halt(3);
  if l<>-1 then
    halt(4);

{$ifdef cpu64}
  i:=-123;
  if interlockedcompareexchange64(i,-1,124)<>-123 then
    halt(5);
  if i<>-123 then
    halt(6);
  if interlockedcompareexchange64(i,-1,-123)<>-123 then
    halt(7);
  if i<>-1 then
    halt(8);
{$endif}
end.
