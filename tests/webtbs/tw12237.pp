function f: longint; safecall;
begin
  f:=-2;
end;

begin
  if (f<>-2) then
    halt(1);
end.
