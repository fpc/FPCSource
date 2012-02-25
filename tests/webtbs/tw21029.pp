{$r+}
{$inline on}

function F(y : byte) : byte; inline;
begin
  f:=byte(not y);
end;

BEGIN
  if F(1)<>254 then
    halt(1);
END.
