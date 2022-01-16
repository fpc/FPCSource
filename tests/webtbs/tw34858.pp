{ %norun }

const a = 6;
{$IFDEF ENDIAN_LITTLE}
const b = 7;
{$ELSEIF a > 5}
const b = 7;
{$ELSE}
  {$ERROR Unknown endian}
{$IFEND}

begin
  if b<>7 then
    halt(1);
end.
