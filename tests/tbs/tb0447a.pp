{ %KNOWNRUNERROR=2 v10 computes cardinal > longint as cardinals }
{$R-}
var
  a : cardinal;
  b : longint;
begin
  a := 0;
  b := -1;
  if a > b then
    writeln ('OK')
  else
{$ifdef VER0}
    halt(2);
{$else not VER0}
{$ifdef VER1_0}
    halt(2);
{$else}
    halt(1);
{$endif}
{$endif}
end.

