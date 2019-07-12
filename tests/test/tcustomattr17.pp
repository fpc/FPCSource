{ %FAIL }

program tcustomattr17;

{$mode objfpc}
{$modeswitch prefixedattributes}

type
  [TCustomAttribute]
  Int = Integer;

begin
end.
