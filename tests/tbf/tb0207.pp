{ %opt=-Sew -vw }
{ %fail }
{$mode objfpc}
procedure p(out o);
  begin
  end;

{$modeswitch out-}
begin
end.
