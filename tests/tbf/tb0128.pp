{%skiptarget=$nothread }
{ %fail }

{$mode delphi}
{$threading on}

threadvar
  { Initializing threadvars is not allowed }
  p : longint = 1;
begin
end.
