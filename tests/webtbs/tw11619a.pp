{ %norun }
{$mode objfpc}
{$inline on}
{$x-}

function Min(a, b: Double): Double;inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function Max(a, b: Double): Double;inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

var
  a, b: double;
begin
  a:=min(max(a,b),min(a,b));
end.
