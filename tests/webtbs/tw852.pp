type
  TFloat80Array = array [0..1000000] of Extended;

procedure AddFloat80Proc(var Vector1; const Vector2; Count: Integer);
var
  I: Integer;
begin
  for I:=0 to Count - 1 do
    TFloat80Array(Vector1)[I]:=TFloat80Array(Vector1)[I] + TFloat80Array(Vector2)[I];
end;

begin
end.
