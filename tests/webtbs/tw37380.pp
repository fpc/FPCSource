{ %NORUN }

program tw37380;
{$mode delphi}
function GTest<T>(a: T; b: T = T(1)): T;
begin
  Result := a + b;
end;

function Foobar(a: Single; b: Single = Single(1)): Single;
begin
  Result := a + b;
end;

begin
  WriteLn(GTest<Single>(2, 1));
  WriteLn(Foobar(2));
end.

