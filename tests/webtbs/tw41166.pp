{ %OPT=-O2 }
program tw41166;

{$mode objfpc} {$q-,r-}
function SatSub(a, b: SizeUint): SizeUint; inline;
begin
  result := a - b;
  if a < b then result := 0;
end;

var
  r: SizeUint;

begin
  r := SatSub((random(0) + 3) * 2, 1);
  if r <> 5 then
  begin
    writeln('r = ', r);
    halt(1);
  end;
  writeln('ok');
end.
