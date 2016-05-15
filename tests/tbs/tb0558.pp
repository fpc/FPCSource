{ %cpu=i386}
{ %opt=-Cfsse2 }

{$mode objfpc}

type
  TChartZPosition = 0..MaxInt;
  TDoublePoint = record x, y: double; end;

var
 d: double;
 z: TChartZPosition;
begin
  if not(has_sse2_support) then
   begin
    WriteLn ('CPU does not support SSE2, skipping test...');
    halt(0);
   end;
  d:=5.0;
  z:=3;
  d:=d-z;
  if (d<>2.0) then
    halt(1);
end.
