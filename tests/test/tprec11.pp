{ from gpc testsuite, original name: waldek9b.pas }

{$ifdef fpc}
{$mode macpas}
{$endif}

program rrr(Output);
type tr = record end;
     tp = packed record
            i : tr;
          end;
var a : array [0..15] of tp;
    pa : packed array [0..15] of tp;
begin
  pack (a, 0, pa);
  if sizeof(a) <> 0 then
    halt(1);
  if (sizeof(pa) <> 0) then
    halt(2);
  if (sizeof(tr) <> 0) then
    halt(3);
  if (sizeof(tp) <> 0) then
    halt(4);
  WriteLn ('OK')
end.

