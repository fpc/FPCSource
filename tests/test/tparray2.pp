{ %fail }

{$mode macpas}

type
  tba = array[0..7] of boolean;
  tkeymap = packed array[0..3] of tba;

var
  p: pointer;
  km: tkeymap;
begin
  p := @km[2];
end.

