{ %fail }

{$mode macpas}

type
  tba = array[0..7] of boolean;
  tkeymap = packed array[0..3] of tba;

procedure test(var b: tba);
begin
end;

var
  km: tkeymap;
begin
  test(km[1]);
end.
