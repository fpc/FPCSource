{ %fail }

{$mode macpas}

type
  tba = packed array[0..7] of boolean;
  tkeymap = packed array[0..3] of tba;

procedure test(var b: boolean);
begin
end;

var
  km: tkeymap;
begin
  test(km[1][2]);
end.
