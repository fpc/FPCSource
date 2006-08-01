{$mode macpas}

type
  tba = array[0..7] of boolean;
  tkeymap = packed array[0..3] of tba;

procedure test(var b: tba);
begin
end;

var
  km: tkeymap;
  i: longint;
  a: tba;
begin
  i := 1;
  fillchar(a,sizeof(a),0);
  km[i] := a;
end.
