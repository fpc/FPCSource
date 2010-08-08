uses
  types;

procedure TestAvg(const r: trect);
var
  res: tpoint;
  o1, o2: int64;
begin
  res:=centerpoint(r);
  o1 := (int64(r.left) + r.right) div 2;
  o2 := (int64(r.top) + r.bottom) div 2;
  if (res.x <> o1) or
     (res.y <> o2) then
    halt(1);
end;

var
 r: trect;
begin
  r.left := 1;
  r.right := 3;
  r.top := 3;
  r.bottom := 1;
  testavg(r);
  r.left:=2000000000;
  r.right:=2000000002;
  r.top:=-2000000000;
  r.bottom:=-2000000002;
  testavg(r);
  r.left:=-2000000000;
  r.right:=2000000002;
  r.top:=2000000000;
  r.bottom:=-2000000002;
end.

