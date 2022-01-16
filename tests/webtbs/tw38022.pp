uses
  variants;
type
  tr = packed record
    b : byte;
    v : tvardata;
  end;
procedure p(var a : variant);
var
  vd : tvardata;
begin
  vd:=tvardata(a);
end;

var
  r : tr;
begin
  p(variant(r.v));
end.
