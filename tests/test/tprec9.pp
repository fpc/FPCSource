{ %fail }

type
  tr = bitpacked record
    a,b: 0..7;
  end;

var
  r: tr;
  p: pointer;
begin
  p := @r.b;
end.
