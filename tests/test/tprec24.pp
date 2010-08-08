{ %fail }

type
  ta = bitpacked record
    a, b, c, d: 0..15;
  end;
var
  c: ta;
  d: byte absolute c.a;
begin
end.

