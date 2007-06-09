{ %fail }

type
  trange = 0..$ffffff;

  tr = bitpacked record
    a,b,c: byte;
    d,e:0..15;
    f: byte;
    g: trange; { 3 bytes }
    h: byte;
  end;

procedure p(var b: trange);
begin
  b := $12
end;

var
  r: tr;
begin
  p(r.g);
end.
