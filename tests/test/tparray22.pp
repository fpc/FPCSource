{ %fail }

type
  trange = 0..$ffffff;
  tarr = bitpacked array[0..20] of trange;

procedure p(var a: trange);
begin
end;

var
  a: tarr;
begin
  a[0]:=5;
  p(a[0]);
end.
