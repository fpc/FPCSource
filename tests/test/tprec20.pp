{ %fail }

type
  pbyte = ^byte;

  tr = bitpacked record
    a,b,c: byte;
    d,e:0..15;
    f: byte;
    g: 0..$ffffff; { 3 bytes }
    h: byte;
  end;

procedure p(b: pbyte);
begin
  b^ := $12
end;

var
  r: tr;
begin
  p(@r.d);
end.
