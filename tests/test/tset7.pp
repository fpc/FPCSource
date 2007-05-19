{ test for subsetreg sets }

{$packset 1}

type
  ta = 0..7;
  tr = record
    b: byte;
    a: set of ta;
    w: word;
  end;


procedure test(r: tr);
var
  b: ta;
begin
  b := 6;
  if (r.b<>101) or
     (r.w<>$abcd) or
     (5 in r.a) or
     (b in r.a) or
     not(7 in r.a) or
     ([1..3] * r.a <> [2..3]) then
    halt(1);
end;

var
  r: tr;
begin
  r.b:=101;
  r.w:=$abcd;
  r.a:=[2..3];
  include(r.a,7);
  test(r);
end.
