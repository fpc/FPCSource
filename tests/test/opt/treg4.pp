{$inline on}
{$mode objfpc}

type
  tr = record
    case byte of
      1: (l: longint);
      2: (b1,b2,b3,b4: byte);
  end;

function f: tr; inline;
begin
  f.l := 5;
  f.b3 := 6;
end;

procedure t(const r1: tr);
begin
  if (r1.b3 <> 6) or
     ((r1.l and 255) <> 5) then
    halt(1);
end;

procedure t2(out r: tr);
begin
  r.l := 7;
  r.b2 := 31;
end;

var
  r: tr;
begin
  t(f);
  t2(r);
  if ((r.l and 255) <> 7) or
     (r.b2 <> 31) then
    halt(1);
end.
