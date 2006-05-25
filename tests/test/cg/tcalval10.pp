{ %cpu=i386}
{ %target=darwin }

type
  tr= packed  record
    b1, b2: byte;
  end;

procedure test(b: tr);mwpascal;
begin
  if b.b2 <> 5 then
    halt(1);
end;

procedure t;
var
  r: tr;
begin
  r.b2 := 5;
  test(r);
end;

procedure t2(b: byte); mwpascal;
var
  p: plongint;
begin
  p := plongint(@b);
  if p^ <> 1 then
    halt(2);
end;

procedure t3;
var
  b,b2,b3,b4: byte;
begin
  b := 1;
  b2 := 2;
  b3 := 3;
  b4 := 4;
  t2(b);
end;

begin
  t;
  t3;
end.
