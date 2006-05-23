type
  tr = record
    b1, b2, b3, b4: shortint;
  end;

procedure t;
var
  r: tr;
  l: longint;
begin
  r.b1 := 1;
  r.b2 := 2;
  r.b3 := 3;
  r.b4 := 4;
  l := -1;
  r.b2 := l;
  if (r.b1 <> 1) or
     (r.b2 <> -1) or
     (r.b3 <> 3) or
     (r.b4 <> 4) then
    begin
      writeln('error');
      halt(1);
    end;
end;

begin
  t;
end.

