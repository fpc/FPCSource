{$bitpacking on}

type
  tr = packed record
    b1, b2, b3, b4: -129..127;
  end;

procedure t;
var
  r: tr;
  l: longint;
begin
  r.b4 := -5;
  r.b3 := -128;
  r.b2 := 127;
  r.b1 := -1;
  if (r.b1 <> -1) or
     (r.b2 <> 127) or
     (r.b3 <> -128) or
     (r.b4 <> -5) then
    begin
      writeln('error');
      halt(1);
    end;
end;

begin
  t;
end.

