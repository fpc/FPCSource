{$bitpacking on}

type
  tenum = (ea,eb,ec,ed,ee,ef,eg,eh);
  tr = packed record
         a: 0..3;    // 2 bits
         w: word;    // 16 bits;
         b: 0..31;   // 5 bits;
         c: boolean; // 1 bit
         d: 0..31;   // 5 bits
         e: tenum;   // 3 bits
       end;

procedure t(var r2: tr);
var
  r: tr;
begin
  r.a := 2;
  r.w := 32768;
  r.b := 23;
  r.c := true;
  r.d := 5;
  r.e := ed;
  r2 := r;
end;

var
  r: tr;

begin
  t(r);
  if (r.a <> 2) or
     (r.w <> 32768) or
     (r.b <> 23) or
     (not r.c) or
     (r.d <> 5) or
     (r.e <> ed) then
    halt(1);
end.
