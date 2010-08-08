{ %interactive }

{ run in gdb, then check whether all absolute variables can be
  printed and show the correct values
}

type
  ta = record
    a,b: word;
    c,d: longint;
  end;

procedure check(a, b, c: longint);
begin
  if (a<>b) then
    halt(c);
end;


procedure local;
var
  ra: ta = (a: 12345; b: $abcd; c: 987654321; d: -1);
  arra: array[4..7] of ta = ((a: 1; b: 2; c: 3; d: 4),
                             (a: 5; b: 6; c: 7; d: 8),
                             (a: 9; b: 10; c: -1; d: -2),
                             (a: 11; b: 12; c: -3; d: -4)
                            );
var
  a: word absolute ra.a;
  b: word absolute ra.b;
  c: longint absolute ra.c;
  d: longint absolute ra.d;

  e: word absolute arra[5].a;
  f: word absolute arra[5].b;
  g: word absolute arra[6].a;
  h: word absolute arra[7].b;
  i: longint absolute arra[4].c;
  j: longint absolute arra[4].d;
  k: longint absolute arra[5].c;
  l: longint absolute arra[6].d;
begin
  check(a,ra.a,1);
  check(b,ra.b,2);
  check(c,ra.c,3);
  check(d,ra.d,4);

  check(e,5,5);
  check(f,6,6);
  check(g,9,7);
  check(h,12,8);

  check(i,3,9);
  check(j,4,10);
  check(k,7,11);
  check(l,-2,12);
end;

const
  ra: ta = (a: 12345; b: $abcd; c: 987654321; d: -1);
  arra: array[4..7] of ta = ((a: 1; b: 2; c: 3; d: 4),
                             (a: 5; b: 6; c: 7; d: 8),
                             (a: 9; b: 10; c: -1; d: -2),
                             (a: 11; b: 12; c: -3; d: -4)
                            );

var
  a: word absolute ra.a;
  b: word absolute ra.b;
  c: longint absolute ra.c;
  d: longint absolute ra.d;

  e: word absolute arra[5].a;
  f: word absolute arra[5].b;
  g: word absolute arra[6].a;
  h: word absolute arra[7].b;
  i: longint absolute arra[4].c;
  j: longint absolute arra[4].d;
  k: longint absolute arra[5].c;
  l: longint absolute arra[6].d;
begin
  local;
  check(a,ra.a,1);
  check(b,ra.b,2);
  check(c,ra.c,3);
  check(d,ra.d,4);

  check(e,5,5);
  check(f,6,6);
  check(g,9,7);
  check(h,12,8);

  check(i,3,9);
  check(j,4,10);
  check(k,7,11);
  check(l,-2,12);
end.
