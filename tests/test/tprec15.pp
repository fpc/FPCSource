type
  tr = bitpacked record
    a,b,c: byte;
    d,e:0..15;
    f: byte;
    g: 0..$ffffff; { 3 bytes }
    h: byte;
  end;

procedure p(var b: byte);
begin
  b := $12
end;

var
  r: tr;
begin
  fillchar(r,sizeof(r),0);
  p(r.a);
  if (r.a<>$12) then
    halt(1);

  fillchar(r,sizeof(r),0);
  p(r.b);
  if (r.b<>$12) then
    halt(1);

  fillchar(r,sizeof(r),0);
  p(r.c);
  if (r.c<>$12) then
    halt(1);

  fillchar(r,sizeof(r),0);
  p(r.f);
  if (r.f<>$12) then
    halt(1);

  fillchar(r,sizeof(r),0);
  p(r.h);
  if (r.h<>$12) then
    halt(1);
end.
