type
  TType = packed record
    a: byte;
    b: byte;
    c: longword;
  end;

  ttypecontainer = packed record
    r: ttype;
    b1,b2: byte;
  end;

function make: TType;
begin
  make.a:=1;
  make.b:=2;
  make.c:=$12345678;
end;

var
  id: ttypecontainer;
begin
  id.b1:=123;
  id.b2:=234;
  id.r := make();
  if id.r.a<>1 then
    halt(1);
  if id.r.b<>2 then
    halt(2);
  if id.r.c<>$12345678 then
    halt(3);
  if id.b1<>123 then
    halt(4);
  if id.b2<>234 then
    halt(5);
end.

