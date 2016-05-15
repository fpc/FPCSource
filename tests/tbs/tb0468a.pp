{ %OPT=-Sg }

procedure foo;
begin
end;

procedure test;
label
  a,b,c,d;
const
  x: array[0..3] of pointer=(@a,@b,@c,@d);
begin
  foo;
a:
  foo;
b:
  foo;
c:
  foo;
d:
  foo;
end;


begin
end.

