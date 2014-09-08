{ %opt=-O2 }

type
  trec = record
    w: longint;
  end;

function test(var r: trec): byte;

begin
  test:=byte(r.w);
  r.w:=r.w shr 8;
end;

var
  r: trec;
begin
  r.w:=$1234;
  if test(r)<>$34 then
    halt(1);
  if r.w<>$12 then
    halt(2);
end.
