{ %OPT=-Or}
program rangecse;

{$r+}

type
  pa = ^ta;
  ta = array[0..100] of longint;

procedure t;
var
  i, j: longint;
  p: pa;
begin
  new(p);
  fillchar(p^,101*sizeof(longint),0);
  p^[100] := 5;
  j := 5;
  for i:=1 to 101 do
   if j=p^[i-1] then
    begin
      writeln('found!');
      dispose(p);
      exit;
    end;
  writeln('failed..');
  dispose(p);
  halt(1);
end;

begin
  t;
end.
