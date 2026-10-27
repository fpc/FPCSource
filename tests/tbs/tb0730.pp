{ %OPT=-O3 }
{ the peephole optimizer may not move a load across a store through a
  different pointer which might point to the same memory location }
program tb0730;

{$mode objfpc}

var
  res : SizeInt;

procedure sink(a, b, d, item : SizeInt);
  begin
    res:=item;
  end;

procedure t(q, r : PSizeInt; c : byte);
  var
    temp : SizeInt;
  begin
    temp:=q^;
    r^:=r^ shl c;
    sink(1,2,3,temp);
  end;

var
  x : SizeInt;
begin
  x:=5;
  t(@x,@x,1);
  if (res<>5) or (x<>10) then
    halt(1);
  writeln('ok');
end.
