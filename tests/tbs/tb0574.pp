{$r+}
{$q+}
var
  i1,i2 : int64;
begin
  i1:=$100000000;
  i2:=$800000000;
  inc(i1,i2);
  if i1<>$900000000 then
    halt(1);
  writeln('ok');
end.
