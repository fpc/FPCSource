Program bug0290;

var i: integer;

begin
  { the following line gives a warning and $ffff is changed to $7fff!}
  i := $ffff;
  if i <> $ffff then
    Writeln('bug!')
end.