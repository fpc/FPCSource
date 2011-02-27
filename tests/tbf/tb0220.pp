{ %fail }

type
  tset = set of byte;
begin
  if 2 in tset then
    writeln('should not compile')
end.
