{ %KNOWNRUNERROR=201 v1.0 computes cardinal > longint as cardinal values }

{$R+}
var
  a : cardinal;
  b : longint;
begin
  a := 0;
  b := -1;
  if a > b then
    writeln ('OK')
  else
    halt(1);
end.

