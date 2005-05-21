{ %version=1.1 }

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
