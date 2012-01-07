{ %fail }
label
  l;
begin
  writeln(hexstr(ptrint(@l),sizeof(ptrint)*2));
end.
