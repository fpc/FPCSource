{ %fail }

var
  i : integer;
begin
  if i=2 then
    begin
      for i:=1 to 10 do
      { The next line should be forbidden }
        for i:=1 to 9 do
          writeln(i);
    end;
end.

