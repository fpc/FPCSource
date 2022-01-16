{ %FAIL }

program tw32412c;

var
  p: Pointer;
begin
  Delete(p, 1, 2, 3);
end.
