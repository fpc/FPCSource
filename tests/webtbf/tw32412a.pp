{ %FAIL }

program tw32412;

var
  p: Pointer;
begin
  Delete();
  Delete(p);
  Delete(p, 1);
  Insert();
  Insert(p);
  Insert(p, 1);
end.
