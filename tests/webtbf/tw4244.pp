{ %fail }

function f: pointer;
begin
  f := pointer(1);
end;

var
  p : pointer;
begin
  p := @longint(f);
end.
