{%opt=-gh}
{$mode macpas}

var
  p: pointer;

begin
  new(p,8);
  fillchar(p^,8,$56);
  dispose(p,8);
end.
