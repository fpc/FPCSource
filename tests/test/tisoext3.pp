{ %fail }
{ this is not supposed to compile in non iso mode }
var
  f : file of byte;
  b : byte;
begin
  rewrite(f,'tisoext1.tmp');
  write(f,123);
  close(f);
end.
