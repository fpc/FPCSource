program bug0068;

var
  p : pointer;
  l  : longint;
begin
  l:=Ofs(p); { Ofs returns a pointer type !? }
  
end.
