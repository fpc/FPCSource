{ Old file: tbs0068.pp }
{  Shows incorrect type of ofs()                        OK 0.99.1 (PFV and FK) }

program bug0068;

var
  p : pointer;
  l  : longint;
begin
  l:=Ofs(p); { Ofs returns a pointer type !? }

end.
