{ Old file: tbs0068.pp }
{  Shows incorrect type of ofs()                        OK 0.99.1 (PFV and FK) }

var
  p : pointer;
  l  : smallint;
begin
  l:=Ofs(p); { Ofs returns a pointer type !? }

end.
