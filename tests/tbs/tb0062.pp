{ Old file: tbs0068.pp }
{  Shows incorrect type of ofs()                        OK 0.99.1 (PFV and FK) }

{ As l is only a smallint, this
  test will almost always
  trigger a range check error,
  disable range check explicitly }
{$R-}
var
  p : pointer;
  l  : smallint;
begin
  l:=Ofs(p); { Ofs returns a pointer type !? }

end.
