{%skiptarget=wince}
var
  f:file;
  p:pointer;
  i : integer;
begin

  Assign(f,'blockwritetest.tmp');
  {$I-}
  Rewrite(f,1);
  p:=nil;
  BlockWrite(f,p^,12345);
  Close(f);
  Erase(f);
  {$I+}
  i:=ioresult;
  writeln('IOResult: ',i);
  if i=101 then
    halt(1);
end.
