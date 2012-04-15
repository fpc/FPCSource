uses
  strings;

var
  p1, p2, p3, p4: pchar;
begin

 { StrECopy(Dest,Source) is equivalent to the following:
    strcopy(Dest,Source);
    StrECopy := StrEnd(Dest);
  }
  p1:='abcdefg';
  getmem(p2,100);
  p3:=strecopy(p2,p1);
  fillchar(p2^,100,0);
  strcopy(p2,p1);
  p4:=strend(p2);
  if p3<>p4 then
    halt(1);
end.
