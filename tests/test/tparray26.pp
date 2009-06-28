{ %norun }

type
  ta = bitpacked array[0..high(longint)-1] of 0..1;
var
  p: pointer;
begin
  getmem(p,sizeof(ta));
  ta(p^)[high(longint)-1]:=1;
end.
