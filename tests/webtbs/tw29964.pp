{$packrecords 1}
{$r-}

var
  r: record
       i: int64;
       p: pointer;
       w: smallint;
     end;

var
  i: longint;
  b: byte;
  orgp,oldp,newp: pointer;
begin
  r.i:=-1;
  r.w:=-1;
  b:=$12;
  ptruint(r.p):=b;
  for i:=2 to sizeof(r.p) do
    begin
      b:=b*3;
      ptruint(r.p):=ptruint(r.p) shl 8 or b;
    end;
  orgp:=r.p;
  newp:=pointer(not ptruint(orgp));
  oldp:=InterlockedCompareExchangePointer(r.p,newp,r.p);
  if oldp<>orgp then
    halt(1);
  if r.p<>newp then
    halt(2);
  if r.i<>-1 then
    halt(3);
  if r.w<>-1 then
    halt(4);
end.
