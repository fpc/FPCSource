{ %cpu=i386,powerpc,powerpc64,x86_64 }
{%SKIPTARGET=wince}
// Title: bad construction status of object.
{$C+}
uses Objects;

var
  mempool: Pointer;
  obj: PObject;
begin
  GetMem(mempool, $2000000);
  Assert(mempool<>nil, 'GetMem failed');
  obj:=Pointer((ptruint(mempool) or $FF));
  Assert(obj^.init, 'case 1a ((addr and $FF) <> 0)');
  Assert(obj^.init, 'case 1b ((addr and $FF) <> 0)');
  Assert(obj^.init, 'case 1c ((addr and $FF) <> 0)');
  obj:=Pointer((SizeUint(mempool) or $FF) + 1);
  Assert(obj^.init, 'case 2 (addr=$xxxxxx00)');
  obj:=Pointer((SizeUint(mempool) or $FFFF) + 1);
  Assert(obj^.init, 'case 3 (addr=$xxxx0000)');
  obj:=Pointer((SizeUint(mempool) or $FFFFFF) + 1);
  Assert(obj^.init, 'case 4 (addr=$xx000000)');
  FreeMem(mempool);
  writeln('ok');
end.
