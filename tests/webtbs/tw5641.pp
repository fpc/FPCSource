// Title: bad construction status of object.
{$C+}
uses Objects;

var
  mempool: Pointer;
  obj: PObject;
begin
  GetMem(mempool, $2000000);
  Assert(mempool<>nil, 'GetMem failed');
  obj:=Pointer((Cardinal(mempool) or $FF));
  Assert(obj^.init, 'case 1a ((addr and $FF) <> 0)');
  Assert(obj^.init, 'case 1b ((addr and $FF) <> 0)');
  Assert(obj^.init, 'case 1c ((addr and $FF) <> 0)');
  obj:=Pointer((Cardinal(mempool) or $FF) + 1);
  Assert(obj^.init, 'case 2 (addr=$xxxxxx00)');
  obj:=Pointer((Cardinal(mempool) or $FFFF) + 1);
  Assert(obj^.init, 'case 3 (addr=$xxxx0000)');
  obj:=Pointer((Cardinal(mempool) or $FFFFFF) + 1);
  Assert(obj^.init, 'case 4 (addr=$xx000000)');
  FreeMem(mempool);
  writeln('ok');
end.
