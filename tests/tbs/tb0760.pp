{ %target=win32,win64 }
{ %result=204 }

{ a memory manager that cannot grow a block returns nil from ReAllocMem and
  keeps the block, as the heap does when ReturnNilIfGrowHeapFails is set.
  Growing a Widestring then ends with run time error 204, as a failed
  allocation of a new Widestring does, instead of going on with the block it
  still has, writing the terminator past it and the new length into it }
program tb0760;

var
  W: WideString;
  OldMM, FailMM: TMemoryManager;
  Armed: boolean = false;

function FailReAllocMem(var p: Pointer; Size: PtrUInt): Pointer;
begin
  if Armed and (p<>nil) and (Size>OldMM.MemSize(p)) then
    begin
      { once; p keeps the block }
      Armed:=false;
      FailReAllocMem:=nil;
      exit;
    end;
  FailReAllocMem:=OldMM.ReAllocMem(p,Size);
end;

begin
  { the memory manager allocates Widestrings }
  winwidestringalloc:=false;
  W:='abc';
  GetMemoryManager(OldMM);
  FailMM:=OldMM;
  FailMM.ReAllocMem:=@FailReAllocMem;
  SetMemoryManager(FailMM);
  Armed:=true;
  SetLength(W,1000);
  SetMemoryManager(OldMM);
  writeln('SetLength went on after the reallocation failed');
  halt(1);
end.
