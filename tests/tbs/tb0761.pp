{ %target=win32,win64 }
{ %result=204 }

{ a Widestring cannot be made longer than its target allows (tb0763): growing
  one to High(SizeInt)-2 characters is run time error 204, as a failed
  allocation is, before any size is computed -- with the terminator's room
  that size wraps to 0, the block passes for large enough and the terminator
  would be stored two bytes before it. The block here is the test's own,
  which its memory manager claims: 1 the bytes before it were written, 2
  SetLength returned without writing them, 3 the reallocation was reached }
program tb0761;

{$mode objfpc}
{$warn symbol_deprecated off}

var
  Arena: array[0..63] of byte;
  W: WideString;
  Block: pointer;
  OldMM, GuardMM: TMemoryManager;

function GuardMemSize(p: pointer): PtrUInt;
begin
  if p=Block then
    GuardMemSize:=32
  else
    GuardMemSize:=OldMM.MemSize(p);
end;

function GuardFreeMem(p: pointer): PtrUInt;
begin
  { the test's own block is not the heap's }
  if p=Block then
    GuardFreeMem:=0
  else
    GuardFreeMem:=OldMM.FreeMem(p);
end;

function GuardReAllocMem(var p: pointer; size: PtrUInt): pointer;
begin
  if p=Block then
    begin
      Pointer(W):=nil;
      SetMemoryManager(OldMM);
      halt(3);
    end;
  GuardReAllocMem:=OldMM.ReAllocMem(p,size);
end;

begin
  { the memory manager allocates Widestrings }
  winwidestringalloc:=false;
  FillChar(Arena,sizeof(Arena),$A5);
  { a Widestring of one character in the arena: the length field, 'x', #0 }
  Block:=@Arena[16];
  PDWord(Block)^:=2;
  PWord(Block+4)^:=Ord('x');
  PWord(Block+6)^:=0;
  Pointer(W):=Block+4;
  GetMemoryManager(OldMM);
  GuardMM:=OldMM;
  GuardMM.MemSize:=@GuardMemSize;
  GuardMM.FreeMem:=@GuardFreeMem;
  GuardMM.ReAllocMem:=@GuardReAllocMem;
  SetMemoryManager(GuardMM);
  SetLength(W,High(SizeInt)-2);
  Pointer(W):=nil;
  SetMemoryManager(OldMM);
  if (Arena[14]<>$A5) or (Arena[15]<>$A5) then
    begin
      writeln('the terminator was stored before the block');
      halt(1);
    end;
  writeln('SetLength returned');
  halt(2);
end.
