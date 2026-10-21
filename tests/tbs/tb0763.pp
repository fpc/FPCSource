{ %target=win32,win64 }
{ %result=204 }

{ the longest Widestring a target can have reaches the memory manager, and
  one character more is run time error 204: 2147483647 characters on a 64-bit
  target, whose byte length fills the length field, a DWord, and 2147483644 on
  a 32-bit one, which with the length field and the terminator fill a
  SizeUInt. The block is the test's own, which its memory manager claims; its
  reallocation for the longest returns to the test by a long jump, once it
  has checked the size asked for. 2 the longest was not handed to the memory
  manager, 3 one more was, 4 SetLength returned for one more, 5 the longest
  was refused, 6 the size asked for the longest was not its whole size }
program tb0763;

{$mode objfpc}
{$warn symbol_deprecated off}

const
{$ifdef CPU64}
  Longest = SizeInt(High(DWord) div 2);
{$else}
  Longest = SizeInt((High(SizeUInt)-6) div 2);
{$endif}

var
  Arena: array[0..63] of byte;
  W: WideString;
  Block: pointer;
  OldMM, GuardMM: TMemoryManager;
  { 1 while the longest is set, 2 while one more is }
  Phase: longint = 0;
  Reached: boolean = false;
  Back: jmp_buf;
  OldExit: CodePointer;

procedure CheckExit;
begin
  ExitProc:=OldExit;
  { a run time error while the longest is set: it was refused }
  if Phase=1 then
    ExitCode:=5;
end;

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
      if Phase=1 then
        begin
          { the length field, the characters and the terminator, in full }
          if size<>PtrUInt(Longest)*sizeof(WideChar)+6 then
            begin
              Phase:=0;
              Pointer(W):=nil;
              SetMemoryManager(OldMM);
              halt(6);
            end;
          Reached:=true;
          longjmp(Back,1);
        end;
      Pointer(W):=nil;
      SetMemoryManager(OldMM);
      halt(3);
    end;
  GuardReAllocMem:=OldMM.ReAllocMem(p,size);
end;

procedure Done(code: longint);
begin
  Pointer(W):=nil;
  SetMemoryManager(OldMM);
  halt(code);
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
  OldExit:=ExitProc;
  ExitProc:=@CheckExit;
  Phase:=1;
  if setjmp(Back)=0 then
    SetLength(W,Longest);
  Phase:=2;
  if not Reached then
    Done(2);
  SetLength(W,Longest+1);
  Done(4);
end.
