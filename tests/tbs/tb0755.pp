{ %target=win32,win64 }

{ a thread whose standard output is a text device that allocates, and keeps
  what it allocated, when it writes. The end of the thread writes it out: what
  the device allocated must not be left to the thread's state in the heap,
  which lives in the thread's block of threadvars, released at the end of the
  thread -- then freeing those blocks from another thread writes to the
  released block. A thread manager whose ReleaseThreadVars keeps that thread's
  block instead of releasing it leaves the block allocated, the test's to
  read: the program frees the kept blocks from its own thread and the block
  must be untouched. Cell 1: the thread leaves the text in the buffer; cell 2:
  the memory manager's DoneThread writes it, so it is written out after that
  hook -- still before the heap gives the thread's memory up. 0 when every
  cell holds; otherwise the cell's number times ten plus: 1 the block was
  written, 2 the block of threadvars was not found, 3 the device did not
  allocate, 4 the thread did not end, 5 the thread manager was not asked to
  release the block }
program tb0755;

{$mode objfpc}

uses
  windows;

const
  KeepCount = 32;
  KeptSize = 1000;

type
{$push}{$packrecords c}
  THeapEntry = record
    lpData: pointer;
    cbData: DWord;
    cbOverhead, iRegionIndex: byte;
    wFlags: word;
    rest: array[0..2*sizeof(pointer)+8-1] of byte;
  end;
{$pop}

function HeapWalkEntry(h: THandle; var e: THeapEntry): LongBool; stdcall; external 'kernel32' name 'HeapWalk';

var
  Kept: array[0..KeepCount-1] of pointer;
  KeptCount: longint;
  Armed: DWord;
  ReleaseKept: boolean;
  { whether the memory manager's DoneThread writes the text, not the thread }
  HookWrites: boolean;
  OldTM, KeepTM: TThreadManager;
  OldMM, HookMM: TMemoryManager;
  Base: pointer;
  Size: PtrUInt;
  Snapshot: array[0..65535] of byte;

threadvar
  TV: longint;

procedure DeviceInOut(var t: TextRec);
begin
  if t.BufPos=0 then
    exit;
  t.BufPos:=0;
  if GetCurrentThreadId=Armed then
    while KeptCount<KeepCount do
      begin
        GetMem(Kept[KeptCount],KeptSize);
        inc(KeptCount);
      end;
end;

procedure HookDone;
begin
  if GetCurrentThreadId=Armed then
    Write(Output,'x');
  if assigned(OldMM.DoneThread) then
    OldMM.DoneThread();
end;

{ the armed thread's block of threadvars is not released: it stays allocated }
procedure KeepThreadVars;
begin
  if GetCurrentThreadId=Armed then
    ReleaseKept:=true
  else
    OldTM.ReleaseThreadVars();
end;

{ the busy block of the process heap that holds the address }
procedure FindBlock(p: pointer);
var
  e: THeapEntry;
begin
  FillChar(e,sizeof(e),0);
  HeapLock(GetProcessHeap);
  while HeapWalkEntry(GetProcessHeap,e) do
    if (e.wFlags and PROCESS_HEAP_ENTRY_BUSY<>0) and (PtrUInt(p)>=PtrUInt(e.lpData)) and
       (PtrUInt(p)<PtrUInt(e.lpData)+e.cbData) then
      begin
        Base:=e.lpData;
        Size:=e.cbData;
      end;
  HeapUnlock(GetProcessHeap);
end;

function Body(p: pointer): ptrint;
begin
  TV:=1;
  FindBlock(@TV);
  TextRec(Output).InOutFunc:=@DeviceInOut;
  TextRec(Output).FlushFunc:=nil;
  { left in the buffer for the end of the thread }
  if not HookWrites then
    Write(Output,'x');
  Body:=0;
end;

procedure Cell(n: longint; hook: boolean);
var
  h, id: TThreadID;
  i: PtrUInt;
begin
  HookWrites:=hook;
  KeptCount:=0;
  ReleaseKept:=false;
  Base:=nil;
  Size:=0;
  if hook then
    SetMemoryManager(HookMM);
  h:=BeginThread(nil,0,@Body,nil,CREATE_SUSPENDED,id);
  Armed:=DWord(id);
  ResumeThread(h);
  if WaitForThreadTerminate(h,20000)<>0 then
    halt(n*10+4);
  CloseThread(h);
  Armed:=0;
  if hook then
    SetMemoryManager(OldMM);
  if not ReleaseKept then
    halt(n*10+5);
  if (Base=nil) or (Size>sizeof(Snapshot)) then
    halt(n*10+2);
  if KeptCount<>KeepCount then
    halt(n*10+3);
  Move(Base^,Snapshot,Size);
  for i:=0 to KeptCount-1 do
    FreeMem(Kept[i]);
  for i:=0 to Size-1 do
    if PByte(Base)[i]<>Snapshot[i] then
      begin
        writeln('cell ',n,': the block of threadvars was written at offset ',i);
        halt(n*10+1);
      end;
end;

begin
{$ifndef FPC_USE_TLS_DIRECTORY}
  { without a TLS directory nothing tells the run time library of the end of a
    thread that returns from its body, so there is nothing to test }
  writeln('skipped: the compiler makes no TLS directory');
  exit;
{$endif FPC_USE_TLS_DIRECTORY}
  GetThreadManager(OldTM);
  KeepTM:=OldTM;
  KeepTM.ReleaseThreadVars:=@KeepThreadVars;
  SetThreadManager(KeepTM);
  GetMemoryManager(OldMM);
  HookMM:=OldMM;
  HookMM.DoneThread:=@HookDone;
  Cell(1,false);
  Cell(2,true);
  SetThreadManager(OldTM);
end.
