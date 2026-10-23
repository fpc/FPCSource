{ %target=win32,win64,linux }

{ what a manager's hook of a thread's end writes to the standard output is
  written out: the memory manager's DoneThread and the widestring manager's
  ThreadFiniProc each write four characters, which the thread's text device
  must receive. What the widestring manager's hook writes is written out
  before the memory manager's DoneThread, which cleans up the heap structures
  of the thread: the device allocates when it writes, and nothing may be
  allocated on the thread after DoneThread then. With threads of BeginThread
  and, on Windows, of CreateThread; and with a device whose FlushFunc is its
  InOutFunc, as SetTextAutoFlush makes it, which allocates when it writes: it
  writes what the memory manager's hook writes at once, and nothing is left
  for the call after the hook. 0 when every cell holds; otherwise the cell's
  number times ten plus: 1 the device did not receive the four characters, 2
  the device allocated after DoneThread, 3 DoneThread was not called once, 4
  the thread did not end }
program tb0759;

{$mode objfpc}

uses
{$ifdef unix}
  cthreads;
{$endif unix}
{$ifdef windows}
  windows;
{$endif windows}

{$ifdef windows}
function CreateWinThread(sa: pointer; stack: PtrUInt; start: pointer; p: pointer; flags: DWord; var id: DWord): THandle; stdcall; external 'kernel32' name 'CreateThread';
{$endif windows}

var
  OldMM, CountMM: TMemoryManager;
  OldWSM, HookWSM: TUnicodeStringManager;
  Armed: TThreadID;
  { which hook writes: 1 the memory manager's, 2 the widestring manager's }
  Writer: longint;
  { whether the device allocates when it writes, and whether it is also the
    FlushFunc }
  Allocating, Flushing: boolean;
  { what the device wrote (a shortstring: nothing allocated) }
  Delivered: shortstring;
  DoneCalls, LateAllocs: longint;

procedure CountAlloc;
begin
  if (GetCurrentThreadId=Armed) and (DoneCalls>0) then
    inc(LateAllocs);
end;

function CountGetMem(size: PtrUInt): pointer;
begin
  CountAlloc;
  CountGetMem:=OldMM.GetMem(size);
end;

function CountAllocMem(size: PtrUInt): pointer;
begin
  CountAlloc;
  CountAllocMem:=OldMM.AllocMem(size);
end;

function CountReAllocMem(var p: pointer; size: PtrUInt): pointer;
begin
  CountAlloc;
  CountReAllocMem:=OldMM.ReAllocMem(p,size);
end;

procedure CountDone;
begin
  if (GetCurrentThreadId=Armed) and (Writer=1) then
    Write(Output,'tail');
  if assigned(OldMM.DoneThread) then
    OldMM.DoneThread();
  if GetCurrentThreadId=Armed then
    inc(DoneCalls);
end;

procedure WideFini;
begin
  if (GetCurrentThreadId=Armed) and (Writer=2) then
    Write(Output,'wide');
  if assigned(OldWSM.ThreadFiniProc) then
    OldWSM.ThreadFiniProc();
end;

procedure DeviceInOut(var t: TextRec);
var
  i: longint;
  p: pointer;
begin
  if t.BufPos=0 then
    exit;
  if GetCurrentThreadId=Armed then
    begin
      for i:=0 to t.BufPos-1 do
        Delivered:=Delivered+t.BufPtr^[i];
      if Allocating then
        begin
          GetMem(p,100);
          FreeMem(p);
        end;
    end;
  t.BufPos:=0;
end;

procedure Work;
begin
  Armed:=GetCurrentThreadId;
  TextRec(Output).InOutFunc:=@DeviceInOut;
  if Flushing then
    TextRec(Output).FlushFunc:=@DeviceInOut
  else
    TextRec(Output).FlushFunc:=nil;
end;

function Body(p: pointer): ptrint;
begin
  Work;
  Body:=0;
end;

{$ifdef windows}
function NativeBody(p: pointer): DWord; stdcall;
begin
  Work;
  NativeBody:=0;
end;
{$endif windows}

procedure Cell(n, hook: longint; native, devflush: boolean);
var
  h: TThreadID;
{$ifdef windows}
  nh: THandle;
  nid: DWord;
{$endif windows}
begin
  Writer:=hook;
  Flushing:=devflush;
  { the widestring manager's hook is written out before DoneThread, so the
    device may allocate there; the memory manager's after it -- unless the
    device writes it at once, as its FlushFunc }
  Allocating:=(hook=2) or devflush;
  Delivered:='';
  DoneCalls:=0;
  LateAllocs:=0;
{$ifdef windows}
  if native then
    begin
      nh:=CreateWinThread(nil,0,@NativeBody,nil,0,nid);
      if WaitForSingleObject(nh,20000)<>WAIT_OBJECT_0 then
        halt(n*10+4);
      CloseHandle(nh);
    end
  else
{$endif windows}
    begin
      h:=BeginThread(@Body);
      if WaitForThreadTerminate(h,20000)<>0 then
        halt(n*10+4);
      CloseThread(h);
    end;
  Armed:=TThreadID(0);
  if DoneCalls<>1 then
    halt(n*10+3);
  if LateAllocs>0 then
    halt(n*10+2);
  if ((hook=1) and (Delivered<>'tail')) or ((hook=2) and (Delivered<>'wide')) then
    halt(n*10+1);
end;

begin
{$if defined(windows) and not defined(FPC_USE_TLS_DIRECTORY)}
  { without a TLS directory nothing tells the run time library of the end of a
    thread that returns from its body, so there is nothing to test }
  writeln('skipped: the compiler makes no TLS directory');
  exit;
{$endif}
  Armed:=TThreadID(0);
  GetMemoryManager(OldMM);
  CountMM:=OldMM;
  CountMM.GetMem:=@CountGetMem;
  CountMM.AllocMem:=@CountAllocMem;
  CountMM.ReAllocMem:=@CountReAllocMem;
  CountMM.DoneThread:=@CountDone;
  GetWideStringManager(OldWSM);
  HookWSM:=OldWSM;
  HookWSM.ThreadFiniProc:=@WideFini;
  SetWideStringManager(HookWSM);
  SetMemoryManager(CountMM);
  Cell(1,1,false,false);
  Cell(2,2,false,false);
{$ifdef windows}
  Cell(3,1,true,false);
  Cell(4,2,true,false);
{$endif windows}
  Cell(5,1,false,true);
  SetMemoryManager(OldMM);
  SetWideStringManager(OldWSM);
end.
