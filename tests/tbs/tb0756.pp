{ %target=win32,win64,linux }

{ a thread whose standard output is a text device that allocates when it
  writes, under a memory manager whose DoneThread is counted. What the thread
  left in the buffer is written out at the thread's end before the memory
  manager's DoneThread, which cleans up the heap structures of the thread, so
  that the device allocates before them: nothing is allocated on the thread
  after its DoneThread. With threads of BeginThread and, on Windows, of
  CreateThread. 0 when nothing is allocated on the thread after its
  DoneThread; otherwise the cell's number times ten plus: 1 something was, 2
  DoneThread was not called once, 3 the device did not write, 4 the thread did
  not end }
program tb0756;

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
  Armed: TThreadID;
  DoneCalls, Writes, LateAllocs: longint;

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
  if assigned(OldMM.DoneThread) then
    OldMM.DoneThread();
  { counted once the manager below has cleaned up, so that what it allocates
    while it does is not taken for an allocation after DoneThread }
  if GetCurrentThreadId=Armed then
    inc(DoneCalls);
end;

procedure DeviceInOut(var t: TextRec);
var
  p: pointer;
begin
  if t.BufPos=0 then
    exit;
  t.BufPos:=0;
  if GetCurrentThreadId=Armed then
    begin
      inc(Writes);
      GetMem(p,100);
      FreeMem(p);
    end;
end;

procedure Work;
begin
  Armed:=GetCurrentThreadId;
  TextRec(Output).InOutFunc:=@DeviceInOut;
  TextRec(Output).FlushFunc:=nil;
  { left in the buffer for the end of the thread }
  Write(Output,'x');
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

procedure Cell(n: longint; native: boolean);
var
  h: TThreadID;
{$ifdef windows}
  nh: THandle;
  nid: DWord;
{$endif windows}
begin
  DoneCalls:=0;
  Writes:=0;
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
  if Writes<>1 then
    halt(n*10+3);
  if DoneCalls<>1 then
    halt(n*10+2);
  if LateAllocs>0 then
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
  SetMemoryManager(CountMM);
  Cell(1,false);
{$ifdef windows}
  Cell(2,true);
{$endif windows}
  SetMemoryManager(OldMM);
end.
