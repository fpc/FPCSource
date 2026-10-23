{ %target=win32,win64,linux }

{ the order in which a thread's end calls the managers' hooks and the device
  of its standard output, recorded without allocating, under a memory manager
  of the C library: the standard files are written out before the hooks and
  again after each hook there is, before the heap's own end -- also when the
  buffer is empty, so a device can be called after the memory manager's
  DoneThread with nothing to write, and what that hook writes is delivered
  after it. A device whose FlushFunc is its InOutFunc writes at once. With an
  I/O error left on the thread nothing is written. In the log: D the device
  called with something to write, E with nothing, w the widestring manager's
  ThreadFiniProc, m the memory manager's DoneThread, M its end. 0 when every
  cell's log is as expected; otherwise the cell's number times ten plus: 1 the
  log differs, 4 the thread did not end }
program tb0765;

{$mode objfpc}
{$I-}

uses
  cmem
{$ifdef unix}
  , cthreads
{$endif unix}
  ;

var
  OldMM, HookMM: TMemoryManager;
  OldWSM, HookWSM: TUnicodeStringManager;
  Armed: TThreadID;
  { the cell: whether each hook is there, whether the device is also the
    FlushFunc, and what writes }
  WideHook, MemHook, Automatic, BodyWrites, HooksWrite, IOError: boolean;
  { a shortstring: nothing allocated }
  Log: shortstring;

procedure Note(c: char);
begin
  if GetCurrentThreadId=Armed then
    Log:=Log+c;
end;

procedure HookDone;
begin
  Note('m');
  if (GetCurrentThreadId=Armed) and HooksWrite then
    Write(Output,'h');
  if assigned(OldMM.DoneThread) then
    OldMM.DoneThread();
  Note('M');
end;

procedure WideFini;
begin
  Note('w');
  if (GetCurrentThreadId=Armed) and HooksWrite then
    Write(Output,'h');
  if assigned(OldWSM.ThreadFiniProc) then
    OldWSM.ThreadFiniProc();
end;

procedure Device(var t: TextRec);
begin
  if t.BufPos<>0 then
    Note('D')
  else
    Note('E');
  t.BufPos:=0;
end;

function Body(p: pointer): ptrint;
begin
  Armed:=GetCurrentThreadId;
  TextRec(Output).InOutFunc:=@Device;
  if Automatic then
    TextRec(Output).FlushFunc:=@Device
  else
    TextRec(Output).FlushFunc:=nil;
  if BodyWrites then
    Write(Output,'x');
  if IOError then
    InOutRes:=5;
  Body:=0;
end;

procedure Cell(n: longint; wide, mem, auto, bodyw, hooksw, ioerr: boolean; const expected: shortstring);
var
  h: TThreadID;
begin
  WideHook:=wide;
  MemHook:=mem;
  Automatic:=auto;
  BodyWrites:=bodyw;
  HooksWrite:=hooksw;
  IOError:=ioerr;
  if WideHook then
    HookWSM.ThreadFiniProc:=@WideFini
  else
    HookWSM.ThreadFiniProc:=nil;
  SetWideStringManager(HookWSM);
  if MemHook then
    HookMM.DoneThread:=@HookDone
  else
    HookMM.DoneThread:=nil;
  SetMemoryManager(HookMM);
  Log:='';
  h:=BeginThread(@Body);
  if WaitForThreadTerminate(h,20000)<>0 then
    halt(n*10+4);
  CloseThread(h);
  Armed:=TThreadID(0);
  if Log<>expected then
    begin
      writeln('cell ',n,': ',Log,' expected ',expected);
      halt(n*10+1);
    end;
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
  HookMM:=OldMM;
  GetWideStringManager(OldWSM);
  HookWSM:=OldWSM;
  { no hooks: one flush, of what the thread left or of nothing }
  Cell(1,false,false,false,true,false,false,'D');
  Cell(2,false,false,false,false,false,false,'E');
  { one hook: written out before it, called again after it }
  Cell(3,true,false,false,true,false,false,'DwE');
  Cell(4,false,true,false,true,false,false,'DmME');
  { both hooks writing: each hook's output delivered after that hook }
  Cell(5,true,true,false,true,true,false,'DwDmMD');
  { the device is the FlushFunc: each write delivered at once }
  Cell(6,true,true,true,true,true,false,'DEwDEmDME');
  { an I/O error left on the thread: nothing written, nothing flushed }
  Cell(7,true,true,false,true,true,true,'wmM');
  SetMemoryManager(OldMM);
  SetWideStringManager(OldWSM);
end.
