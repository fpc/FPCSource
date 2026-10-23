{ %target=win32,win64,linux }

{ a text device whose FlushFunc keeps what it is given and whose InOutFunc
  delivers what was kept -- Flush calls InOutFunc to write a file out. What
  the thread writes, and what the widestring manager's ThreadFiniProc and the
  memory manager's DoneThread write at the thread's end, are all delivered,
  though each write left the file's buffer empty: what the thread wrote before
  the widestring manager's hook, what that hook wrote before the memory
  manager's (each hook notes | where it begins). With threads of BeginThread
  and, on Windows, of CreateThread. 0 when every cell holds; otherwise the
  cell's number times ten plus: 1 not all of it was delivered, each part
  before the next hook, 3 DoneThread was not called once, 4 the thread did
  not end }
program tb0764;

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
  OldMM, HookMM: TMemoryManager;
  OldWSM, HookWSM: TUnicodeStringManager;
  Armed: TThreadID;
  { what the device keeps and what it delivered (shortstrings: nothing
    allocated) }
  Kept, Delivered: shortstring;
  DoneCalls: longint;

procedure HookDone;
begin
  if GetCurrentThreadId=Armed then
    begin
      Delivered:=Delivered+'|';
      Write(Output,'tail');
    end;
  if assigned(OldMM.DoneThread) then
    OldMM.DoneThread();
  if GetCurrentThreadId=Armed then
    inc(DoneCalls);
end;

procedure WideFini;
begin
  if GetCurrentThreadId=Armed then
    begin
      Delivered:=Delivered+'|';
      Write(Output,'wide');
    end;
  if assigned(OldWSM.ThreadFiniProc) then
    OldWSM.ThreadFiniProc();
end;

procedure DeviceKeep(var t: TextRec);
var
  i: longint;
begin
  if GetCurrentThreadId=Armed then
    for i:=0 to t.BufPos-1 do
      Kept:=Kept+t.BufPtr^[i];
  t.BufPos:=0;
end;

procedure DeviceDeliver(var t: TextRec);
begin
  DeviceKeep(t);
  if GetCurrentThreadId=Armed then
    begin
      Delivered:=Delivered+Kept;
      Kept:='';
    end;
end;

procedure Work;
begin
  Armed:=GetCurrentThreadId;
  TextRec(Output).InOutFunc:=@DeviceDeliver;
  TextRec(Output).FlushFunc:=@DeviceKeep;
  Write(Output,'body');
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
  Kept:='';
  Delivered:='';
  DoneCalls:=0;
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
  if Delivered<>'body|wide|tail' then
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
  HookMM:=OldMM;
  HookMM.DoneThread:=@HookDone;
  GetWideStringManager(OldWSM);
  HookWSM:=OldWSM;
  HookWSM.ThreadFiniProc:=@WideFini;
  SetWideStringManager(HookWSM);
  SetMemoryManager(HookMM);
  Cell(1,false);
{$ifdef windows}
  Cell(2,true);
{$endif windows}
  SetMemoryManager(OldMM);
  SetWideStringManager(OldWSM);
end.
