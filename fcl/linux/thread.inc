{
  $Id$

  Linux TThread implementation
}


{ Thread management routines }

const
  Sig_Cancel = SIGUSR2;

type
  PThreadRec=^TThreadRec;
  TThreadRec=record
    thread : TThread;
    next   : PThreadRec;
  end;

var
  ThreadRoot : PThreadRec;
  ThreadsInited : boolean;

function StackPointer:longint;assembler;
asm
        movl    %esp,%eax
end;


function ThreadSelf:TThread;
var
  hp : PThreadRec;
  sp : longint;
begin
  sp:=StackPointer;
  hp:=ThreadRoot;
  while assigned(hp) do
   begin
     if (sp<=hp^.Thread.FStackPointer) and
        (sp>(hp^.Thread.FStackPointer-hp^.Thread.FStackSize)) then
      begin
        Result:=hp^.Thread;
        exit;
      end;
     hp:=hp^.next;
   end;
  Result:=nil;
end;


procedure ThreadCancelHandler(Sig:integer);cdecl;
var
  p : TThread;
begin
  p:=ThreadSelf;
  if assigned(p) and (p.FCallExitProcess) then
   ExitProcess(p.FReturnValue);
end;


procedure InitThreads;
begin
  ThreadRoot:=nil;
  ThreadsInited:=true;
  { Install sig_cancel handler }
  Signal(Sig_Cancel,@ThreadCancelHandler);
end;


procedure DoneThreads;
var
  hp : PThreadRec;
begin
  while assigned(ThreadRoot) do
   begin
     ThreadRoot^.Thread.Destroy;
     hp:=ThreadRoot;
     ThreadRoot:=ThreadRoot^.Next;
     dispose(hp);
   end;
  ThreadsInited:=false;
end;


procedure AddThread(t:TThread);
var
  hp : PThreadRec;
begin
  { Need to initialize threads ? }
  if not ThreadsInited then
   InitThreads;
  { Put thread in the linked list }
  new(hp);
  hp^.Thread:=t;
  hp^.next:=ThreadRoot;
  ThreadRoot:=hp;
end;


procedure RemoveThread(t:TThread);
var
  lasthp,hp : PThreadRec;
begin
  hp:=ThreadRoot;
  lasthp:=nil;
  while assigned(hp) do
   begin
     if hp^.Thread=t then
      begin
        if assigned(lasthp) then
         lasthp^.next:=hp^.next
        else
         ThreadRoot:=hp^.next;
        dispose(hp);
        exit;
      end;
     lasthp:=hp;
     hp:=hp^.next;
   end;
end;


{ TThread }
function ThreadProc(Thread: TThread): Integer;cdecl;
var
  FreeThread: Boolean;
begin
  Thread.Execute;
  FreeThread := Thread.FFreeOnTerminate;
  Result := Thread.FReturnValue;
  Thread.FFinished := True;
  Thread.DoTerminate;
  if FreeThread then
    Thread.Free;
  ExitProcess(Result);
end;


constructor TThread.Create(CreateSuspended: Boolean);
var
  Flags: Integer;
begin
  inherited Create;
  AddThread(self);
  FSuspended := CreateSuspended;
  Flags:=CLONE_VM+CLONE_FS+CLONE_FILES+CLONE_SIGHAND;
  { Setup 16k of stack }
  FStackSize:=16384;
  Getmem(pointer(FStackPointer),FStackSize);
  inc(FStackPointer,FStackSize);
  FCallExitProcess:=false;
  { Clone }
  FHandle:= Clone(@ThreadProc,pointer(FStackPointer),Flags,self);
  FThreadID := FHandle;
end;


destructor TThread.Destroy;
begin
  if not FFinished and not Suspended then
   begin
     Terminate;
     WaitFor;
   end;
{ Remove stack }
  dec(FStackPointer,FStackSize);
  Freemem(pointer(FStackPointer),FStackSize);
  inherited Destroy;
  RemoveThread(self);
end;


procedure TThread.CallOnTerminate;
begin
  FOnTerminate(Self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    Synchronize(@CallOnTerminate);
end;


const
{ I Don't know idle or timecritical, value is also 20, so the largest other
  possibility is 19 (PFV) }
  Priorities: array [TThreadPriority] of Integer =
   (-20,-19,-10,9,10,19,20);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := Linux.GetPriority(Prio_Process,FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then
      Result := I;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
  Linux.SetPriority(Prio_Process,FHandle, Priorities[Value]);
end;


procedure TThread.Synchronize(Method: TThreadMethod);
begin
  FSynchronizeException := nil;
  FMethod := Method;
{  SendMessage(ThreadWindow, CM_EXECPROC, 0, Longint(Self)); }
  if Assigned(FSynchronizeException) then
    raise FSynchronizeException;
end;


procedure TThread.SetSuspended(Value: Boolean);
begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;


procedure TThread.Suspend;
begin
  FSuspended := True;
{  SuspendThread(FHandle); }
end;


procedure TThread.Resume;
begin
{  if ResumeThread(FHandle) = 1 then }
    FSuspended := False;
end;


procedure TThread.Terminate;
begin
  { Set the flag for this tthread, so the sighandler knows which tthread
    needs termination }
  FCallExitProcess:=true;
  Kill(FHandle,Sig_Cancel);
  FTerminated := True;
end;


function TThread.WaitFor: Integer;
var
  status : longint;
begin
  if FThreadID = MainThreadID then
   WaitPid(0,@status,0)
  else
   WaitPid(FHandle,@status,0);
  Result:=status;
end;

{
  $Log$
  Revision 1.1  1999-05-30 10:46:42  peter
    * start of tthread for linux,win32

}
