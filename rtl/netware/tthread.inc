{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2003-2004 Armin Diehl, member of the Free Pascal
    development team

    Netware clib TThread implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


type
  PThreadRec=^TThreadRec;
  TThreadRec=record
    thread : TThread;
    next   : PThreadRec;
  end;

var
  ThreadRoot : PThreadRec;
  ThreadsInited : boolean;
  DisableRemoveThread : boolean;

Const
  ThreadCount: longint = 0;

{function ThreadSelf:TThread;
var
  hp : PThreadRec;
  sp : longint;
begin
  sp:=SPtr;
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
end;}


procedure InitThreads;
begin
  ThreadRoot:=nil;
  ThreadsInited:=true;
  DisableRemoveThread:=false;
end;

{DoneThreads will terminate all remaining threads}
procedure DoneThreads;
var
  hp,next : PThreadRec;
begin
  DisableRemoveThread := true;    {to avoid that Destroy calling RemoveThread modifies Thread List}
  while assigned(ThreadRoot) do
   begin
     ThreadRoot^.Thread.Destroy;
     hp:=ThreadRoot;
     ThreadRoot:=ThreadRoot^.Next;
     dispose(hp);
     {$ifdef DEBUG_MT}
     ConsolePrintf(#13'DoneThreads: called destroy, remaining threads: %d ThreadRoot: %x'#13#10,ThreadCount,longint(ThreadRoot));
     {$endif}
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

  inc(ThreadCount);
end;


procedure RemoveThread(t:TThread);
var
  lasthp,hp : PThreadRec;
begin
  if not DisableRemoveThread then  {disabled while in DoneThreads}
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
        Dec(ThreadCount);
        if ThreadCount = 0 then ThreadsInited := false;
        exit;
      end;
      lasthp:=hp;
      hp:=hp^.next;
    end;
  end else
    dec(ThreadCount);
end;


constructor TThread.Create(CreateSuspended: Boolean;
                           const StackSize: SizeUInt = DefaultStackSize);
var
  Flags: Integer;
begin
  inherited Create;
  AddThread(self);
  FSuspended := CreateSuspended;
  { Create new thread }
  FHandle := BeginThread (@ThreadProc,pointer(self));
  if FSuspended then Suspend;
  FThreadID := FHandle;
  FFatalException := nil;
end;


destructor TThread.Destroy;
begin
  if not FFinished then
  begin
    Terminate;
    if Suspended then
      ResumeThread (FHandle);  {netware can not kill a thread, the thread has to}
                               {leave it's execute routine if terminated is true}
    WaitFor;                   {wait for the thread to terminate}
  end;
  FFatalException.Free;
  FFatalException := nil;
  inherited Destroy;
  RemoveThread(self);          {remove it from the list of active threads}
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
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := ThreadGetPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
  ThreadSetPriority(FHandle, Priorities[Value]);
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
  SuspendThread (FHandle);
  FSuspended := true;
end;


procedure TThread.Resume;
begin
  ResumeThread (FHandle);
  FSuspended := False;
end;


procedure TThread.Terminate;
begin
  FTerminated := True;
  ThreadSwitch;
end;


function TThread.WaitFor: Integer;
begin
  Result := WaitForThreadTerminate (FHandle,0);
  if Result = 0 then
    FHandle := 0;
end;

