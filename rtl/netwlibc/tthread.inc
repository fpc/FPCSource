{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Peter Vreman

    Netware Libc TThread implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  What follows, is a short description on my implementation of TThread.
  Most information can also be found by reading the source and accompanying
  comments.

  A thread is created using BeginThread, which in turn calls
  pthread_create. So the threads here are always posix threads.
  Posix doesn't define anything for suspending threads as this is
  inherintly unsafe. Just don't suspend threads at points they cannot
  control. Therefore, I didn't implement .Suspend() if its called from
  outside the threads execution flow (except on Linux _without_ NPTL).

  The implementation for .suspend uses a semaphore, which is initialized
  at thread creation. If the thread tries to suspend itself, we simply
  let it wait on the semaphore until it is unblocked by someone else
  who calls .Resume.

  If a thread is supposed to be suspended (from outside its own path of
  execution) on a system where the symbol LINUX is defined, two things
  are possible.
  1) the system has the LinuxThreads pthread implementation
  2) the system has NPTL as the pthread implementation.

  In the first case, each thread is a process on its own, which as far as
  know actually violates posix with respect to signal handling.
  But we can detect this case, because getpid(2) will
  return a different PID for each thread. In that case, sending SIGSTOP
  to the PID associated with a thread will actually stop that thread
  only.
  In the second case, this is not possible. But getpid(2) returns the same
  PID across all threads, which is detected, and TThread.Suspend() does
  nothing in that case. This should probably be changed, but I know of
  no way to suspend a thread when using NPTL.

  If the symbol LINUX is not defined, then the unimplemented
  function SuspendThread is called.

  Johannes Berg <johannes@sipsolutions.de>, Sunday, November 16 2003
}

// ========== semaphore stuff ==========
{
  I don't like this. It eats up 2 filedescriptors for each thread,
  and those are a limited resource. If you have a server programm
  handling client connections (one per thread) it will not be able
  to handle many if we use 2 fds already for internal structures.
  However, right now I don't see a better option unless some sem_*
  functions are added to systhrds.
  I encapsulated all used functions here to make it easier to
  change them completely.
}

{ ok, so this is a hack, but it works nicely. Just never use
  a multiline argument with WRITE_DEBUG! }
{$MACRO ON}
{$IFDEF DEBUG_MT}
{$define WRITE_DEBUG := ConsolePrintf} // actually write something
{$ELSE}
{$define WRITE_DEBUG := //}      // just comment out those lines
{$ENDIF}


function SemaphoreInit: Pointer;
begin
  SemaphoreInit := GetMem(SizeOf(TFilDes));
  fppipe(PFilDes(SemaphoreInit)^);
end;

procedure SemaphoreWait(const FSem: Pointer);
var
  b: byte;
begin
  fpread(PFilDes(FSem)^[0], b, 1);
end;

procedure SemaphorePost(const FSem: Pointer);
var c : char;
begin
  c := #0;
  fpwrite(PFilDes(FSem)^[1], c, 1);
end;

procedure SemaphoreDestroy(const FSem: Pointer);
begin
  fpclose(PFilDes(FSem)^[0]);
  fpclose(PFilDes(FSem)^[1]);
  FreeMemory(FSem);
end;

// =========== semaphore end ===========

type
  PThreadRec=^TThreadRec;
  TThreadRec=record
    thread : TThread;
    next   : PThreadRec;
  end;

var
  ThreadRoot : PThreadRec;
  ThreadsInited : boolean = false;
  DisableRemoveThread : boolean;
  ThreadCount: longint = 0;
{$IFDEF LINUX}
  GMainPID: LongInt = 0;
{$ENDIF}

const
  // stupid, considering its not even implemented...
  Priorities: array [TThreadPriority] of Integer =
   (-20,-19,-10,0,9,18,19);

procedure InitThreads;
begin
  if not ThreadsInited then begin
    ThreadsInited := true;
    {$IFDEF LINUX}
    GMainPid := fpgetpid();
    {$ENDIF}
    ThreadRoot:=nil;
    ThreadsInited:=true;
    DisableRemoveThread:=false;
  end;
end;

procedure DoneThreads;
var
  hp,next : PThreadRec;
begin
  DisableRemoveThread := true;    {to avoid that Destroy calling RemoveThread modifies Thread List}
  while assigned(ThreadRoot) do
   begin
     WRITE_DEBUG('DoneThreads: calling Destroy'#13#10);
     ThreadRoot^.Thread.Destroy;
     hp:=ThreadRoot;
     ThreadRoot:=ThreadRoot^.Next;
     dispose(hp);
     WRITE_DEBUG('DoneThreads: called destroy, remaining threads: %d ThreadRoot: %x'#13#10,ThreadCount,longint(ThreadRoot));
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



function ThreadFunc(parameter: Pointer): LongInt;
var
  LThread: TThread;
  c: char;
begin
  WRITE_DEBUG('ThreadFunc is here...'#13#10);
  LThread := TThread(parameter);
  {$IFDEF LINUX}
  // save the PID of the "thread"
  // this is different from the PID of the main thread if
  // the LinuxThreads implementation is used
  LThread.FPid := fpgetpid();
  {$ENDIF}
  WRITE_DEBUG('thread initing, parameter = %d'#13#10, LongInt(LThread));
  try
    // wait until AfterConstruction has been called, so we cannot
    // free ourselves before TThread.Create has finished
    // (since that one may check our VTM in case of $R+, and
    //  will call the AfterConstruction method in all cases)
    LThread.Suspend;
    if LThread.FInitialSuspended then begin
       LThread.Suspend;
      if not LThread.FInitialSuspended then begin
        CurrentThreadVar := LThread;
        WRITE_DEBUG('going into LThread.Execute'#13#10);
        LThread.Execute;
      end;
    end else begin
      CurrentThreadVar := LThread;
      WRITE_DEBUG('going into LThread.Execute'#13#10);
      LThread.Execute;
    end;
  except
    on e: exception do begin
      WRITE_DEBUG('got exception: %s'#13#10,pchar(e.message));
      LThread.FFatalException :=  TObject(AcquireExceptionObject);
      // not sure if we should really do this...
      // but .Destroy was called, so why not try FreeOnTerminate?
      if e is EThreadDestroyCalled then LThread.FFreeOnTerminate := true;
    end;
  end;
  WRITE_DEBUG('thread done running'#13#10);
  Result := LThread.FReturnValue;
  WRITE_DEBUG('Result is %d'#13#10,Result);
  LThread.FFinished := True;
  LThread.DoTerminate;
  if LThread.FreeOnTerminate then begin
    WRITE_DEBUG('Thread should be freed'#13#10);
    LThread.Free;
    WRITE_DEBUG('Thread freed'#13#10);
  end;
  WRITE_DEBUG('thread func calling EndThread'#13#10);
  EndThread(Result);
end;

{ TThread }
procedure TThread.SysCreate(CreateSuspended: Boolean;
                            const StackSize: SizeUInt);
begin
  // lets just hope that the user doesn't create a thread
  // via BeginThread and creates the first TThread Object in there!
  InitThreads;
  AddThread(self);
  FSem := SemaphoreInit;
  FSuspended := False;
  FSuspendedExternal := false;
  FInitialSuspended := CreateSuspended;
  FFatalException := nil;
  WRITE_DEBUG('creating thread, self = %d'#13#10,longint(self));
  FHandle:= BeginThread(@ThreadFunc, Pointer(Self), FThreadID, StackSize);
  WRITE_DEBUG('TThread.Create done'#13#10);
end;


procedure TThread.SysDestroy;
begin
  if FThreadID = GetCurrentThreadID then begin
    raise EThreadDestroyCalled.Create('A thread cannot destroy itself except by setting FreeOnTerminate and leaving!');
  end;
  // if someone calls .Free on a thread with
  // FreeOnTerminate, then don't crash!
  FFreeOnTerminate := false;
  if not FFinished and not FSuspended then begin
    Terminate;
    WaitFor;
  end;
  if (FInitialSuspended) then begin
    // thread was created suspended but never woken up.
    SemaphorePost(FSem);
    WaitFor;
  end;
  FFatalException.Free;
  FFatalException := nil;
  SemaphoreDestroy(FSem);
  RemoveThread(self);          {remove it from the list of active threads}
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
  if not FSuspended then begin
    if FThreadID = GetCurrentThreadID then begin
      FSuspended := true;
      SemaphoreWait(FSem);
    end else begin
      FSuspendedExternal := true;
{$IFDEF LINUX}
      // naughty hack if the user doesn't have Linux with NPTL...
      // in that case, the PID of threads will not be identical
      // to the other threads, which means that our thread is a normal
      // process that we can suspend via SIGSTOP...
      // this violates POSIX, but is the way it works on the
      // LinuxThreads pthread implementation. Not with NPTL, but in that case
      // getpid(2) also behaves properly and returns the same PID for
      // all threads. Thats actually (FINALLY!) native thread support :-)
      if FPid <> GMainPID then begin
        FSuspended := true;
        fpkill(FPid, SIGSTOP);
      end;
{$ELSE}
      SuspendThread(FHandle);
{$ENDIF}
    end;
  end;
end;


procedure TThread.Resume;
begin
  if (not FSuspendedExternal) then begin
    if FSuspended then begin
      SemaphorePost(FSem);
      FInitialSuspended := false;
      FSuspended := False;
    end;
  end else begin
{$IFDEF LINUX}
    // see .Suspend
    if FPid <> GMainPID then begin
      fpkill(FPid, SIGCONT);
      FSuspended := False;
    end;
{$ELSE}
    ResumeThread(FHandle);
{$ENDIF}
    FSuspendedExternal := false;
  end;
end;


procedure TThread.Terminate;
begin
  FTerminated := True;
  TerminatedSet;
end;

function TThread.WaitFor: Integer;
begin
  WRITE_DEBUG('waiting for thread ',FHandle);
  WaitFor := WaitForThreadTerminate(FHandle, 0);
  WRITE_DEBUG('thread terminated');
end;

procedure TThread.CallOnTerminate;
begin
  // no need to check if FOnTerminate <> nil, because
  // thats already done in DoTerminate
  FOnTerminate(self);
end;

procedure TThread.DoTerminate;
begin
  if Assigned(FOnTerminate) then
    Synchronize(@CallOnTerminate);
end;

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
  P := ThreadGetPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then
      Result := I;
end;


procedure TThread.SetPriority(Value: TThreadPriority);
begin
  ThreadSetPriority(FHandle, Priorities[Value]);
end;

