{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   TThread implementation old (1.0) and new (pthreads) style

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}



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
{$ifdef VER2_0}
var
  b : byte;
{$endif}
begin
{$ifdef VER2_0}
  b:=0;
  fpwrite(PFilDes(FSem)^[1], b, 1);
{$else}
  fpwrite(PFilDes(FSem)^[1], #0, 1);
{$endif}
end;

procedure SemaphoreDestroy(const FSem: Pointer);
begin
  fpclose(PFilDes(FSem)^[0]);
  fpclose(PFilDes(FSem)^[1]);
  FreeMemory(FSem);
end;

// =========== semaphore end ===========

var
  ThreadsInited: boolean = false;
const
  // stupid, considering its not even implemented...
  Priorities: array [TThreadPriority] of Integer =
   (-20,-19,-10,0,9,18,19);

procedure InitThreads;
begin
  if not ThreadsInited then begin
    ThreadsInited := true;
  end;
end;

procedure DoneThreads;
begin
  ThreadsInited := false;
end;

{ ok, so this is a hack, but it works nicely. Just never use
  a multiline argument with WRITE_DEBUG! }
{$MACRO ON}
{$IFDEF DEBUG_MT}
{$define WRITE_DEBUG := writeln} // actually write something
{$ELSE}
{$define WRITE_DEBUG := //}      // just comment out those lines
{$ENDIF}

function ThreadFunc(parameter: Pointer): LongInt; cdecl;
var
  LThread: TThread;
  c: char;
begin
  WRITE_DEBUG('ThreadFunc is here...');
  LThread := TThread(parameter);
  WRITE_DEBUG('thread initing, parameter = ', LongInt(LThread));
  try
    if LThread.FInitialSuspended then begin
      SemaphoreWait(LThread.FSem);
      if not LThread.FSuspended then begin
        LThread.FInitialSuspended := false;
        WRITE_DEBUG('going into LThread.Execute');
        LThread.Execute;
      end;
    end else begin
      WRITE_DEBUG('going into LThread.Execute');
      LThread.Execute;
    end;
  except
    on e: exception do begin
      WRITE_DEBUG('got exception: ',e.message);
      LThread.FFatalException :=  TObject(AcquireExceptionObject);
      // not sure if we should really do this...
      // but .Destroy was called, so why not try FreeOnTerminate?
      if e is EThreadDestroyCalled then LThread.FFreeOnTerminate := true;
    end;
  end;
  WRITE_DEBUG('thread done running');
  Result := LThread.FReturnValue;
  WRITE_DEBUG('Result is ',Result);
  LThread.FFinished := True;
  LThread.DoTerminate;
  if LThread.FreeOnTerminate then begin
    WRITE_DEBUG('Thread should be freed');
    LThread.Free;
    WRITE_DEBUG('Thread freed');
  end;
  WRITE_DEBUG('thread func calling EndThread');
  EndThread(Result);
end;

{ TThread }
constructor TThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
begin
  // lets just hope that the user doesn't create a thread
  // via BeginThread and creates the first TThread Object in there!
  InitThreads;
  inherited Create;
  FSem := SemaphoreInit;
  FSuspended := CreateSuspended;
  FSuspendedExternal := false;
  FInitialSuspended := CreateSuspended;
  FFatalException := nil;
  WRITE_DEBUG('creating thread, self = ',longint(self));
  FHandle:= BeginThread(@ThreadFunc, Pointer(Self), FThreadID, StackSize);
  WRITE_DEBUG('TThread.Create done');
end;


destructor TThread.Destroy;
begin
  if (FThreadID = GetCurrentThreadID) and not(FFreeOnTerminate) and not ffinished then begin
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
  inherited Destroy;
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
      SuspendThread(FHandle);
    end;
  end;
end;


procedure TThread.Resume;
begin
  if (not FSuspendedExternal) then begin
    if FSuspended then begin
      FSuspended := False;
      SemaphorePost(FSem);
    end;
  end else begin
    FSuspendedExternal := false;
    ResumeThread(FHandle);
  end;
end;


procedure TThread.Terminate;
begin
  FTerminated := True;
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

