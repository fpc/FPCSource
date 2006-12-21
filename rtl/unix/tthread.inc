{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    Copyright (c) 2006 by Jonas Maebe
    members of the Free Pascal development team.

    Generic *nix TThread implementation

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


  Johannes Berg <johannes@sipsolutions.de>, Sunday, November 16 2003
}

{ ok, so this is a hack, but it works nicely. Just never use
  a multiline argument with WRITE_DEBUG! }
{$MACRO ON}
{$IFDEF DEBUG_MT}
{$define WRITE_DEBUG := writeln} // actually write something
{$ELSE}
{$define WRITE_DEBUG := //}      // just comment out those lines
{$ENDIF}

var
  ThreadsInited: boolean = false;
  CurrentTM: TThreadManager;
const
  // stupid, considering its not even implemented...
  Priorities: array [TThreadPriority] of Integer =
   (-20,-19,-10,0,9,18,19);

procedure InitThreads;
begin
  { This is not thread safe, but it doesn't matter if this is executed }
  { multiple times. Conversely, if one thread goes by this without the }
  { operation having been finished by another thread already, it will  }
  { use an uninitialised thread manager -> leave as it is              }
  if not ThreadsInited then
    GetThreadManager(CurrentTM);
end;

procedure DoneThreads;
begin
  ThreadsInited := false;
end;

function ThreadFunc(parameter: Pointer): LongInt;
var
  LThread: TThread;
begin
  WRITE_DEBUG('ThreadFunc is here...');
  LThread := TThread(parameter);
  WRITE_DEBUG('thread initing, parameter = ', LongInt(LThread));
  try
    // wait until AfterConstruction has been called, so we cannot
    // free ourselves before TThread.Create has finished
    // (since that one may check our VTM in case of $R+, and
    //  will call the AfterConstruction method in all cases)
    LThread.Suspend;
    WRITE_DEBUG('AfterConstruction should have been called for ',ptrint(lthread));
    if LThread.FInitialSuspended then
      begin
        LThread.Suspend;
        if not(LThread.FTerminated) then
          begin
            if not LThread.FSuspended then
              begin
                LThread.FInitialSuspended := false;
                WRITE_DEBUG('going into LThread.Execute');
                LThread.Execute;
              end;
          end;
      end
     else
       begin
         WRITE_DEBUG('going into LThread.Execute');
         LThread.Execute;
       end;
  except
    on e: exception do begin
      WRITE_DEBUG('got exception: ',e.message);
      LThread.FFatalException := TObject(AcquireExceptionObject);
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
  if LThread.FreeOnTerminate then
    begin
      WRITE_DEBUG('Thread ',ptrint(lthread),' should be freed');
      LThread.Free;
      WRITE_DEBUG('Thread freed');
//    tthread.destroy already frees all things and terminates the thread
//    WRITE_DEBUG('thread func calling EndThread');
//    EndThread(Result);
    end
  else
    begin
      FlushThread;
    end;
end;

{ TThread }
constructor TThread.Create(CreateSuspended: Boolean;
                           const StackSize: SizeUInt = DefaultStackSize);
begin
  // lets just hope that the user doesn't create a thread
  // via BeginThread and creates the first TThread Object in there!
  InitThreads;
  inherited Create;
  FSem := CurrentTM.SemaphoreInit();
  if FSem = nil then
    raise EThread.create('Semaphore init failed (possibly too many concurrent threads)');
  FSuspended := True;
  FSuspendedExternal := false;
  FInitialSuspended := CreateSuspended;
  FFatalException := nil;
  WRITE_DEBUG('creating thread, self = ',longint(self));
  FHandle:= BeginThread(@ThreadFunc, Pointer(Self), FThreadID, StackSize);
  if FHandle = TThreadID(0) then
    raise EThread.create('Failed to create new thread');
  WRITE_DEBUG('TThread.Create done, fhandle = ', ptrint(fhandle));
end;


destructor TThread.Destroy;
begin
  if (FSem = nil) then
    { exception in constructor }
    begin
      inherited destroy;
      exit;
    end;
  CurrentTM.SemaphoreDestroy(FSem);
  if (FHandle = TThreadID(0)) then
  { another exception in constructor }
    begin
      inherited destroy;
      exit;
    end;
  if (FThreadID = GetCurrentThreadID) and not(FFreeOnTerminate) and not FFinished then
    raise EThreadDestroyCalled.Create('A thread cannot destroy itself except by setting FreeOnTerminate and leaving!');
  // if someone calls .Free on a thread with
  // FreeOnTerminate, then don't crash!
  FFreeOnTerminate := false;
  if not FFinished then
    begin
      Terminate;
      if (FInitialSuspended) then
        Resume;
      WaitFor;
    end;
  FFatalException.Free;
  FFatalException := nil;
  { threadvars have been released by cthreads.ThreadMain -> DoneThread }
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
      CurrentTM.SemaphoreWait(FSem);
    end else begin
      FSuspendedExternal := true;
      SuspendThread(FHandle);
    end;
  end;
end;


procedure TThread.Resume;
begin
  if (not FSuspendedExternal) then
    begin
      if FSuspended then
        begin
          WRITE_DEBUG('resuming ',ptrint(self));
          FSuspended := False;
          CurrentTM.SemaphorePost(FSem);
        end;
    end
  else
    begin
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
  WRITE_DEBUG('waiting for thread ',ptrint(FHandle));
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
