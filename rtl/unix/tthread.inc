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

  The implementation for .suspend uses an RTLEvent, which is initialized
  at thread creation. If the thread tries to suspend itself, we simply
  let it wait on the Event until it is unblocked by someone else
  who calls .Resume.


  Johannes Berg <johannes@sipsolutions.de>, Sunday, November 16 2003
}

{ ok, so this is a hack, but it works nicely. Just never use
  a multiline argument with WRITE_DEBUG! }

{.$DEFINE DEBUG_MT}
{$MACRO ON}
{$IFDEF DEBUG_MT}
{$define WRITE_DEBUG := writeln} // actually write something
{$ELSE}
{$define WRITE_DEBUG := //}      // just comment out those lines
{$ENDIF}

var
  ThreadsInited: boolean = false;
const
  // stupid, considering its not even implemented...
  Priorities: array [TThreadPriority] of Integer =
   (-20,-19,-10,0,9,18,19);


procedure DoneThreads;
begin
  ThreadsInited := false;
end;

function ThreadFunc(parameter: Pointer): ptrint;
var
  LThread: TThread;
  LFreeOnTerminate: boolean;
{$ifdef DEBUG_MT}
  lErrorAddr, lErrorBase: Pointer;
{$endif}
begin
  WRITE_DEBUG('ThreadFunc is here...');
  LThread := TThread(parameter);
  WRITE_DEBUG('thread initing, parameter = ', ptruint(LThread));
  try
    // wait until AfterConstruction has been called, so we cannot
    // free ourselves before TThread.Create has finished
    // (since that one may check our VTM in case of $R+, and
    //  will call the AfterConstruction method in all cases)
//    LThread.Suspend;
    WRITE_DEBUG('AfterConstruction should have been called for ',ptruint(lthread));
    if LThread.FInitialSuspended then
      begin
        WRITE_DEBUG('thread ', ptruint(LThread), ' waiting for RTLEvent ', ptruint(LThread.FSuspendEvent));
        RtlEventWaitFor(LThread.FSuspendEvent);
        if not(LThread.FTerminated) then
          begin
            if not LThread.FSuspended then
              begin
                LThread.FInitialSuspended := false;
                CurrentThreadVar := LThread;
                WRITE_DEBUG('going into LThread.Execute');
                LThread.Execute;
              end
            else
              WRITE_DEBUG('thread ', ptruint(LThread), ' initially created suspended, resumed, but still suspended?!');
          end
        else
          WRITE_DEBUG('initially created suspended, but already terminated');
      end
     else
       begin
         LThread.FSuspendedInternal := true;
         WRITE_DEBUG('waiting for SuspendedInternal - ', LThread.ClassName);
         RtlEventWaitFor(LThread.FSuspendEvent);
         CurrentThreadVar := LThread;
         WRITE_DEBUG('going into LThread.Execute - ', LThread.ClassName);
         LThread.Execute;
       end;
  except
    on e: exception do begin
      LThread.FFatalException := TObject(AcquireExceptionObject);
{$ifdef DEBUG_MT}
      lErrorAddr:=ExceptAddr;
      lErrorBase:=ExceptFrames^;
      writeln(stderr,'Exception caught in thread $',hexstr(LThread),
        ' at $',hexstr(lErrorAddr));
      writeln(stderr,BackTraceStrFunc(lErrorAddr));
      dump_stack(stderr,lErrorBase);
      writeln(stderr);
{$endif}
      // not sure if we should really do this...
      // but .Destroy was called, so why not try FreeOnTerminate?
      if e is EThreadDestroyCalled then LThread.FFreeOnTerminate := true;
    end;
  end;
  WRITE_DEBUG('thread done running');
  Result := LThread.FReturnValue;
  WRITE_DEBUG('Result is ',Result);
  LFreeOnTerminate := LThread.FreeOnTerminate;
  LThread.DoTerminate;
  LThread.FFinished := True;
  if LFreeOnTerminate then
    begin
      WRITE_DEBUG('Thread ',ptruint(lthread),' should be freed');
      LThread.Free;
      WRITE_DEBUG('Thread freed');
      WRITE_DEBUG('thread func calling EndThread');
      // we can never come here if the thread has already been joined, because
      // this function is the thread's main function (so it would have terminated
      // already in case it was joined)
      EndThread(Result);
    end;
end;

{ TThread }
procedure TThread.SysCreate(CreateSuspended: Boolean;
                            const StackSize: SizeUInt);
begin
  FSuspendEvent := RtlEventCreate;
  WRITE_DEBUG('thread ', ptruint(self), ' created RTLEvent ', ptruint(FSuspendEvent));
  FSuspended := CreateSuspended;
  FThreadReaped := false;
  FInitialSuspended := CreateSuspended;
  FFatalException := nil;
  FSuspendedInternal := not CreateSuspended;
  WRITE_DEBUG('creating thread, self = ',ptruint(self));
  FHandle:= BeginThread(@ThreadFunc, Pointer(Self), FThreadID, StackSize);
  if FHandle = TThreadID(0) then
    raise EThread.create('Failed to create new thread');
  WRITE_DEBUG('TThread.Create done, fhandle = ', ptruint(fhandle));
end;


procedure TThread.SysDestroy;
begin
  if not assigned(FSuspendEvent) then
    { exception in constructor }
    exit;
  if (FHandle = TThreadID(0)) then
  { another exception in constructor }
    begin
      RtlEventDestroy(FSuspendEvent);
      exit;
    end;
  if (FThreadID = GetCurrentThreadID) then
    begin
      if not(FFreeOnTerminate) and not FFinished then
        raise EThreadDestroyCalled.Create('A thread cannot destroy itself except by setting FreeOnTerminate and leaving!');
      FFreeOnTerminate := false;
    end
  else
    begin
      // if someone calls .Free on a thread with not(FreeOnTerminate), there
      // is no problem. Otherwise, FreeOnTerminate must be set to false so
      // when ThreadFunc exits the main runloop, it does not try to Free
      // itself again
      FFreeOnTerminate := false;
      { you can't join yourself, so only for FThreadID<>GetCurrentThreadID }
      { and you can't join twice -> make sure we didn't join already       }
      if not FThreadReaped then
        begin
          Terminate;
          if (FSuspendedInternal or FInitialSuspended) then
            Resume;
          WaitFor;
        end;
    end;
  RtlEventDestroy(FSuspendEvent);
  FFatalException.Free;
  FFatalException := nil;
  { threadvars have been released by cthreads.ThreadMain -> DoneThread, or  }
  { or will be released (in case of FFreeOnTerminate) after this destructor }
  { has exited by ThreadFunc->EndThread->cthreads.CEndThread->DoneThread)   }
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
  if FThreadID = GetCurrentThreadID then
    begin
      if not FSuspended and
         (InterLockedExchange(longint(FSuspended),longint(longbool(true))) = longint(longbool(false))) then
        RtlEventWaitFor(FSuspendEvent)
    end
  else
    begin
      Raise EThread.create('Suspending one thread from inside another one is unsupported (because it is unsafe and deadlock prone) by *nix and posix operating systems');
    end;
end;


procedure TThread.Resume;
begin
  if FSuspendedInternal and (InterLockedExchange(longint(FSuspendedInternal),ord(false)) = longint(longbool(true))) then
    begin
      WRITE_DEBUG('resuming thread after TThread construction',ptruint(self));
      RtlEventSetEvent(FSuspendEvent);
    end
  else
    begin
      if FSuspended and
         { don't compare with ord(true) or ord(longbool(true)), }
         { becaue a longbool's "true" value is anyting <> false }
         (InterLockedExchange(longint(FSuspended),longint(false)) <> longint(longbool(false))) then
        begin
          WRITE_DEBUG('resuming ',ptruint(self));
          RtlEventSetEvent(FSuspendEvent);
        end
    end
end;


procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: Integer;
begin
  WRITE_DEBUG('waiting for thread ',ptruint(FHandle));
  If (MainThreadID=GetCurrentThreadID) then 
    {
     FFinished is set after DoTerminate, which does a synchronize of OnTerminate, 
     so make sure synchronize works (or indeed any other synchronize that may be 
     in progress)
    }
    While not FFinished do  
      CheckSynchronize(100);
  WaitFor := WaitForThreadTerminate(FHandle, 0);
  { should actually check for errors in WaitForThreadTerminate, but no }
  { error api is defined for that function                             }
  FThreadReaped:=true;
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
