{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Karoly Balogh,
    member of the Free Pascal development team.

    native TThread implementation for Amiga-like systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Thread management routines }

{ Based on the Win32 version, but since that mostly just wraps to a stock
  ThreadManager, it was relatively straightforward to get this working,
  after we had a ThreadManager (AThreads) (KB) }

procedure TThread.SysCreate(CreateSuspended: Boolean;
                            const StackSize: SizeUInt);
begin
  FSuspended := CreateSuspended;
  FInitialSuspended := CreateSuspended;
  { Always start in suspended state, will be resumed in AfterConstruction if necessary
    See Mantis #16884 }
  FHandle := BeginThread(nil, StackSize, @ThreadProc, pointer(self), 1{CREATE_SUSPENDED},
                         FThreadID);
  if FHandle = TThreadID(0) then
    raise EThread.CreateFmt(SThreadCreateError, ['Cannot create thread.']);

  FFatalException := nil;
end;


procedure TThread.SysDestroy;
begin
  if FHandle<>0 then
    begin
      { Don't check Suspended. If the thread has been externally suspended (which is
        deprecated and strongly discouraged), it's better to deadlock here than
        to silently free the object and leave OS resources leaked. }
      if not FFinished {and not Suspended} then
        begin
          Terminate;
          { Allow the thread function to perform the necessary cleanup. Since
            we've just set Terminated flag, it won't call Execute. }
          if FInitialSuspended then
            Start;
          WaitFor;
        end;
    end;

  FFatalException.Free;
  FFatalException := nil;
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

{const
  Priorities: array [TThreadPriority] of Integer =
   (THREAD_PRIORITY_IDLE, THREAD_PRIORITY_LOWEST, THREAD_PRIORITY_BELOW_NORMAL,
    THREAD_PRIORITY_NORMAL, THREAD_PRIORITY_ABOVE_NORMAL,
    THREAD_PRIORITY_HIGHEST, THREAD_PRIORITY_TIME_CRITICAL);}

function TThread.GetPriority: TThreadPriority;
var
  P: Integer;
  I: TThreadPriority;
begin
{  P := GetThreadPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;}
end;

procedure TThread.SetPriority(Value: TThreadPriority);
begin
//  SetThreadPriority(FHandle, Priorities[Value]);
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
  { Unsupported, but lets have it... }
  FSuspended := True;
  SuspendThread(FHandle);
end;

procedure TThread.Resume;
begin
  if ResumeThread(FHandle) = 1 then FSuspended := False;
end;

procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: Integer;
begin
  result:=WaitForThreadTerminate(FThreadID,0);
  FFinished:=(result = 0);
end;
