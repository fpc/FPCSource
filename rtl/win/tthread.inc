{ Thread management routines }

procedure TThread.SysCreate(CreateSuspended: Boolean;
                            const StackSize: SizeUInt);
begin
  FSuspended := CreateSuspended;
  FInitialSuspended := CreateSuspended;
  { Always start in suspended state, will be resumed in AfterConstruction if necessary
    See Mantis #16884 }
  FHandle := BeginThread(nil, StackSize, @ThreadProc, pointer(self), CREATE_SUSPENDED OR STACK_SIZE_PARAM_IS_A_RESERVATION,
                         FThreadID);
  if FHandle = TThreadID(0) then
    raise EThread.CreateFmt(SThreadCreateError, [SysErrorMessage(getlasterror)]);

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
  P := GetThreadPriority(FHandle);
  Result := tpNormal;
  for I := Low(TThreadPriority) to High(TThreadPriority) do
    if Priorities[I] = P then Result := I;
end;

procedure TThread.SetPriority(Value: TThreadPriority);
begin
  SetThreadPriority(FHandle, Priorities[Value]);
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
  SuspendThread(FHandle);
end;

procedure TThread.Resume;
begin
  if ResumeThread(FHandle) = 1 then FSuspended := False;
end;

{ Raises EThread with the last OS error }
procedure RaiseThreadError;

var
  lError: DWord;

begin
  lError:=GetLastError;
  raise EThread.CreateFmt(SThreadError,[SysErrorMessage(lError),lError]);
end;


function TThread.WaitFor: Integer;
var
  Msg: TMsg;
  WaitHandles : array[0..1] of THandle;
  lHandle: THandle;
  lExitCode: DWord;
begin
  { wait on a duplicate, FHandle is closed when a FreeOnTerminate thread frees itself }
  if not DuplicateHandle(GetCurrentProcess, FHandle, GetCurrentProcess, @lHandle, 0, False, DUPLICATE_SAME_ACCESS) then
    RaiseThreadError;
  try
    if GetCurrentThreadID = MainThreadID then
      begin
        WaitHandles[0]:=lHandle;
        WaitHandles[1]:=THandle(SynchronizeTimeoutEvent);
        while true do
          begin
            case MsgWaitForMultipleObjects(2, WaitHandles, False, INFINITE, QS_SENDMESSAGE) of
              WAIT_OBJECT_0:
                break;
              WAIT_OBJECT_0+1:
                CheckSynchronize;
              WAIT_OBJECT_0+2:
                PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
              WAIT_FAILED:
                RaiseThreadError;
            end;
          end;
      end
    else if WaitForSingleObject(lHandle, INFINITE)=WAIT_FAILED then
      RaiseThreadError;
    if not GetExitCodeThread(lHandle, lExitCode) then
      RaiseThreadError;
    Result:=Integer(lExitCode);
  finally
    CloseHandle(lHandle);
  end;
end;
