{ Thread management routines }

const
  CM_EXECPROC = $8FFF;
  CM_DESTROYWINDOW = $8FFE;

type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: pointer; {PExceptionRecord}
  end;

var
  ThreadWindow: HWND;
  ThreadCount: Integer;
  { event that happens when gui thread is done executing the method
}

function ThreadWndProc(Window: HWnd; AMessage:UInt; WParam : WParam; LParam: LParam): Longint; {$ifdef wince}cdecl{$else}stdcall{$endif};

begin
  case AMessage of
    CM_EXECPROC:
      with TThread(lParam) do
      begin
        Result := 0;
        try
          FSynchronizeException := nil;
          FMethod;
        except
{          if RaiseList <> nil then
          begin
            FSynchronizeException := PRaiseFrame(RaiseList)^.ExceptObject;
            PRaiseFrame(RaiseList)^.ExceptObject := nil;
          end; }
        end;
      end;
  else
    Result := DefWindowProc(Window, AMessage, wParam, lParam);
  end;
end;

const
  ThreadWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @ThreadWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TThreadWindow');

procedure AddThread;

  function AllocateWindow: HWND;
  var
    TempClass: TWndClass;
    ClassRegistered: Boolean;
  begin
    ThreadWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance, ThreadWindowClass.lpszClassName,
      @TempClass);
    if not ClassRegistered or (TempClass.lpfnWndProc <> WndProc(@ThreadWndProc)) then
    begin
      if ClassRegistered then
        Windows.UnregisterClass(ThreadWindowClass.lpszClassName, HInstance);
      Windows.RegisterClass(ThreadWindowClass);
    end;
    Result := CreateWindow(ThreadWindowClass.lpszClassName, '', 0,
      0, 0, 0, 0, 0, 0, HInstance, nil);
  end;

begin
  if ThreadCount = 0 then
    ThreadWindow := AllocateWindow;
  InterlockedIncrement(ThreadCount);
end;

procedure RemoveThread;
begin
  { note that when thread count reaches 0 we must be in main thread context }
  { windows may only be destroyed in same thread as created in }
  { posting a message to window thread does not work when we have no message loop }
  if InterlockedDecrement(ThreadCount)=0 then
    DestroyWindow(ThreadWindow);
end;


constructor TThread.Create(CreateSuspended: Boolean;
                           const StackSize: SizeUInt = DefaultStackSize);
var
  Flags: Integer;
begin
  inherited Create;
  AddThread;
  FSuspended := CreateSuspended;
  Flags := 0;
  if CreateSuspended then Flags := CREATE_SUSPENDED;
  FHandle := BeginThread(nil, StackSize, @ThreadProc, pointer(self), Flags,
                         FThreadID);
  FFatalException := nil;
end;


destructor TThread.Destroy;
begin
  if not FFinished and not Suspended then
  begin
    Terminate;
    WaitFor;
  end;
  if FHandle <> 0 then CloseHandle(FHandle);
  FFatalException.Free;
  FFatalException := nil;
  inherited Destroy;
  RemoveThread;
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

procedure TThread.Terminate;
begin
  FTerminated := True;
end;

function TThread.WaitFor: Integer;
var
  Msg: TMsg;
  WaitHandles : array[0..1] of THandle;
begin
  if GetCurrentThreadID = MainThreadID then
    begin
      WaitHandles[0]:=FHandle;
      WaitHandles[1]:=THandle(SynchronizeTimeoutEvent);
      while true do
        begin
          case MsgWaitForMultipleObjects(2, WaitHandles, False, INFINITE, QS_SENDMESSAGE) of
            WAIT_OBJECT_0:
              break;
            WAIT_OBJECT_0+1:
              CheckSynchronize;
            WAIT_OBJECT_0+2:
              PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE)
          end;
        end;
    end
  else
    WaitForSingleObject(ulong(FHandle), INFINITE);
  GetExitCodeThread(FHandle, DWord(Result));
end;
