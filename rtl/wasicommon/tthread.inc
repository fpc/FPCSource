{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TThread                                      *}
{****************************************************************************}


{$ifdef FPC_WASM_THREADS}
procedure TThread.CallOnTerminate;

begin
  FOnTerminate(self);
end;


function TThread.GetPriority: TThreadPriority;

begin
  GetPriority:=tpNormal;
end;


procedure TThread.SetPriority(Value: TThreadPriority);

begin
  // Not supported
end;


procedure TThread.SetSuspended(Value: Boolean);

begin
  if Value <> FSuspended then
    if Value then
      Suspend
    else
      Resume;
end;


procedure TThread.DoTerminate;

begin
  if Assigned(FOnTerminate) then
    Synchronize(@CallOnTerminate);
end;


function ThreadFunc(parameter: Pointer): ptrint;

Var
  LThread : TThread Absolute parameter;
  LFreeOnTerminate : Boolean;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('In threadfunc. Thread object: '+IntToStr(PTrUint(LThread))+' thread id :'+IntToStr(ptrint(Lthread.FThreadID)));{$ENDIF}
  try
    if LThread.FInitialSuspended then
      begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('thread '+IntToStr(ptruint(LThread))+' waiting for RTLEvent '+IntToStr(ptruint(LThread.FSuspendEvent)));{$ENDIF}
      RtlEventWaitFor(LThread.FSuspendEvent);
      if (LThread.FTerminated) then
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('initially created suspended, but already terminated'){$ENDIF}
      else if LThread.FSuspended then
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('thread '+IntToStr(PtrUint(LThread))+' initially created suspended, resumed, but still suspended?!'){$ENDIF}
      else
        begin
        LThread.FInitialSuspended := false;
        CurrentThreadVar := LThread;
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('going into LThread.Execute (1)');{$ENDIF}
        LThread.Execute;
        end
      end
    else
      begin
      // The suspend internal is needed due to bug 16884
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Suspending internally');{$ENDIF}
      LThread.FSuspendedInternal:=True;
      RtlEventWaitFor(LThread.FSuspendEvent);
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Internal suspend done.');{$ENDIF}
      CurrentThreadVar := LThread;
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('going into LThread.Execute (2)');{$ENDIF}
      LThread.Execute;
      end;
  except
    on e: exception do
      begin
      LThread.FFatalException := TObject(AcquireExceptionObject);
      if e is EThreadDestroyCalled then
        LThread.FFreeOnTerminate := true;
      end;
  end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('thread done running');{$ENDIF}
  Result := LThread.FReturnValue;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Result is '+IntToStr(Result));{$ENDIF}
  LFreeOnTerminate := LThread.FreeOnTerminate;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Calling doterminate');{$ENDIF}
  LThread.DoTerminate;
  LThread.FFinished := True;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Thread Set to finished');{$ENDIF}
  if LFreeOnTerminate then
    begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Thread '+IntToStr(ptruint(lthread))+' should be freed');{$ENDIF}
      LThread.Free;
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Thread freed');{$ENDIF}
    end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Thread func calling EndThread');{$ENDIF}
  EndThread(Result);
  
end;

procedure TThread.SysCreate(CreateSuspended: Boolean; const StackSize: SizeUInt);

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('In TThread.SysCreate');{$ENDIF}
  FSuspendEvent := RtlEventCreate;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysCreate: Created suspend event');{$ENDIF}
  FSuspended := CreateSuspended;
  FThreadReaped := false;
  FInitialSuspended := CreateSuspended;
  FSuspendedInternal := not CreateSuspended;
  FFatalException := nil;
  FHandle:=BeginThread(@ThreadFunc, Pointer(Self), FThreadID, StackSize);
  if FHandle = TThreadID(0) then
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysCreate: Failed to create thread');{$ENDIF}
    raise EThread.create('Failed to create new thread');
    end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysCreate: thread created');{$ENDIF}
end;


procedure TThread.SysDestroy;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: enter');{$ENDIF}
  { exception in constructor }
  if not assigned(FSuspendEvent) then
    exit;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: have suspendevent');{$ENDIF}
  { exception in constructor }
  if (FHandle = TThreadID(0)) then
    begin
    RtlEventDestroy(FSuspendEvent);
    exit;
    end;
  { Thread itself called destroy ? }
  if (FThreadID = GetCurrentThreadID) then
    begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: '+IntToStr(PtrInt(FThreadID))+' = '+IntToStr(PtrInt(GetCurrentThreadID)));{$ENDIF}
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: thread itself is freeing');{$ENDIF}
    if not(FFreeOnTerminate) and not FFinished then
       begin 
       {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: error condition');{$ENDIF}
       raise EThreadDestroyCalled.Create('A thread cannot destroy itself except by setting FreeOnTerminate and leaving!');
       end;
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: clearing FreeOnTerminate');{$ENDIF}
    FFreeOnTerminate := false;
    end
  else
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: other thread is freeing');{$ENDIF}
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: '+IntToStr(PtrInt(FThreadID))+' = '+IntToStr(PtrInt(GetCurrentThreadID)));{$ENDIF}
    { avoid recursion}
    FFreeOnTerminate := false;
    { you can't join yourself, so only for FThreadID<>GetCurrentThreadID }
    { and you can't join twice -> make sure we didn't join already       }
    if not FThreadReaped then
      begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: reaping thread');{$ENDIF}
      Terminate;
      if (FSuspendedInternal or FInitialSuspended) then
        begin
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: resuming thread in order to reap');{$ENDIF}
        Resume;
        end;
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: waiting on thread');{$ENDIF}
      // Before calling WaitFor, signal main thread with WakeMainThread, so pending checksynchronize calls are handled.
      if assigned(WakeMainThread) then
        WakeMainThread(Self);
      WaitFor;
      end;
    end;
   {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: destroying RTL suspend event');{$ENDIF}
   RtlEventDestroy(FSuspendEvent);
   {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TThread.SysDestroy: freeing fatal exception if it exists');{$ENDIF}
   FFatalException.Free;
   FFatalException := nil;
   // Free resources associated with thread. 
   // This must be done after EndThread is called, but that is called in ThreadFunc
   CloseThread(FHandle);
end;


procedure TThread.Resume;

begin
  if FSuspendedInternal and (InterLockedExchange(longint(FSuspendedInternal),ord(false)) = longint(longbool(true))) then
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('resuming thread after TThread construction '+IntToStr(ptruint(self)));{$ENDIF}
    RtlEventSetEvent(FSuspendEvent);
    end
  else
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('resuming thread '+IntToStr(ptruint(self)));{$ENDIF}
    { don't compare with ord(true) or ord(longbool(true)), }
    { becaue a longbool's "true" value is anyting <> false }
    if FSuspended and
       (InterLockedExchange(longint(FSuspended),longint(false)) <> longint(longbool(false))) then
      begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('resuming '+IntToStr(ptruint(self)));{$ENDIF}
      RtlEventSetEvent(FSuspendEvent);
      end;
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('resumed thread '+IntToStr(ptruint(self)));{$ENDIF}
    end
end;


procedure TThread.Suspend;

begin
  if FThreadID<>GetCurrentThreadID then
    Raise EThread.create('Suspending one thread from inside another one is unsupported (because it is unsafe and deadlock prone) by *nix and posix operating systems');
  { don't compare with ord(true) or ord(longbool(true)), }
  { becaue a longbool's "true" value is anyting <> false }
  if not FSuspended and
     (InterLockedExchange(longint(FSuspended),longint(longbool(true))) = longint(longbool(false))) then
     RtlEventWaitFor(FSuspendEvent)
end;


function TThread.WaitFor: Integer;

begin
  If (MainThreadID=GetCurrentThreadID) then
    {
     FFinished is set after DoTerminate, which does a synchronize of OnTerminate,
     so make sure synchronize works (or indeed any other synchronize that may be
     in progress)
    }
    While not FFinished do
      CheckSynchronize(100);
  WaitFor:=WaitForThreadTerminate(FThreadID,-1);
  { should actually check for errors in WaitForThreadTerminate, but no }
  { error api is defined for that function                             }
  FThreadReaped:=true;
end;

{$else FPC_WASM_THREADS}
procedure TThread.CallOnTerminate;

begin
end;


function TThread.GetPriority: TThreadPriority;

begin
  GetPriority:=tpNormal;
end;


procedure TThread.SetPriority(Value: TThreadPriority);

begin
end;


procedure TThread.SetSuspended(Value: Boolean);

begin
end;


procedure TThread.DoTerminate;

begin
end;


procedure TThread.SysCreate(CreateSuspended: Boolean; const StackSize: SizeUInt);

begin
 {IsMultiThread := TRUE; }
end;


procedure TThread.SysDestroy;

begin
end;


procedure TThread.Resume;

begin
end;


procedure TThread.Suspend;

begin
end;


function TThread.WaitFor: Integer;

begin
  WaitFor:=0;
end;
{$endif FPC_WASM_THREADS}

