{%MainUnit system.pp}
// In nanoseconds


Type
  // We use an alias here.
  TWasmMutex = TRTLCriticalSection;
  TMutexKind = (mkNormal,mkRecursive);
  TLockMutexResult = (lmrNone,lmrOK,lmrNotOwner,lmrError,lmrTimeout);

Function MutexKind(const M : TWasmMutex) : TMutexKind;

begin
  Result:=TMutexKind(M.Kind);
end;

procedure InitMutex(var M : TWasmMutex; aKind : TMutexKind = mkNormal; aOwner : TThreadID = Nil);

begin
  FillChar(M,SizeOf(TWasmMutex),0);
  if aOwner=Nil then
    aOwner:=GetSelfThread;
  M.Owner:=aOwner;
  M.Kind:=Ord(aKind);
end;

procedure DoneMutex(var M : TWasmMutex);

Var
  a : LongInt;

begin
  if (M.Locked>0) and (M.Owner=GetSelfThread) then
    begin
    M.Destroying:=True;
    a:=fpc_wasm32_memory_atomic_notify(@M.Locked,MaxThreadSignal);
    end;
end;

Function TryLockMutex(var M : TWasmMutex) : Boolean;

Var
  Res : Boolean;

begin
  // We already have the lock ?
  Res:=(M.Locked=1) and (M.Owner=GetSelfThread);
  if Not Res then
    Res:=fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@M.Locked,0,1)=0
  else
    begin
// TryLockMutex is called in a loop. Be VERY careful when adding this log.
//    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TryLockMutex : we ('+IntToStr(PtrUint(GetSelfThread))+') own the lock.');{$ENDIF}
    end;
  if Res then
    begin
    if (MutexKind(M)=mkRecursive) or (M.Count=0) then
      InterLockedIncrement(M.Count);
//    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TryLockMutex : setting owner to '+IntToStr(PtrUint(GetSelfThread))+'.');{$ENDIF}
    M.Owner:=GetSelfThread;
    end;
  TryLockMutex:=Res;
end;


// aTimeOutMS is in milliseconds. <0 (e.g. -1) is infinite
Function LockMutexTimeoutNoWait(var m : TWasmMutex; aTimeOutMS : Int64) : TLockMutexResult;

Var
  Res : TLockMutexResult;
  MyThread : TThreadID;
  EndTime: TOSTime;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutNoWait('+IntToStr(m.locked)+','+intToStr(aTimeOutMs)+')');{$ENDIF}
  Res:=lmrNone;
  if aTimeOutMS>=0 then
    EndTime:=GetClockTime+aTimeOutMS*1000000
  else
    EndTime:=0;
  MyThread:=GetSelfThread;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutNoWait: entering loop');{$ENDIF}
  Repeat
    if TryLockMutex(M) then
      Result:=lmrOK
    else
      begin
      If (GetThreadState(MyThread)<>tsRunning) then
        Res:=lmrError
      else
        begin
        If (aTimeOutMS>=0) and (GetClockTime>EndTime) then
          Res:=lmrTimeOut
        end;
      end;
  Until (res<>lmrNone);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutNoWait: done loop');{$ENDIF}
  LockMutexTimeoutNoWait:=Res;
end;

Function LockMutexTimeoutWait(var m : TWasmMutex; aTimeOutMS : Int64) : TLockMutexResult;

Var
  Res : TLockMutexResult;
  MyThread : TThreadID;
  EndTime: TOSTime;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutWait('+IntToStr(m.locked)+','+intToStr(aTimeOutMs)+')');{$ENDIF}
  Res:=lmrNone;
  MyThread:=GetSelfThread;
  if aTimeOutMS>=0 then
    EndTime:=GetClockTime+aTimeOutMS*1000000
  else
    EndTime:=0;
  InterLockedIncrement(M.Waiters);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutWait: entering loop');{$ENDIF}
  Repeat
    Case fpc_wasm32_memory_atomic_wait32(@M.Locked,1,1000) of
      0 : begin
          if M.Destroying then
            Res:=lmrError
          else
            Res:=lmrOK;
          end;
      1 : Res:=lmrError;
      2 : begin
          if M.Destroying then
            Res:=lmrError
          else if (GetThreadState(MyThread)<>tsRunning) then
            Res:=lmrError
          else
            begin
            If (aTimeOutMS>=0) and (GetClockTime>EndTime) then
              Res:=lmrTimeOut
            end;
          end;
    end;
  Until Res<>lmrNone;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutWait: done loop');{$ENDIF}
  InterLockedDecrement(M.Waiters);
  LockMutexTimeoutWait:=Res;
end;

Function LockMutexTimeout(var m : TWasmMutex; aTimeOutMS : Int64) : TLockMutexResult;


begin
  if TryLockMutex(M) then
    Result:=lmrOK
  else if isWaitAllowed then
    Result:=LockMutexTimeoutWait(m,aTimeOutMS)
  else
    Result:=LockMutexTimeoutNoWait(m,aTimeOutMS)
end;

Function LockMutex(var m : TRTLCriticalSection) : TLockMutexResult;

begin
  LockMutexTimeout(M,-1);
end;

function UnLockMutex(var m : TRTLCriticalSection) : TLockMutexResult;

var
  Res : TLockMutexResult;
  MyThread : TThreadID;
  EndTime: TOSTime;
  a : LongInt;

begin
  Res:=lmrNone;
  MyThread:=GetSelfThread;
  if MyThread<>M.owner then
    Res:=lmrNotOwner
  else if M.Count=0 then
    Res:=lmrError
  else
    begin
    res:=lmrOK;
    if (MutexKind(M)=mkRecursive) or (M.Count=1) then
      InterLockedDecrement(M.Count);
    if (M.Count=0) then
      begin
        M.Locked:=0;
        a:=fpc_wasm32_memory_atomic_notify(@M.Locked,1);
      end;
    end;
end;


