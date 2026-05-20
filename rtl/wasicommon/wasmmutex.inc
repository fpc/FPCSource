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

procedure InitMutex(var M : TWasmMutex; aKind : TMutexKind = mkRecursive; aOwner : TThreadID = Nil);

begin
  FillChar(M,SizeOf(TWasmMutex),0);
  if aOwner=Nil then
    aOwner:=GetSelfThread;
  M.Creator:=aOwner;
  M.Kind:=Ord(aKind);
  fpc_wasm32_i32_atomic_store(@M.Owner,0);
  fpc_wasm32_i32_atomic_store(@M.Locked,0);
end;

procedure DoneMutex(var M : TWasmMutex);

Var
  a : LongInt;

begin
  if (fpc_wasm32_i32_atomic_load(@M.Locked)<>0) and (M.Creator=GetSelfThread) then
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
  Res:=(fpc_wasm32_i32_atomic_load(@M.Locked)=1) and (TThreadID(fpc_wasm32_i32_atomic_load(@M.Owner))=GetSelfThread);
  if not Res then
    Res:=fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@M.Locked,0,1)=0
  else  
    Res:=MutexKind(M)=mkRecursive; // OK if recursive. If not recursive, then fail.
  if Res then
    begin
    // Always increase. 
    InterLockedIncrement(M.Count);
//    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('TryLockMutex : setting owner to '+IntToStr(PtrUint(GetSelfThread))+'.');{$ENDIF}
    fpc_wasm32_i32_atomic_store(@M.Owner,LongWord(GetSelfThread));
    end;
  TryLockMutex:=Res;
end;


// aTimeOutNS is in nanoseconds. <0 (e.g. -1) is infinite
Function LockMutexTimeoutNoWait(var m : TWasmMutex; aTimeOutNS : Int64) : TLockMutexResult;

Var
  Res : TLockMutexResult;
  EndTime: TOSTime;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutNoWait('+IntToStr(m.locked)+','+intToStr(aTimeOutNS)+')');{$ENDIF}
  Res:=lmrNone;
  if aTimeOutNS>=0 then
    EndTime:=GetClockTime+aTimeOutNS
  else
    EndTime:=0;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutNoWait: entering loop');{$ENDIF}
  Repeat
    if TryLockMutex(M) then
      Res:=lmrOK
    else if (aTimeOutNS>=0) and (GetClockTime>EndTime) then
      Res:=lmrTimeOut;
  Until (res<>lmrNone);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutNoWait: done loop');{$ENDIF}
  LockMutexTimeoutNoWait:=Res;
end;

Function LockMutexTimeoutWait(var m : TWasmMutex; aTimeOutNS : Int64) : TLockMutexResult;

const
  // Cap each wait32 call so parked threads periodically re-check the
  // lock even if notify is missed or delayed. 
  // This is Important on WASM running in the browser where
  // each thread is a Web Worker and a parked thread freezes its own JS
  // event loop entirely. A lock holder that needs a JS-mediated
  // round-trip through a parked worker would otherwise hang forever;
  // the wake/retry interval guarantees progress. 1ms keeps the recheck
  // cadence well inside real-time audio/video budgets (audio buffers
  // 10-20ms, 60fps frames 16.7ms) — a longer slice would risk visible
  // AV glitches. Per-cycle cost is one cheap TryLockMutex (a CAS) so
  // 1000Hz polling per parked thread is fine.
  WatchdogWaitNS = 1000000; // 1 ms


Var
  Res : TLockMutexResult;
  EndTime: TOSTime;
  RemainingTime: Int64;
  WaitSlice: Int64;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutWait('+IntToStr(m.locked)+','+intToStr(aTimeOutNS)+')');{$ENDIF}
  Res:=lmrNone;
  if aTimeOutNS>=0 then
    EndTime:=GetClockTime+aTimeOutNS
  else
    begin
      EndTime:=0;
      RemainingTime:=-1;
    end;
  InterLockedIncrement(M.Waiters);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutWait: entering loop');{$ENDIF}
  Repeat
    if TryLockMutex(m) then
      Res:=lmrOk
    else
      begin
        // No lock, determine remaining wait time.
        if aTimeOutNS>=0 then
          begin
            RemainingTime:=EndTime-GetClockTime;
            if RemainingTime<0 then
              Res:=lmrTimeOut;
          end;
        // No timeout yet, wait for signal for a max duration of WatchdogWaitNS.
        begin
          if (RemainingTime<0) or (RemainingTime>WatchdogWaitNS) then
            WaitSlice:=WatchdogWaitNS
          else
            WaitSlice:=RemainingTime;
          Case fpc_wasm32_memory_atomic_wait32(@M.Locked,1,WaitSlice) of
            0, 1, 2:
              if M.Destroying then
                Res:=lmrError;
          end;
        end;
      end;
  Until Res<>lmrNone;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('LockMutexTimeoutWait: done loop');{$ENDIF}
  InterLockedDecrement(M.Waiters);
  LockMutexTimeoutWait:=Res;
end;

Function LockMutexTimeout(var m : TWasmMutex; aTimeOutNS : Int64) : TLockMutexResult;


begin
  if TryLockMutex(M) then
    Result:=lmrOK
  else if isWaitAllowed then
    Result:=LockMutexTimeoutWait(m,aTimeOutNS)
  else
    Result:=LockMutexTimeoutNoWait(m,aTimeOutNS)
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
  if MyThread<>TThreadID(fpc_wasm32_i32_atomic_load(@M.Owner)) then
    Res:=lmrNotOwner
  else if M.Count=0 then
    Res:=lmrError
  else
    begin
    res:=lmrOK;
//    if (MutexKind(M)=mkRecursive) or (M.Count=1) then
      InterLockedDecrement(M.Count);
    if (M.Count=0) then
      begin
        fpc_wasm32_i32_atomic_store(@M.Owner,0);
        fpc_wasm32_i32_atomic_store(@M.Locked,0);
        a:=fpc_wasm32_memory_atomic_notify(@M.Locked,1);
      end;
    end;
end;


