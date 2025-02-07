{%MainUnit system.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Nikolay Nikolov,
    member of the Free Pascal development team.

    WASI threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_WASM_THREADS}
  {$fatal This file shouldn't be included if thread support is disabled!}
{$endif FPC_WASM_THREADS}

{$DEFINE FPC_WASM_MAIN_THREAD_CAN_WAIT}
{$DEFINE FPC_WASM_WORKER_THREADS_CAN_WAIT}

{//$DEFINE DEBUGWASMTHREADS}

Const
  MaxThreadSignal = high(uint32); // maximum threads to signal

Type
  TThreadState = (tsNone,tsInit,tsRunning,tsCanceling,tsExit);
  TOSTime = __wasi_timestamp_t;
// Forwards used in mutex
Function GetClockTime: TOSTime; forward;
Function IsWaitAllowed : Boolean; forward;
Function GetSelfThread : TThreadID; forward;
Function GetThreadState(aThread : TThreadID) : TThreadState; forward;

{$i wasmmem.inc}
{$i wasmmutex.inc}

Type
  PWasmRTLEvent = ^TWasmRTLEvent;
  TWasmRTLEvent = record
    Signal : Longint;
    Destroying : Boolean;
  end;

  PWasmThread = ^TWasmThread;
  TWasmThread = Record
    InitialStackPointer : Pointer;
    InitTLSBase : Pointer;
    ThreadHasFinished : Boolean;
    ID : LongInt; // Allocated by host
    ThreadFunction : TThreadFunc;
    ThreadFunctionArg : Pointer;
    State : TThreadState;
    DoneEvent : PWasmRTLEvent;
    Running : TWasmMutex;
    ExitCode : Cardinal;
    StackBlock : Pointer;
    TLSBlock : Pointer;
    StackSize : PtrUInt;
    ThreadName : Array of byte; // UTF8 name
  end;

  { EWasmThreadTerminate }

  EWasmThreadTerminate = class(TObject)
  strict private
    FExitCode : DWord;
  public
    constructor Create(AExitCode: DWord);
    property ExitCode: DWord read FExitCode;
  end;

Var
  MainThread : TWasmThread;
  WasiThreadManager : TThreadManager;
  GlobalIsWorkerThread : Longint; section 'WebAssembly.Global';
  GlobalIsMainThread : Longint; section 'WebAssembly.Global';
  GlobalIsThreadBlockable : Longint; section 'WebAssembly.Global';
  GlobalCurrentThread : PWasmThread; section 'WebAssembly.Global';

{ EWasmThreadTerminate }

constructor EWasmThreadTerminate.Create(AExitCode: DWord);
begin
  FExitCode:=AExitCode;
end;


// Forward functions


Function IsWaitAllowed : Boolean;

begin
  IsWaitAllowed:=GlobalIsThreadBlockable<>0;
end;


Function GetClockTime: TOSTime;

var
  NanoSecsPast: TOSTime;

begin
  if __wasi_clock_time_get(__WASI_CLOCKID_REALTIME,1000000,@NanoSecsPast)=__WASI_ERRNO_SUCCESS then
    GetClockTime:=NanoSecsPast
  else
    GetClockTime:=0;
end;

Function GetSelfThread : TThreadID;

begin
  GetSelfThread:=GlobalCurrentThread;
end;

Function GetMainThread : TThreadID;

begin
  Result:=PWasmThread(@MainThread);
end;

Function GetThreadState(aThread : TThreadID) : TThreadState;

begin
  GetThreadState:=PWasmThread(aThread)^.State
end;


function WasiInitManager: Boolean;
begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Initializing manager');{$ENDIF}
  FillChar(MainThread,SizeOf(MainThread),0);
  MainThread.State:=tsRunning;
  GlobalIsMainThread:=1;
  GlobalIsWorkerThread:=0;
  GlobalCurrentThread:=@MainThread;
  ThreadId:=@MainThread;
{$IFDEF FPC_WASM_MAIN_THREAD_CAN_WAIT}
  GlobalIsThreadBlockable:=1;
{$ELSE FPC_WASM_MAIN_THREAD_CAN_WAIT}
  GlobalIsThreadBlockable:=0;
{$ENDIF FPC_WASM_MAIN_THREAD_CAN_WAIT}
  InitMutex(TWasmMutex(InitialHeapCriticalSection));
  InitialHeapCriticalSectionInitialized:=true;
  if TLSInfoBlock=Nil then
    TLSInfoBlock:=AllocateOSInfoBlock;
{$IFDEF DEBUGWASMTHREADS}
  if TLSInfoBlock = Nil then
    DebugWriteln('Initializing manager done: failed');
{$ENDIF}
  WasiInitManager:=True;
end;

function WasiDoneManager: Boolean;
begin
  WasiDoneManager:=True;
end;

{ ----------------------------------------------------------------------
  Critical section (mutex)
  ----------------------------------------------------------------------}


procedure WasiInitCriticalSection(var cs);
begin
  InitMutex(TWasmMutex(CS));
end;

procedure WasiDoneCriticalSection(var cs);
begin
  DoneMutex(TWasmMutex(CS));
end;

procedure WasiEnterCriticalSection(var cs);
begin
  LockMutex(TWasmMutex(CS));
end;

function WasiCriticalSectionTryEnter(var cs):longint;
begin
  WasiCriticalSectionTryEnter:=Ord(TryLockMutex(TWasmMutex(CS)))
end;

procedure WasiLeaveCriticalSection(var cs);
begin
  UnLockMutex(TWasmMutex(CS));
end;

{ ----------------------------------------------------------------------
  RTL event
  ----------------------------------------------------------------------}


function WasiRTLCreateEvent:PRTLEvent;

Var
  P : PWasmRTLEvent;

begin
  New(P);
  fpc_wasm32_i32_atomic_store(@P^.Signal,0);
  fpc_wasm32_i32_atomic_store8(@P^.Destroying,0);
  Result:=P;
end;

procedure WasiRTLEventSetEvent(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;
  a : longint;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventSetEvent : setting signal=1');{$ENDIF}
  if fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@P^.Signal,0,1)=0 then
    begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventSetEvent : notifying 1 waiting thread');{$ENDIF}
      a:=fpc_wasm32_memory_atomic_notify(@(P^.Signal),1);
    end
  else
    begin
      {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventSetEvent : signal was already 1, nothing to do');{$ENDIF}
    end;
end;

procedure WasiRTLEventDestroy(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;
  a : LongInt;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : setting destroying to true');{$ENDIF}
  fpc_wasm32_i32_atomic_store8(@P^.Destroying,1);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : setting event to notify others');{$ENDIF}
  fpc_wasm32_i32_atomic_store(@P^.Signal,1);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : notifying waiting threads');{$ENDIF}
  a:=fpc_wasm32_memory_atomic_notify(@(P^.Signal),MaxThreadSignal);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : freeing memory');{$ENDIF}
  Dispose(P);
end;


procedure WasiRTLEventResetEvent(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventResetEvent : setting signal=0');{$ENDIF}
  fpc_wasm32_i32_atomic_store(@P^.Signal,0);
end;

procedure WasiRTLEventWaitFor_WaitAllowed(P:PWasmRTLEvent; aTimeoutNs : Int64);

Var
  a : Longint;
  EndTime: TOSTime;
  RemainingTime: Int64;

begin
  if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
    exit;  // abandoned
  if aTimeOutNS>=0 then
    EndTime:=GetClockTime+aTimeOutNS
  else
    begin
      EndTime:=0;
      RemainingTime:=-1;
    end;
  repeat
    if aTimeOutNS>=0 then
      begin
        RemainingTime:=EndTime-GetClockTime;
        if RemainingTime<0 then
          exit;  // timeout
      end;
    case fpc_wasm32_memory_atomic_wait32(@P^.Signal,0,RemainingTime) of
      0, { "ok" }
      1: { "not-equal" }
        begin
          if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
            exit  // abandoned
          else if fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@P^.Signal,1,0)=1 then
            exit  // signaled
          else
            ; { try waiting again (loop continues) }
        end;
      2: { "timed-out" }
        exit;  // timeout or abandoned
      else { invalid result from wait }
        exit;  // error
    end;
  until false;
end;


procedure WasiRTLEventWaitFor_WaitNotAllowed(P:PWasmRTLEvent; aTimeoutNs : Int64);

Var
  EndTime: TOSTime;
  RemainingTime: Int64;

begin
  if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
    exit;  // abandoned
  if aTimeOutNS>=0 then
    EndTime:=GetClockTime+aTimeOutNS
  else
    begin
      EndTime:=0;
      RemainingTime:=-1;
    end;
  repeat
    if aTimeOutNS>=0 then
      begin
        RemainingTime:=EndTime-GetClockTime;
        if RemainingTime<0 then
          exit;  // timeout
      end;
    if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
      exit  // abandoned
    else if fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@P^.Signal,1,0)=1 then
      exit;  // signaled
  until false;
end;

procedure WasiRTLEventWaitFor(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;

begin
  if IsWaitAllowed then
    WasiRTLEventWaitFor_WaitAllowed(P,-1)
  else
    WasiRTLEventWaitFor_WaitNotAllowed(P,-1);
end;

procedure WasiRTLEventWaitForTimeout(AEvent:PRTLEvent;timeout : longint);
Var
  P : PWasmRTLEvent absolute aEvent;
  TimeoutNs: Int64;

begin
  if timeout=-1 then
    TimeoutNs:=-1
  else
    TimeoutNs:=Int64(timeout)*1000000;
  if IsWaitAllowed then
    WasiRTLEventWaitFor_WaitAllowed(P,TimeoutNs)
  else
    WasiRTLEventWaitFor_WaitNotAllowed(P,TimeoutNs);
end;


{ ----------------------------------------------------------------------
  Thread
  ----------------------------------------------------------------------}


//procedure FPCWasmThreadSetStackPointer(Address: Pointer); [public, alias: 'FPC_WASM_THREAD_SET_STACK_POINTER'];
//begin
//  fpc_wasm32_set_base_pointer(Address);
//end;

// Javascript definition: TThreadInitInstanceFunction = Function(IsWorkerThread : Longint; IsMainThread : Integer; CanBlock : Integer) : Integer;

//Function FPCWasmThreadInit(IsWorkerThread : Longint; IsMainThread : Longint; CanBlock : Longint) : Longint; [public, alias: 'FPC_WASM_THREAD_INIT'];
//
//begin
//  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadInit('+IntToStr(IsWorkerThread)+','+IntToStr(IsMainThread)+','+IntToStr(CanBlock)+')');{$ENDIF}
//  GlobalIsWorkerThread:=IsWorkerThread;
//  GlobalIsMainThread:=IsMainThread;
//  GlobalIsThreadBlockable:=CanBlock;
//  Result:=0;
//end;

procedure WasiAllocateThreadVars; forward;

// Javascript definition: TThreadEntryFunction = Function(ThreadId : Longint; RunFunction : Longint; Args : LongInt) : Longint;

//Function FPCWasmThreadEntry(ThreadID : PWasmThread; RunFunction : Pointer; Args : Pointer) : Longint; [public, alias: 'FPC_WASM_THREAD_ENTRY'];
//begin
//  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry('+IntToStr(PtrUint(ThreadID))+','+IntToStr(PtrUint(RunFunction))+','+IntToStr(PtrUint(Args))+')');{$ENDIF}
//  GlobalCurrentThread:=ThreadID;
//  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry: allocating threadvars (thread function: '+intToStr(PtrUint(RunFunction))+')');{$ENDIF}
//  WasiAllocateThreadVars;
//  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry: calling initthread (thread function: '+intToStr(PtrUint(RunFunction))+')');{$ENDIF}
//  InitThread;
//  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry: calling thread function '+intToStr(PtrUint(RunFunction)));{$ENDIF}
//  Result:=tthreadfunc(RunFunction)(args);
//end;

{$push}{$S-}  // no stack checking for this procedure
procedure FPCWasmThreadStartPascal(tid: longint; start_arg: PWasmThread);
begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadStartPascal(...)');{$ENDIF}

  start_arg^.ID:=tid;
  GlobalCurrentThread:=@start_arg;
  GlobalIsMainThread:=0;
  GlobalIsWorkerThread:=1;
{$IFDEF FPC_WASM_WORKER_THREADS_CAN_WAIT}
  GlobalIsThreadBlockable:=1;
{$ELSE FPC_WASM_WORKER_THREADS_CAN_WAIT}
  GlobalIsThreadBlockable:=0;
{$ENDIF FPC_WASM_WORKER_THREADS_CAN_WAIT}

  start_arg^.State:=tsRunning;
  InitThread(start_arg^.StackSize);
  StackBottom:=start_arg^.StackBlock;
  try
    start_arg^.ExitCode:=Cardinal(start_arg^.ThreadFunction(start_arg^.ThreadFunctionArg));
  except
    on e: EWasmThreadTerminate do
      begin
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadStartPascal: Caught EWasmThreadTerminate with ExitCode='+IntToStr(e.ExitCode));{$ENDIF}
        start_arg^.ExitCode:=e.ExitCode;
      end;
    else
      begin
        {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadStartPascal: Uncaught exception');{$ENDIF}
        { TODO: what should we return here? }
        start_arg^.ExitCode:=High(Cardinal);
      end;
  end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadStartPascal: Signaling end of thread');{$ENDIF}
  WasiRTLEventSetEvent(start_arg^.DoneEvent);
end;
{$pop}

procedure wasi_thread_start(tid: longint; start_arg: PWasmThread); assembler; nostackframe;
asm
  local.get 1  ;; start_arg
  i32.load     ;; load InitialStackPointer
  global.set $__stack_pointer

  ;; call fpc_wasm32_init_tls from within assembly code, because in branchful
  ;; exceptions mode, Free Pascal generates threadvar access after every
  ;; function call. Therefore, we want threadvars to be initialized, before we
  ;; call any sort of Pascal code.
  local.get 1  ;; start_arg
  i32.const 4  ;; offset to InitTLSBase
  i32.add
  i32.load
  call $fpc_wasm32_init_tls

  local.get 0  ;; tid
  local.get 1  ;; start_arg
  call $FPCWasmThreadStartPascal

  ;; Set start_arg^.ThreadHasFinished to true.
  ;; This is done from within inline asm, after the pascal code has finished
  ;; executing, because it indicates that the thread no longer needs its TLS
  ;; block and linear stack block, so this means it's safe to free them.
  local.get 1  ;; start_arg
  i32.const 8  ;; offset to ThreadHasFinished
  i32.add
  i32.const 1  ;; true
  i32.atomic.store8
end;

exports wasi_thread_start, GetSelfThread, GetMainThread;

Function wasi_thread_spawn(start_arg: PWasmThread) : LongInt; external 'wasi' name 'thread-spawn';

{ Just because we set the original pointer to nil, using InterlockedExchange
  to avoid race conditions leading to double free, doesn't mean this function is
  meant to be called more than once, or from multiple threads. This just adds
  some extra layer of protection. }
procedure FreeStackAndTlsBlock(T : PWasmThread);
var
  P: Pointer;
begin
  P:=InterlockedExchange(T^.StackBlock,nil);
  if Assigned(P) then
    FreeMem(P);
  P:=InterlockedExchange(T^.TLSBlock,nil);
  if Assigned(P) then
    FreeMem(P);
end;

function WasiBeginThread(sa : Pointer;stacksize : PtrUInt; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword; var ThreadId : TThreadID) : TThreadID;

Const
  HeapAlignment=16;
Var
  T : PWasmThread;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread(sa: '+IntToStr(PtrUint(Sa))+',ss: '+IntToStr(PtrUint(StackSize))+',TF: '+IntToStr(PtrUint(ThreadFunction))+',Arg: '+IntToStr(PtrUint(P))+',fl: '+IntToStr(PtrUint(CreationFlags))+',ID: '+IntToStr(PtrUint(ThreadID))+')');{$ENDIF}
  IsMultiThread:=true;
  New(T);
  fpc_wasm32_i32_atomic_store8(@T^.ThreadHasFinished,0);
  T^.StackBlock:=nil;
  T^.TLSBlock:=nil;
  ThreadID:=T;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread thread ID : '+IntToStr(PtrUint(ThreadID)));{$ENDIF}
  T^.ThreadFunction:=ThreadFunction;
  T^.ThreadFunctionArg:=p;
  if stacksize<=0 then
    stacksize:=StkLen;
  T^.StackSize:=stacksize;
  T^.StackBlock:=GetMem(stacksize);
  T^.InitialStackPointer:=Pointer(PtrUInt(PtrUInt(T^.StackBlock)+stacksize) and $FFFFFFF0);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: InitialStackPointer='+IntToStr(PtrUint(T^.InitialStackPointer)));{$ENDIF}
  T^.TLSBlock:=AllocMem(fpc_wasm32_tls_size+fpc_wasm32_tls_align-1);
  T^.InitTLSBase:=Align(T^.TLSBlock,fpc_wasm32_tls_align);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: InitTLSBase='+IntToStr(PtrUint(T^.InitTLSBase)));{$ENDIF}
  InitMutex(T^.Running,mkNormal);
  T^.DoneEvent:=WasiRTLCreateEvent;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: Locked mutex');{$ENDIF}
  if wasi_thread_spawn(T)>0 then
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread OK, setting result');{$ENDIF}
    WasiBeginThread:=T;
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread OK, done setting result');{$ENDIF}
    end
  else
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread failed');{$ENDIF}
    WasiRTLEventDestroy(T^.DoneEvent);
    DoneMutex(T^.Running);
    FreeStackAndTlsBlock(T);
    Dispose(T);
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread failed, freeing thread struct');{$ENDIF}
    WasiBeginThread:=TThreadID(0);
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread failed, returning 0');{$ENDIF}
    end
end;

procedure WasiEndThread(ExitCode : DWord);

Var
  T : PWasmThread;
begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('EndThread('+IntToStr(ExitCode)+')');{$ENDIF}
  raise EWasmThreadTerminate.Create(ExitCode);
end;

function WasiSuspendThread(threadHandle : TThreadID) : dword;
begin
  WasiSuspendThread:=DWord(-1);
end;

function WasiResumeThread(threadHandle : TThreadID) : dword;
begin
  WasiResumeThread:=DWord(-1);
end;

function WasiKillThread(threadHandle : TThreadID) : dword;
begin
  WasiKillThread:=DWord(-1);
end;

function WasiCloseThread(threadHandle : TThreadID) : dword;
begin
  Result:=0;
end;

procedure WasiThreadSwitch;
begin
  // Normally a yield, but this does not (yet) exist in webassembly.
end;

function WasiWaitForThreadTerminate(threadHandle : TThreadID; TimeoutMs : longint) : dword;

Var
  Res : LongInt;
  TH : PWasmThread absolute ThreadHandle;
  TimeoutNs : Int64;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+','+IntToStr(TimeoutMs)+')');{$ENDIF}
  if TimeoutMs>=0 then
    TimeoutNs:=TimeoutMs*1000000
  else
    TimeoutNs:=-1;
  WasiRTLEventWaitFor(TH^.DoneEvent);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Event set, waiting for lock');{$ENDIF}
  Case LockMuTexTimeout(PWasmThread(ThreadHandle)^.Running,TimeoutNs) of
    lmrOK : Res:=LongInt(TH^.ExitCode);
    lmrError : Res:=-2;
  else
     Res:=-1;
  end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Got Lock');{$ENDIF}
  UnLockMuTex(PWasmThread(ThreadHandle)^.Running);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Lock released');{$ENDIF}
  WasiWaitForThreadTerminate:=DWord(Res);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Waiting until ThreadHasFinished becomes true');{$ENDIF}
  repeat
  until fpc_wasm32_i32_atomic_load8_u(@TH^.ThreadHasFinished)<>0;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : FreeStackAndTlsBlock');{$ENDIF}
  FreeStackAndTlsBlock(TH);
end;

function WasiThreadSetPriority(threadHandle : TThreadID; Prio: longint): boolean;
begin
  Result:=False;
end;

function WasiThreadGetPriority(threadHandle : TThreadID): longint;
begin
  Result:=0;
end;

function WasiGetCurrentThreadId : TThreadID;
begin
  Result:=GetSelfThread;
end;

procedure WasiThreadSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);

Var
  P : PWasmThread absolute ThreadHandle;
  Len : Integer;
begin
  Len:=Length(ThreadName);
  SetLength(P^.ThreadName,Len);
  if Len>0 then
    Move(ThreadName[1],P^.ThreadName[0],Len);
end;

{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
procedure WasiThreadSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);

Var
  P : PWasmThread absolute ThreadHandle;
  LThreadName : RawBytestring;
  Len : Integer;
begin
  Len:=Length(LThreadName);
  LThreadName:=Utf8Encode(ThreadName);
  SetLength(P^.ThreadName,Len*SizeOf(UnicodeChar));
  if Len>0 then
    Move(LThreadName[1],P^.ThreadName[0],Len*SizeOf(UnicodeChar));
end;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}


{ ----------------------------------------------------------------------
  Threadvars
  ----------------------------------------------------------------------}


Var
  threadvarblocksize : PtrUint;

procedure WasiInitThreadVar(var offset : dword;size : dword);
begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiInitThreadVar('+IntToStr(offset)+','+IntToStr(size)+')');{$ENDIF}
  threadvarblocksize:=align(threadvarblocksize, fpc_wasm32_tls_align);
  offset:=threadvarblocksize;
  inc(threadvarblocksize,size);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Done WasiInitThreadVar. Total size: '+IntToStr(threadvarblocksize));{$ENDIF}
end;


procedure WasiAllocateThreadVars;

var
  tlsMemBlock : pointer;
  tlsBlockSize : Integer;
  P : POSMemBlock;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiAllocateThreadVars');{$ENDIF}
  tlsBlockSize:=fpc_wasm32_tls_size;
  if threadvarblocksize<>tlsBlocksize then
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Warning : block sizes differ: (linker) '+IntToStr(tlsBlocksize)+'<>'+IntToStr(threadvarblocksize)+' (calculated) !');{$ENDIF}
  P:=GetFreeOSBlock;
  FillChar((P^.Data)^.TLSMemory,tlsBlockSize,0);
  fpc_wasm32_init_tls(@((P^.Data)^.TLSMemory));
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('Done WasiAllocateThreadVars');{$ENDIF}
end;

Function GetTLSMemory : Pointer;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetTLSMemory Enter');{$ENDIF}
  GetTLSMemory:=fpc_wasm32_tls_base();
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('GetTLSMemory exit: '+InttoStr(PtrUint(fpc_wasm32_tls_base())));{$ENDIF}
end;

procedure WasiReleaseThreadVars;

Var
  PTLS : PTLSMem;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiReleaseThreadVars');{$ENDIF}
  PTLS:=GetTLSMemory-Sizeof(Pointer);
  ReleaseOSBlock(PTLS^.OSMemBlock);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiReleaseThreadVars done');{$ENDIF}
end;

procedure HookThread;
{ Set up externally created thread }
begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('HookThread');{$ENDIF}
  WasiAllocateThreadVars;
  InitThread(1000000000);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('HookThread done');{$ENDIF}
end;

function WasiRelocateThreadVar(offset : dword) : pointer;
var
  P : Pointer;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRelocateThreadVar ('+IntToStr(offset)+')');{$ENDIF}
  P:=GetTLSMemory;
  if (P=Nil) then
    begin
    HookThread;
    P:=GetTLSMemory;
    end;
  WasiRelocateThreadvar:=P+Offset;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRelocateThreadVar done. Result: '+IntToStr(PtrUint(P+Offset)));{$ENDIF}
end;

{ ----------------------------------------------------------------------
  Basic event
  ----------------------------------------------------------------------}

const
  wrSignaled  = 0;
  wrTimeout   = 1;
  wrAbandoned = 2;
  wrError     = 3;

type
  PWasmBasicEventState = ^TWasmBasicEventState;
  TWasmBasicEventState = record
    Signal : Longint;
    ManualReset : Boolean;
    Destroying : Boolean;
  end;

function WasiBasicEventCreate(EventAttributes :Pointer;  AManualReset,InitialState : Boolean;const Name:ansistring):pEventState;
var
  P: PWasmBasicEventState;
begin
  New(P);
  fpc_wasm32_i32_atomic_store(@P^.Signal,Ord(InitialState));
  fpc_wasm32_i32_atomic_store8(@P^.ManualReset,Ord(AManualReset));
  fpc_wasm32_i32_atomic_store8(@P^.Destroying,0);
  Result:=P;
end;

procedure WasiBasicEventDestroy(state:peventstate);
var
  P: PWasmBasicEventState absolute state;
  a: longword;
begin
  fpc_wasm32_i32_atomic_store8(@P^.Destroying,1);
  fpc_wasm32_i32_atomic_store(@P^.Signal,1);
  a:=fpc_wasm32_memory_atomic_notify(@(P^.Signal),MaxThreadSignal);
  Dispose(P);
end;

procedure WasiBasicEventResetEvent(state:peventstate);
var
  P: PWasmBasicEventState absolute state;
begin
  fpc_wasm32_i32_atomic_store(@P^.Signal,0);
end;

procedure WasiBasicEventSetEvent(state:peventstate);
var
  P: PWasmBasicEventState absolute state;
  a: longword;
begin
  if fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@P^.Signal,0,1)=0 then
    begin
      if fpc_wasm32_i32_atomic_load8_u(@P^.ManualReset)<>0 then
        a:=fpc_wasm32_memory_atomic_notify(@(P^.Signal),MaxThreadSignal)
      else
        a:=fpc_wasm32_memory_atomic_notify(@(P^.Signal),1);
    end;
end;

function WasiBasicEventWaitFor_WaitAllowed(aTimeOutNS:Int64;P:PWasmBasicEventState):longint;
var
  EndTime: TOSTime;
  RemainingTime: Int64;
begin
  if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
    begin
      result:=wrAbandoned;
      exit;
    end;
  if fpc_wasm32_i32_atomic_load8_u(@P^.ManualReset)<>0 then
    begin
      { manual reset event }
      case fpc_wasm32_memory_atomic_wait32(@P^.Signal,0,aTimeOutNS) of
        0, 1:
          result:=wrSignaled;
        2:
          result:=wrTimeout;
        else
          result:=wrError;
      end;
      if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
        result:=wrAbandoned;
    end
  else
    begin
      { auto reset event }
      if aTimeOutNS>=0 then
        EndTime:=GetClockTime+aTimeOutNS
      else
        begin
          EndTime:=0;
          RemainingTime:=-1;
        end;
      repeat
        if aTimeOutNS>=0 then
          begin
            RemainingTime:=EndTime-GetClockTime;
            if RemainingTime<0 then
              begin
                result:=wrTimeout;
                exit;
              end;
          end;
        case fpc_wasm32_memory_atomic_wait32(@P^.Signal,0,RemainingTime) of
          0, { "ok" }
          1: { "not-equal" }
            begin
              if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
                begin
                  result:=wrAbandoned;
                  exit;
                end
              else if fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@P^.Signal,1,0)=1 then
                begin
                  if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
                    begin
                      fpc_wasm32_i32_atomic_store(@P^.Signal,1);
                      result:=wrAbandoned;
                      exit;
                    end
                  else
                    begin
                      result:=wrSignaled;
                      exit;
                    end;
                end
              else
                ; { try waiting again (loop continues) }
            end;
          2: { "timed-out" }
            if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
              begin
                result:=wrAbandoned;
                exit;
              end
            else
              begin
                result:=wrTimeout;
                exit;
              end;
          else { invalid result from wait }
            begin
              result:=wrError;
              exit;
            end;
        end;
      until false;
    end;
end;

function WasiBasicEventWaitFor_WaitNotAllowed(aTimeOutNS:Int64;P:PWasmBasicEventState):longint;
var
  EndTime: TOSTime;
  RemainingTime: Int64;
begin
  if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
    begin
      result:=wrAbandoned;
      exit;
    end;
  if aTimeOutNS>=0 then
    EndTime:=GetClockTime+aTimeOutNS
  else
    begin
      EndTime:=0;
      RemainingTime:=-1;
    end;
  if fpc_wasm32_i32_atomic_load8_u(@P^.ManualReset)<>0 then
    begin
      { manual reset event }
      repeat
        if aTimeOutNS>=0 then
          begin
            RemainingTime:=EndTime-GetClockTime;
            if RemainingTime<0 then
              begin
                result:=wrTimeout;
                exit;
              end;
          end;
        if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
          begin
            result:=wrAbandoned;
            exit;
          end
        else if fpc_wasm32_i32_atomic_load(@P^.Signal)<>0 then
          begin
            if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
              begin
                result:=wrAbandoned;
                exit;
              end
            else
              begin
                result:=wrSignaled;
                exit;
              end;
          end;
      until false;
    end
  else
    begin
      { auto reset event }
      repeat
        if aTimeOutNS>=0 then
          begin
            RemainingTime:=EndTime-GetClockTime;
            if RemainingTime<0 then
              begin
                result:=wrTimeout;
                exit;
              end;
          end;
        if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
          begin
            result:=wrAbandoned;
            exit;
          end
        else if fpc_wasm32_i32_atomic_rmw_cmpxchg_u(@P^.Signal,1,0)=1 then
          begin
            if fpc_wasm32_i32_atomic_load8_u(@P^.Destroying)<>0 then
              begin
                fpc_wasm32_i32_atomic_store(@P^.Signal,1);
                result:=wrAbandoned;
                exit;
              end
            else
              begin
                result:=wrSignaled;
                exit;
              end;
          end;
      until false;
    end;
end;

function WasiBasicEventWaitFor(timeout:cardinal;state:peventstate;FUseComWait : Boolean=False):longint;
var
  timeoutNS: Int64;
begin
  if timeout<>$FFFFFFFF then
    timeoutNS:=timeout*1000000
  else
    timeoutNS:=-1;
  if isWaitAllowed then
      Result:=WasiBasicEventWaitFor_WaitAllowed(timeoutNS,PWasmBasicEventState(state))
    else
      Result:=WasiBasicEventWaitFor_WaitNotAllowed(timeoutNS,PWasmBasicEventState(state));
end;


procedure InitSystemThreads;public name '_FPC_InitSystemThreads';
begin
  with WasiThreadManager do
    begin
      InitManager            := @WasiInitManager;
      DoneManager            := @WasiDoneManager;
      BeginThread            := @WasiBeginThread;
      EndThread              := @WasiEndThread;
      SuspendThread          := @WasiSuspendThread;
      ResumeThread           := @WasiResumeThread;
      KillThread             := @WasiKillThread;
      CloseThread            := @WasiCloseThread;
      ThreadSwitch           := @WasiThreadSwitch;
      WaitForThreadTerminate := @WasiWaitForThreadTerminate;
      ThreadSetPriority      := @WasiThreadSetPriority;
      ThreadGetPriority      := @WasiThreadGetPriority;
      GetCurrentThreadId     := @WasiGetCurrentThreadId;
      SetThreadDebugNameA    := @WasiThreadSetThreadDebugNameA;
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
      SetThreadDebugNameU    := @WasiThreadSetThreadDebugNameU;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}
      InitCriticalSection    := @WasiInitCriticalSection;
      DoneCriticalSection    := @WasiDoneCriticalSection;
      EnterCriticalSection   := @WasiEnterCriticalSection;
      TryEnterCriticalSection:= @WasiCriticalSectionTryEnter;
      LeaveCriticalSection   := @WasiLeaveCriticalSection;
      InitThreadVar          := @WasiInitThreadVar;
      RelocateThreadVar      := @WasiRelocateThreadVar;
      AllocateThreadVars     := @WasiAllocateThreadVars;
      ReleaseThreadVars      := @WasiReleaseThreadVars;
      BasicEventCreate       := @WasiBasicEventCreate;
      BasicEventDestroy      := @WasiBasicEventDestroy;
      BasicEventResetEvent   := @WasiBasicEventResetEvent;
      BasicEventSetEvent     := @WasiBasicEventSetEvent;
      BasiceventWaitFOr      := @WasiBasicEventWaitFor;
      RTLEventCreate         := @WasiRTLCreateEvent;
      RTLEventDestroy        := @WasiRTLEventDestroy;
      RTLEventSetEvent       := @WasiRTLEventSetEvent;
      RTLEventResetEvent     := @WasiRTLEventResetEvent;
      RTLEventWaitFor        := @WasiRTLEventWaitFor;
      RTLEventWaitForTimeout := @WasiRTLEventWaitForTimeout;
    end;
  SetThreadManager(WasiThreadManager);
end;
