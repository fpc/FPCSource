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

{$DEFINE DEBUGWASMTHREADS}

Const
  MaxThreadSignal = 1000; // maximum threads to signal

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
    mutex: TWasmMutex;
    Destroying : Boolean;
  end;

  PWasmThread = ^TWasmThread;
  TWasmThread = Record
    ID : LongInt; // Allocated by host javascript code
    State : TThreadState;
    DoneEvent : PWasmRTLEvent;
    Running : TWasmMutex;
    ExitCode : Cardinal;
    ThreadName : Array of byte; // UTF8 name
  end;

Var
  MainThread : TWasmThread;
  WasiThreadManager : TThreadManager;
  GlobalIsWorkerThread : Longint; section 'WebAssembly.Global';
  GlobalIsMainThread : Longint; section 'WebAssembly.Global';
  GlobalIsThreadBlockable : Longint; section 'WebAssembly.Global';
  GlobalCurrentThread : PWasmThread; section 'WebAssembly.Global';



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

Function GetThreadState(aThread : TThreadID) : TThreadState;

begin
  GetThreadState:=PWasmThread(aThread)^.State
end;


function WasiInitManager: Boolean;
begin
  DebugWriteln('Initializing manager');
  if TLSInfoBlock=Nil then
    TLSInfoBlock:=AllocateOSInfoBlock;
  if TLSInfoBlock = Nil then
    DebugWriteln('Initializing manager done: failed');
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
  P^.Signal:=0;
  P^.Destroying:=False;
  InitMutex(P^.Mutex);
  Result:=P;
end;

procedure WasiRTLEventSetEvent(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;
  a : longint;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventSetEvent : locking mutex');{$ENDIF}
  LockMutex(P^.Mutex);
  P^.Signal:=1;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventSetEvent : unlocking mutex');{$ENDIF}
  UnLockMutex(P^.Mutex);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventSetEvent : send signal');{$ENDIF}
  a:=fpc_wasm32_memory_atomic_notify(@(P^.Signal),MaxThreadSignal);
end;

procedure WasiRTLEventDestroy(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : locking mutex');{$ENDIF}
  LockMutex(P^.Mutex);
  P^.Destroying:=True;
  UnlockMutex(P^.Mutex);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : setting event to notify others');{$ENDIF}
  WasiRTLEventSetEvent(aEvent);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventDestroy : set event to notify others');{$ENDIF}
  FreeMem(P);
end;


procedure WasiRTLEventResetEvent(AEvent:PRTLEvent);

Var
  P : PWasmRTLEvent absolute aEvent;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventResetEvent : locking mutex');{$ENDIF}
  LockMutex(P^.Mutex);
  P^.Signal:=0;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventResetEvent : unlocking mutex');{$ENDIF}
  UnLockMutex(P^.Mutex);
end;

procedure WasiRTLEventWaitFor_WaitAllowed(AEvent:PWasmRTLEvent; aTimeoutNs : Int64);

Var
  a : Longint;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventWaitFor_WaitAllowed : waiting');{$ENDIF}
  a:=fpc_wasm32_memory_atomic_wait32(@(aEvent^.Signal),0,aTimeoutNs);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventWaitFor_WaitAllowed : done');{$ENDIF}
end;


procedure WasiRTLEventWaitFor_WaitNotAllowed(AEvent:PWasmRTLEvent; aTimeoutNs : Int64);

Var
  EndTime : Int64;
  IsTimeOut : Boolean;
  IsDone : Boolean;
  isMain : Boolean;

begin
  IsMain:=GlobalIsMainThread<>0;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventWaitFor_WaitNotAllowed : waiting (is main: '+intToStr(Ord(IsMain))+')');{$ENDIF}
  if aTimeoutNs>0 then
    EndTime:=GetClockTime+aTimeoutNs
  else
    EndTime:=0;
  Repeat
    IsTimeOut:=(aTimeoutNs>0) and (GetClockTime>EndTime);
    IsDone:=(aEvent^.Signal=1) or (aEvent^.Destroying) or (Not IsMain and (GetThreadState(GetSelfThread)<>tsRunning));
  Until isTimeOut or IsDone;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiRTLEventWaitFor_WaitNotAllowed : done waiting');{$ENDIF}
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


procedure FPCWasmThreadSetStackPointer(Address: Pointer); [public, alias: 'FPC_WASM_THREAD_SET_STACK_POINTER'];
begin
  fpc_wasm32_set_base_pointer(Address);
end;

// Javascript definition: TThreadInitInstanceFunction = Function(IsWorkerThread : Longint; IsMainThread : Integer; CanBlock : Integer) : Integer;

Function FPCWasmThreadInit(IsWorkerThread : Longint; IsMainThread : Longint; CanBlock : Longint) : Longint; [public, alias: 'FPC_WASM_THREAD_INIT'];

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadInit('+IntToStr(IsWorkerThread)+','+IntToStr(IsMainThread)+','+IntToStr(CanBlock)+')');{$ENDIF}
  GlobalIsWorkerThread:=IsWorkerThread;
  GlobalIsMainThread:=IsMainThread;
  GlobalIsThreadBlockable:=CanBlock;
  Result:=0;
end;

procedure WasiAllocateThreadVars; forward;

// Javascript definition: TThreadEntryFunction = Function(ThreadId : Longint; RunFunction : Longint; Args : LongInt) : Longint;

Function FPCWasmThreadEntry(ThreadID : PWasmThread; RunFunction : Pointer; Args : Pointer) : Longint; [public, alias: 'FPC_WASM_THREAD_ENTRY'];
begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry('+IntToStr(PtrUint(ThreadID))+','+IntToStr(PtrUint(RunFunction))+','+IntToStr(PtrUint(Args))+')');{$ENDIF}
  GlobalCurrentThread:=ThreadID;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry: allocating threadvars (thread function: '+intToStr(PtrUint(RunFunction))+')');{$ENDIF}
  WasiAllocateThreadVars;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry: calling initthread (thread function: '+intToStr(PtrUint(RunFunction))+')');{$ENDIF}
  InitThread;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('FPCWasmThreadEntry: calling thread function '+intToStr(PtrUint(RunFunction)));{$ENDIF}
  Result:=tthreadfunc(RunFunction)(args);
end;

exports FPCWasmThreadSetStackPointer, FPCWasmThreadInit, FPCWasmThreadEntry;

Function thread_spawn(thread_id : PInteger; attrs: Pointer; thread_start_func : Pointer; args : Pointer) : LongInt;  external 'FPCThreading' name 'thread_spawn';

function WasiBeginThread(sa : Pointer;stacksize : PtrUInt; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword; var ThreadId : TThreadID) : TThreadID;

Var
  T : PWasmThread;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread(sa: '+IntToStr(PtrUint(Sa))+',ss: '+IntToStr(PtrUint(StackSize))+',TF: '+IntToStr(PtrUint(ThreadFunction))+',Arg: '+IntToStr(PtrUint(P))+',fl: '+IntToStr(PtrUint(CreationFlags))+',ID: '+IntToStr(PtrUint(ThreadID))+')');{$ENDIF}
  T:=GetMem(SizeOf(TWasmThread));
  ThreadID:=T;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread thread ID : '+IntToStr(PtrUint(ThreadID)));{$ENDIF}
  InitMutex(T^.Running,mkNormal);
  T^.DoneEvent:=WasiRTLCreateEvent;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: Locked mutex');{$ENDIF}
  if thread_spawn(@(T^.ID),Nil,ThreadFunction,P)=0 then
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread OK, setting result');{$ENDIF}
    WasiBeginThread:=T;
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread OK, done setting result');{$ENDIF}
    end
  else
    begin
    {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WasiBeginThread: spawn thread failed');{$ENDIF}
    FreeMem(T);
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
  T:=PWasmThread(GetSelfThread);
  T^.ExitCode:=ExitCode;
  // Signal that we're done
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('EndThread: Signaling end of thread');{$ENDIF}
  WasiRTLEventSetEvent(T^.DoneEvent);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('EndThread: Unlocking mutex');{$ENDIF}
  // Now unlock running mutex
  UnlockMutex(T^.Running);
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
  {todo:implement}
end;

function WasiWaitForThreadTerminate(threadHandle : TThreadID; TimeoutMs : longint) : dword;

Var
  Res : Integer;
  TH : PWasmThread absolute ThreadHandle;

begin
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+','+IntToStr(TimeoutMs)+')');{$ENDIF}
  WasiRTLEventWaitFor(TH^.DoneEvent);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Event set, waiting for lock');{$ENDIF}
  Case LockMuTexTimeout(PWasmThread(ThreadHandle)^.Running,TimeoutMS) of
    lmrOK : Res:=0;
    lmrError : Res:=-2;
  else
     Res:=-1;
  end;
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Got Lock');{$ENDIF}
  UnLockMuTex(PWasmThread(ThreadHandle)^.Running);
  {$IFDEF DEBUGWASMTHREADS}DebugWriteln('WaitForThreadTerminate('+IntToStr(PtrUINT(TH))+') : Lock released');{$ENDIF}
  WasiWaitForThreadTerminate:=DWord(Res);
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



function WasiBasicEventCreate(EventAttributes :Pointer;  AManualReset,InitialState : Boolean;const Name:ansistring):pEventState;
begin
  {todo:implement}
end;

procedure WasiBasicEventDestroy(state:peventstate);
begin
  {todo:implement}
end;

procedure WasiBasicEventResetEvent(state:peventstate);
begin
  {todo:implement}
end;

procedure WasiBasicEventSetEvent(state:peventstate);
begin
  {todo:implement}
end;

function WasiBasicEventWaitFor(timeout:cardinal;state:peventstate;FUseComWait : Boolean=False):longint;
begin
  {todo:implement}
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
