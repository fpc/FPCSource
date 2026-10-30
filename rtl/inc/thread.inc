{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2000 by the Free Pascal development team

    OS independent thread functions/overloads

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Var
  CurrentTM : TThreadManager;
{$ifndef THREADVAR_RELOCATED_ALREADY_DEFINED}
  fpc_threadvar_relocate_proc : TRelocateThreadVarHandler; public name 'FPC_THREADVAR_RELOCATE';
{$endif THREADVAR_RELOCATED_ALREADY_DEFINED}

{$ifndef HAS_GETCPUCOUNT}
    function GetCPUCount: LongWord;
      begin
        Result := 1;
      end;
{$endif}


{*****************************************************************************
                           Threadvar initialization
*****************************************************************************}

    procedure InitThread(stklen:SizeUInt);
      begin
{$ifndef FPUNONE}
        SysInitFPU;
        { the call to SysResetFPU should not be needed here, but SysInitFPU is not properly implemented for all CPUs yet }
        SysResetFPU;
{$endif}
{$ifndef HAS_MEMORYMANAGER}
        { initialize this thread's heap }
        InitHeapThread;
{$endif ndef HAS_MEMORYMANAGER}
        if MemoryManager.InitThread <> nil then
          MemoryManager.InitThread();
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
        if assigned(widestringmanager.ThreadInitProc) then
          widestringmanager.ThreadInitProc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
        { ExceptAddrStack and ExceptObjectStack are threadvars       }
        { so every thread has its on exception handling capabilities }
        SysInitExceptions;
{$endif FPC_HAS_FEATURE_EXCEPTIONS}
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
{$ifndef EMBEDDED}
        { Open all stdio fds again }
        SysInitStdio;
        InOutRes:=0;
        // ErrNo:=0;
{$endif EMBEDDED}
{$endif FPC_HAS_FEATURE_CONSOLEIO}
{$ifdef FPC_HAS_FEATURE_STACKCHECK}
        { Stack checking }
        StackLength:= CheckInitialStkLen(stkLen);
        StackBottom:=Sptr - StackLength;
{$endif FPC_HAS_FEATURE_STACKCHECK}
        ThreadID := CurrentTM.GetCurrentThreadID();
      end;

    procedure DoneThread;
      begin
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
        if assigned(widestringmanager.ThreadFiniProc) then
          widestringmanager.ThreadFiniProc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
        if MemoryManager.DoneThread <> nil then
          MemoryManager.DoneThread();
{$ifndef HAS_MEMORYMANAGER}
        FinalizeHeap;
{$endif ndef HAS_MEMORYMANAGER}
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
        { Open all stdio fds again }
        SysFlushStdio;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
        { Support platforms where threadvar memory is managed outside of the RTL:
          reset ThreadID and allow ReleaseThreadVars to be unassigned }
        ThreadID := TThreadID(0);
        if assigned(CurrentTM.ReleaseThreadVars) then
          CurrentTM.ReleaseThreadVars;
      end;


    procedure InitThread;
      begin
        { we should find a reasonable value here }
        InitThread($1000000);
      end;

{*****************************************************************************
                            Overloaded functions
*****************************************************************************}

    function BeginThread(ThreadFunction : tthreadfunc) : TThreadHandle;
      var
        dummy : TThreadID;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,nil,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : TThreadHandle;
      var
        dummy : TThreadID;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : TThreadID) : TThreadHandle;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,ThreadId);
      end;

    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
                     var ThreadId : TThreadID; const stacksize: SizeUInt) : TThreadHandle;
      begin
        BeginThread:=BeginThread(nil,stacksize,ThreadFunction,p,0,ThreadId);
      end;

type
  PSuspendedStart = ^TSuspendedStart;
  TSuspendedStart = record
    Next : PSuspendedStart;
    Handle : TThreadHandle;
    Func : TThreadFunc; { Reset to nil to signal it shouldn’t run. }
    Param : Pointer;
    StartEvent : PRTLEvent;
  end;

const
  SuspendedStartList : PSuspendedStart = nil;
  SuspendedStartLock : longint = 0;

{ Acquire the spinlock protecting SuspendedStartList. }
procedure LockSuspendedStarts;

begin
  while InterlockedExchange(SuspendedStartLock,1)<>0 do
    ThreadSwitch;
end;


{ Release the spinlock protecting SuspendedStartList. }
procedure UnlockSuspendedStarts;

begin
  InterlockedExchange(SuspendedStartLock,0);
end;


{ Searches and unregisters the suspended start of aHandle (returns true if found).
  Sets its StartEvent, and if “cancel”, signals it to skip the user body (used by Kill/CloseThread). }
function PulseSuspendedStart(aHandle : TThreadHandle; cancel : boolean) : boolean;

var
  lPrev,lThis : PSuspendedStart;

begin
  Result:=false;
  if SuspendedStartList=nil then
    exit;
  LockSuspendedStarts;
  lPrev:=nil;
  lThis:=SuspendedStartList;
  while assigned(lThis) and (lThis^.Handle<>aHandle) do
    begin
    lPrev:=lThis;
    lThis:=lThis^.Next;
    end;
  if assigned(lThis) then
    begin
    if assigned(lPrev) then
      lPrev^.Next:=lThis^.Next
    else
      SuspendedStartList:=lThis^.Next;
    end;
  UnlockSuspendedStarts;
  if assigned(lThis) then
    begin
    Result:=true;
    if cancel then
      lThis^.Func:=nil;
    RTLEventSetEvent(lThis^.StartEvent);
    end;
end;


{ Thread function of a suspended thread: waits for its start event, then runs the user function. }
function SuspendedStartMain(aStart : pointer) : ptrint;

var
  lFunc : TThreadFunc;
  lParam : Pointer;

begin
  RTLEventWaitFor(PSuspendedStart(aStart)^.StartEvent);
  RTLEventDestroy(PSuspendedStart(aStart)^.StartEvent);
  lFunc:=PSuspendedStart(aStart)^.Func;
  lParam:=PSuspendedStart(aStart)^.Param;
  Dispose(PSuspendedStart(aStart));
  Result:=0;
  if assigned(lFunc) then
    Result:=lFunc(lParam);
end;


{ Start a thread that waits in SuspendedStartMain until ResumeThread is called on the returned handle. }
function BeginSuspendedThread(sa : Pointer;stacksize : SizeUInt; ThreadFunction : tthreadfunc;p : pointer;
  creationFlags : dword; var ThreadId : TThreadID) : TThreadHandle;

var
  lStart : PSuspendedStart;

begin
  New(lStart);
  lStart^.Func:=ThreadFunction;
  lStart^.Param:=p;
  lStart^.StartEvent:=RTLEventCreate;
  Result:=CurrentTM.BeginThread(sa,stacksize,@SuspendedStartMain,lStart,creationFlags,ThreadId);
  if Result=TThreadHandle(0) then
    begin
    RTLEventDestroy(lStart^.StartEvent);
    Dispose(lStart);
    exit;
    end;
  lStart^.Handle:=Result;
  LockSuspendedStarts;
  lStart^.Next:=SuspendedStartList;
  SuspendedStartList:=lStart;
  UnlockSuspendedStarts;
end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : TThreadID;
                     Suspended : Boolean; const stacksize: SizeUInt) : TThreadHandle;
      begin
        if Suspended then
          Result:=BeginSuspendedThread(nil,stacksize,ThreadFunction,p,0,ThreadId)
        else
          Result:=BeginThread(nil,stacksize,ThreadFunction,p,0,ThreadId);
      end;

    procedure EndThread;
      begin
        EndThread(0);
      end;

function InitMultiThreading : Boolean;

begin
  Result:=IsMultiThread;
  if assigned(CurrentTM.InitMultiThreading) then
    CurrentTM.InitMultiThreading;
  if not IsMultiThread then
    begin
    LazyInitThreading;
    IsMultiThread:=true;
    end;
end;

function BeginThread(sa : Pointer;stacksize : SizeUInt; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;  var ThreadId : TThreadID) : TThreadHandle;

begin
{$ifndef HAS_CREATE_SUSPENDED}
  if (creationFlags and CREATE_SUSPENDED)<>0 then
    Result:=BeginSuspendedThread(sa,stacksize,threadfunction,P,creationFlags and not CREATE_SUSPENDED,ThreadID)
  else
{$endif HAS_CREATE_SUSPENDED}
    Result:=CurrentTM.BeginThread(sa,stacksize,threadfunction,P,creationflags,ThreadID);
end;

procedure FlushThread;

begin
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  SysFlushStdio;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
end;

procedure EndThread(ExitCode : DWord);

begin
  CurrentTM.EndThread(ExitCode);
end;

function  SuspendThread (threadHandle : TThreadHandle) : dword;

begin
  Result:=CurrentTM.SuspendThread(ThreadHandle);
end;

function ResumeThread  (threadHandle : TThreadHandle) : dword;

begin
  if PulseSuspendedStart(threadHandle,false) then
    Result:=1
  else
    Result:=CurrentTM.ResumeThread(ThreadHandle);
end;

function CloseThread  (threadHandle : TThreadHandle):dword;

begin
  PulseSuspendedStart(threadHandle,true);
  result:=CurrentTM.CloseThread(ThreadHandle);
end;

procedure ThreadSwitch;

begin
  CurrentTM.ThreadSwitch;
end;

function  KillThread (threadHandle : TThreadHandle) : dword;

begin
  PulseSuspendedStart(threadHandle,true);
  Result:=CurrentTM.KillThread(ThreadHandle);
end;

function  WaitForThreadTerminate (threadHandle : TThreadHandle; TimeoutMs : longint) : dword;

begin
  Result:=CurrentTM.WaitForThreadTerminate(ThreadHandle,TimeOutMS);
end;

function  ThreadSetPriority (threadHandle : TThreadHandle; Prio: longint): boolean;
begin
  Result:=CurrentTM.ThreadSetPriority(ThreadHandle,Prio);
end;

function  ThreadGetPriority (threadHandle : TThreadHandle): longint;

begin
  Result:=CurrentTM.ThreadGetPriority(ThreadHandle);
end;

function  GetCurrentThreadId : TThreadID;

begin
  Result:=CurrentTM.GetCurrentThreadID();
end;

procedure SetThreadDebugName(threadHandle: TThreadID; const ThreadName: AnsiString);
begin
  CurrentTM.SetThreadDebugNameA(threadHandle, ThreadName);
end;

{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
procedure SetThreadDebugName(threadHandle: TThreadID; const ThreadName: UnicodeString);
begin
  CurrentTM.SetThreadDebugNameU(threadHandle, ThreadName);
end;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}

procedure InitCriticalSection(out cs : TRTLCriticalSection);

begin
  CurrentTM.InitCriticalSection(cs);
end;

procedure DoneCriticalSection(var cs : TRTLCriticalSection);

begin
  CurrentTM.DoneCriticalSection(cs);
end;

procedure EnterCriticalSection(var cs : TRTLCriticalSection);

begin
  CurrentTM.EnterCriticalSection(cs);
end;

function TryEnterCriticalSection(var cs : TRTLCriticalSection):longint;

begin
  result:=CurrentTM.TryEnterCriticalSection(cs);
end;

procedure LeaveCriticalSection(var cs : TRTLCriticalSection);

begin
  CurrentTM.LeaveCriticalSection(cs);
end;

Function GetThreadManager(Out TM : TThreadManager) : Boolean;

begin
  TM:=CurrentTM;
  Result:=True;
end;

Function SetThreadManager(Const NewTM : TThreadManager; Out OldTM : TThreadManager) : Boolean;

begin
  GetThreadManager(OldTM);
  Result:=SetThreadManager(NewTM);
end;

Function SetThreadManager(Const NewTM : TThreadManager) : Boolean;

begin
  Result:=True;
  If Assigned(CurrentTM.DoneManager) then
    Result:=CurrentTM.DoneManager();
  If Result then
    begin
    CurrentTM:=NewTM;
    If Assigned(CurrentTM.InitManager) then
      Result:=CurrentTM.InitManager();
    end;
end;

function  BasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;

begin
  result:=currenttm.BasicEventCreate(EventAttributes,AManualReset,InitialState, Name);
end;

procedure BasicEventDestroy(state:peventstate);

begin
  currenttm.BasicEventDestroy(state);
end;

procedure BasicEventResetEvent(state:peventstate);

begin
  currenttm.BasicEventResetEvent(state);
end;

procedure BasicEventSetEvent(state:peventstate);

begin
  currenttm.BasicEventSetEvent(state);
end;

function  BasicEventWaitFor(Timeout : Cardinal;state:peventstate;FUseComWait : Boolean=False) : longint;

begin
 result:=currenttm.BasicEventWaitFor(Timeout,state,FUseComWait);
end;

function  RTLEventCreate :PRTLEvent;

begin
  result:=currenttm.RTLEventCreate();
end;


procedure RTLeventDestroy(state:pRTLEvent);

begin
  currenttm.RTLEventDestroy(state);
end;

procedure RTLeventSetEvent(state:pRTLEvent);

begin
  currenttm.RTLEventSetEvent(state);
end;

procedure RTLeventResetEvent(state:pRTLEvent);

begin
  currenttm.RTLEventResetEvent(state);
end;

procedure RTLeventWaitFor(state:pRTLEvent);

begin
  currenttm.RTLEventWaitFor(state);
end;

function RTLeventWaitFor(state:pRTLEvent;timeout : longint) : Boolean;

begin
  Result:=currenttm.RTLEventWaitForTimeout(state,timeout);
end;

{ ---------------------------------------------------------------------
    lazy thread initialization support
  ---------------------------------------------------------------------}

type
  PLazyInitThreadingProcInfo = ^TLazyInitThreadingProcInfo;
  TLazyInitThreadingProcInfo = Record
    Next     : PLazyInitThreadingProcInfo;
    Proc     : TProcedure;
  End;
const
  LazyInitThreadingProcList: PLazyInitThreadingProcInfo = nil;

procedure FinalizeLazyInitThreading;
var
  p: PLazyInitThreadingProcInfo;
begin
  while assigned(LazyInitThreadingProcList) do
    begin
    p:=LazyInitThreadingProcList^.Next;
    Dispose(LazyInitThreadingProcList);
    LazyInitThreadingProcList:=p;
    end;
end;

procedure RegisterLazyInitThreadingProc(const proc: TProcedure);
var
  p: PLazyInitThreadingProcInfo;
begin
  if IsMultiThread then
    begin
    { multithreading is already enabled - execute directly }
    proc();
    end
  else
    begin
    if not assigned(LazyInitThreadingProcList) then
      AddExitProc(@FinalizeLazyInitThreading);
    new(p);
    p^.Next:=LazyInitThreadingProcList;
    p^.Proc:=proc;
    LazyInitThreadingProcList:=p;
    end;
end;

procedure LazyInitThreading;
var
  p: PLazyInitThreadingProcInfo;
begin
  p:=LazyInitThreadingProcList;
  while assigned(p) do
    begin
    p^.Proc();
    p:=p^.Next;
    end;
end;

{ ---------------------------------------------------------------------
    ThreadManager which gives run-time error. Use if no thread support.
  ---------------------------------------------------------------------}

{$ifndef DISABLE_NO_THREAD_MANAGER}

{ resourcestrings are not supported by the system unit,
  they are in the objpas unit and not available for fpc/tp modes }
const
  SNoThreads = 'This binary has no thread support compiled in.';
  SRecompileWithThreads = 'Recompile the application with a thread-driver in the program uses clause before other units using thread.';

Procedure NoThreadError;

begin
{$ifndef EMBEDDED}
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
{$ifndef FPC_SYSTEM_NO_VERBOSE_THREADERROR}
  If IsConsole then
    begin
    Writeln(StdErr,SNoThreads);
    Writeln(StdErr,SRecompileWithThreads);
    end;
{$endif FPC_SYSTEM_NO_VERBOSE_THREADERROR}
{$endif FPC_HAS_FEATURE_CONSOLEIO}
{$endif EMBEDDED}
  RunError(232)
end;

function NoBeginThread(sa : Pointer;stacksize : PtrUInt;
                     ThreadFunction : tthreadfunc;p : pointer;
                     creationFlags : dword; var ThreadId : TThreadID) : TThreadHandle;
begin
  NoThreadError;
  result:=TThreadHandle(-1);
end;

procedure NoEndThread(ExitCode : DWord);
begin
  NoThreadError;
end;

function  NoThreadHandler (threadHandle : TThreadHandle) : dword;
begin
  NoThreadError;
  result:=dword(-1);
end;

function  NoWaitForThreadTerminate (threadHandle : TThreadHandle; TimeoutMs : longint) : dword;  {0=no timeout}
begin
  NoThreadError;
  result:=dword(-1);
end;

function  NoThreadSetPriority (threadHandle : TThreadHandle; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  NoThreadError;
  result:=false;
end;

function  NoThreadGetPriority (threadHandle : TThreadHandle): longint;
begin
  NoThreadError;
  result:=-1;
end;

function  NoGetCurrentThreadId : TThreadID;
begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
  result:=TThreadID(1);
end;

procedure NoSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);
begin
  NoThreadError;
end;

{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
procedure NoSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);
begin
  NoThreadError;
end;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}

procedure NoCriticalSection(var CS);

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
end;

function NoTryEnterCriticalSection(var CS):longint;

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
  Result:=-1;
end;

procedure NoInitThreadvar(var offset : {$ifdef cpu16}word{$else}dword{$endif};size : dword);

begin
  NoThreadError;
end;

function NoRelocateThreadvar(offset : {$ifdef cpu16}word{$else}dword{$endif}) : pointer;

begin
  NoThreadError;
  result:=nil;
end;


function  NoBasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
  result:=nil;
end;

procedure NoBasicEvent(state:peventstate);

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
end;

function  NoBasicEventWaitFor(Timeout : Cardinal;state:peventstate;FUseComWait : Boolean=False) : longint;

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
  result:=-1;
end;

function  NoRTLEventCreate :PRTLEvent;

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
  result:=nil;
end;

procedure NoRTLEvent(state:pRTLEvent);

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true
end;

function NoRTLEventWaitForTimeout(state:pRTLEvent;timeout : longint) : Boolean;
begin
  Result:=False;
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
end;


const
  NoThreadManager : TThreadManager = (
         InitManager            : Nil;
         DoneManager            : Nil;
{$ifdef EMBEDDED}
         { while this is pretty hacky, it reduces the size of typical embedded programs
           and works fine on arm and avr }
         BeginThread            : @NoBeginThread;
         EndThread              : TEndThreadHandler(@NoThreadError);
         SuspendThread          : TThreadHandler(@NoThreadError);
         ResumeThread           : TThreadHandler(@NoThreadError);
         KillThread             : TThreadHandler(@NoThreadError);
         CloseThread            : TThreadHandler(@NoThreadError);
         ThreadSwitch           : TThreadSwitchHandler(@NoThreadError);
         WaitForThreadTerminate : TWaitForThreadTerminateHandler(@NoThreadError);
         ThreadSetPriority      : TThreadSetPriorityHandler(@NoThreadError);
         ThreadGetPriority      : TThreadGetPriorityHandler(@NoThreadError);
         GetCurrentThreadId     : @NoGetCurrentThreadId;
         SetThreadDebugNameA    : TThreadSetThreadDebugNameHandlerA(@NoThreadError);
         {$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
         SetThreadDebugNameU    : TThreadSetThreadDebugNameHandlerU(@NoThreadError);
         {$endif FPC_HAS_FEATURE_UNICODESTRINGS}
         InitCriticalSection    : TCriticalSectionHandler(@NoThreadError);
         DoneCriticalSection    : TCriticalSectionHandler(@NoThreadError);
         EnterCriticalSection   : TCriticalSectionHandler(@NoThreadError);
         TryEnterCriticalSection: TCriticalSectionHandlerTryEnter(@NoThreadError);
         LeaveCriticalSection   : TCriticalSectionHandler(@NoThreadError);
         InitThreadVar          : TInitThreadVarHandler(@NoThreadError);
         RelocateThreadVar      : TRelocateThreadVarHandler(@NoThreadError);
         AllocateThreadVars     : @NoThreadError;
         ReleaseThreadVars      : @NoThreadError;
         BasicEventCreate       : TBasicEventCreateHandler(@NoThreadError);
         BasicEventdestroy      : TBasicEventHandler(@NoThreadError);
         BasicEventResetEvent   : TBasicEventHandler(@NoThreadError);
         BasicEventSetEvent     : TBasicEventHandler(@NoThreadError);
         BasicEventWaitFor      : TBasicEventWaitForHandler(@NoThreadError);
         RTLEventCreate         : TRTLCreateEventHandler(@NoThreadError);
         RTLEventdestroy        : TRTLEventHandler(@NoThreadError);
         RTLEventSetEvent       : TRTLEventHandler(@NoThreadError);
         RTLEventResetEvent     : TRTLEventHandler(@NoThreadError);
         RTLEventWaitFor        : TRTLEventHandler(@NoThreadError);
         RTLEventwaitfortimeout : TRTLEventHandlerTimeout(@NoThreadError);
         InitMultiThreading     : @NoThreadError;
{$else EMBEDDED}
         BeginThread            : @NoBeginThread;
         EndThread              : @NoEndThread;
         SuspendThread          : @NoThreadHandler;
         ResumeThread           : @NoThreadHandler;
         KillThread             : @NoThreadHandler;
         CloseThread            : @NoThreadHandler;
         ThreadSwitch           : @NoThreadError;
         WaitForThreadTerminate : @NoWaitForThreadTerminate;
         ThreadSetPriority      : @NoThreadSetPriority;
         ThreadGetPriority      : @NoThreadGetPriority;
         GetCurrentThreadId     : @NoGetCurrentThreadId;
         SetThreadDebugNameA    : @NoSetThreadDebugNameA;
         {$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
         SetThreadDebugNameU    : @NoSetThreadDebugNameU;
         {$endif FPC_HAS_FEATURE_UNICODESTRINGS}
         InitCriticalSection    : @NoCriticalSection;
         DoneCriticalSection    : @NoCriticalSection;
         EnterCriticalSection   : @NoCriticalSection;
         TryEnterCriticalSection: @NoTryEnterCriticalSection;
         LeaveCriticalSection   : @NoCriticalSection;
         InitThreadVar          : @NoInitThreadVar;
         RelocateThreadVar      : @NoRelocateThreadVar;
         AllocateThreadVars     : @NoThreadError;
         ReleaseThreadVars      : @NoThreadError;
         BasicEventCreate       : @NoBasicEventCreate;
         BasicEventDestroy      : @NoBasicEvent;
         BasicEventResetEvent   : @NoBasicEvent;
         BasicEventSetEvent     : @NoBasicEvent;
         BasicEventWaitFor      : @NoBasiceventWaitFor;
         RTLEventCreate         : @NoRTLEventCreate;
         RTLEventDestroy        : @NoRTLevent;
         RTLEventSetEvent       : @NoRTLevent;
         RTLEventResetEvent     : @NoRTLEvent;
         RTLEventWaitFor        : @NoRTLEvent;
         RTLEventWaitforTimeout : @NoRTLEventWaitForTimeout;
         InitMultiThreading     : @NoThreadError;
{$endif EMBEDDED}
      );

Procedure SetNoThreadManager;

begin
  SetThreadManager(NoThreadManager);
end;

Procedure InitSystemThreads; public name '_FPC_InitSystemThreads';
begin
  { This should be changed to a real value during
    thread driver initialization if appropriate. }
  ThreadID := TThreadID(1);
  SetNoThreadManager;
end;

{$endif DISABLE_NO_THREAD_MANAGER}
