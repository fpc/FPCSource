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

{*****************************************************************************
                           Threadvar initialization
*****************************************************************************}

    procedure InitThread(stklen:SizeUInt);
      begin
        SysResetFPU;
        SysInitFPU;
{$ifndef HAS_MEMORYMANAGER}
        { initialize this thread's heap }
        InitHeapThread;
{$endif HAS_MEMORYMANAGER}
        if MemoryManager.InitThread <> nil then
          MemoryManager.InitThread();
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
        if assigned(widestringmanager.ThreadInitProc) then
          widestringmanager.ThreadInitProc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
        { ExceptAddrStack and ExceptObjectStack are threadvars       }
        { so every thread has its on exception handling capabilities }
        SysInitExceptions;
        { Open all stdio fds again }
        SysInitStdio;
        InOutRes:=0;
        // ErrNo:=0;
        { Stack checking }
        StackLength:= CheckInitialStkLen(stkLen);
        StackBottom:=Sptr - StackLength;
        ThreadID := CurrentTM.GetCurrentThreadID();
      end;

    procedure DoneThread;
      begin
{$ifdef FPC_HAS_FEATURE_WIDESTRINGS}
        if assigned(widestringmanager.ThreadFiniProc) then
          widestringmanager.ThreadFiniProc;
{$endif FPC_HAS_FEATURE_WIDESTRINGS}
{$ifndef HAS_MEMORYMANAGER}
        FinalizeHeap;
{$endif HAS_MEMORYMANAGER}
        if MemoryManager.DoneThread <> nil then
          MemoryManager.DoneThread();
        CurrentTM.ReleaseThreadVars;
      end;

{*****************************************************************************
                            Overloaded functions
*****************************************************************************}

    function BeginThread(ThreadFunction : tthreadfunc) : TThreadID;
      var
        dummy : TThreadID;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,nil,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : TThreadID;
      var
        dummy : TThreadID;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : TThreadID) : TThreadID;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,ThreadId);
      end;

    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
                     var ThreadId : TThreadID; const stacksize: SizeUInt) : TThreadID;
      begin
        BeginThread:=BeginThread(nil,stacksize,ThreadFunction,p,0,ThreadId);
      end;

    procedure EndThread;
      begin
        EndThread(0);
      end;

function BeginThread(sa : Pointer;stacksize : SizeUInt; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;  var ThreadId : TThreadID) : TThreadID;

begin
  Result:=CurrentTM.BeginThread(sa,stacksize,threadfunction,P,creationflags,ThreadID);
end;

procedure FlushThread;

begin
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  { Make sure that all output is written to the redirected file }
  Flush(Output);
  Flush(ErrOutput);
  Flush(StdOut);
  Flush(StdErr);
{$endif FPC_HAS_FEATURE_CONSOLEIO}
end;

procedure EndThread(ExitCode : DWord);

begin
  FlushThread;
  CurrentTM.EndThread(ExitCode);
end;

function  SuspendThread (threadHandle : TThreadID) : dword;

begin
  Result:=CurrentTM.SuspendThread(ThreadHandle);
end;

function ResumeThread  (threadHandle : TThreadID) : dword;

begin
  Result:=CurrentTM.ResumeThread(ThreadHandle);
end;

procedure ThreadSwitch;

begin
  CurrentTM.ThreadSwitch;
end;

function  KillThread (threadHandle : TThreadID) : dword;

begin
  Result:=CurrentTM.KillThread(ThreadHandle);
end;

function  WaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;

begin
  Result:=CurrentTM.WaitForThreadTerminate(ThreadHandle,TimeOutMS);
end;

function  ThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean;
begin
  Result:=CurrentTM.ThreadSetPriority(ThreadHandle,Prio);
end;

function  ThreadGetPriority (threadHandle : TThreadID): longint;

begin
  Result:=CurrentTM.ThreadGetPriority(ThreadHandle);
end;

function  GetCurrentThreadId : TThreadID;

begin
  Result:=CurrentTM.GetCurrentThreadID();
end;

procedure InitCriticalSection(var cs : TRTLCriticalSection);

begin
  CurrentTM.InitCriticalSection(cs);
end;

procedure DoneCriticalsection(var cs : TRTLCriticalSection);

begin
  CurrentTM.DoneCriticalSection(cs);
end;

procedure EnterCriticalsection(var cs : TRTLCriticalSection);

begin
  CurrentTM.EnterCriticalSection(cs);
end;

procedure LeaveCriticalsection(var cs : TRTLCriticalSection);

begin
  CurrentTM.LeaveCriticalSection(cs);
end;

Function GetThreadManager(Var TM : TThreadManager) : Boolean;

begin
  TM:=CurrentTM;
  Result:=True;
end;

Function SetThreadManager(Const NewTM : TThreadManager; Var OldTM : TThreadManager) : Boolean;

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

procedure basiceventdestroy(state:peventstate);

begin
  currenttm.basiceventdestroy(state);
end;

procedure basiceventResetEvent(state:peventstate);

begin
  currenttm.basiceventResetEvent(state);
end;

procedure basiceventSetEvent(state:peventstate);

begin
  currenttm.basiceventSetEvent(state);
end;

function  basiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

begin
 result:=currenttm.basiceventWaitFor(Timeout,state);
end;

function  RTLEventCreate :PRTLEvent;

begin
  result:=currenttm.rtleventcreate();
end;


procedure RTLeventdestroy(state:pRTLEvent);

begin
  currenttm.rtleventdestroy(state);
end;

procedure RTLeventSetEvent(state:pRTLEvent);

begin
  currenttm.rtleventsetEvent(state);
end;

procedure RTLeventResetEvent(state:pRTLEvent);

begin
  currenttm.rtleventResetEvent(state);
end;

procedure RTLeventWaitFor(state:pRTLEvent);

begin
  currenttm.rtleventWaitFor(state);
end;

procedure RTLeventWaitFor(state:pRTLEvent;timeout : longint);

begin
  currenttm.rtleventWaitForTimeout(state,timeout);
end;

procedure RTLeventsync(m:trtlmethod;p:tprocedure);

begin
  currenttm.rtleventsync(m,p);
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
{$ifdef FPC_HAS_FEATURE_CONSOLEIO}
  If IsConsole then
    begin
    Writeln(StdErr,SNoThreads);
    Writeln(StdErr,SRecompileWithThreads);
    end;
{$endif FPC_HAS_FEATURE_CONSOLEIO}
  RunError(232)
end;

function NoBeginThread(sa : Pointer;stacksize : PtrUInt;
                     ThreadFunction : tthreadfunc;p : pointer;
                     creationFlags : dword; var ThreadId : TThreadID) : TThreadID;
begin
  NoThreadError;
end;

procedure NoEndThread(ExitCode : DWord);
begin
  NoThreadError;
end;

function  NoThreadHandler (threadHandle : TThreadID) : dword;
begin
  NoThreadError;
end;

procedure NoThreadSwitch;  {give time to other threads}
begin
  NoThreadError;
end;

function  NoWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
begin
  NoThreadError;
end;

function  NoThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  NoThreadError;
end;

function  NoThreadGetPriority (threadHandle : TThreadID): longint;
begin
  NoThreadError;
end;

function  NoGetCurrentThreadId : TThreadID;
begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
  result:=ThreadID;
end;

procedure NoCriticalSection(var CS);

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true;
end;

procedure NoInitThreadvar(var offset : dword;size : dword);

begin
  NoThreadError;
end;

function NoRelocateThreadvar(offset : dword) : pointer;

begin
  NoThreadError;
end;


procedure NoAllocateThreadVars;

begin
  NoThreadError;
end;

procedure NoReleaseThreadVars;

begin
  NoThreadError;
end;

function  noBasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;

begin
  NoThreadError;
end;

procedure nobasiceventdestroy(state:peventstate);

begin
  NoThreadError;
end;

procedure nobasiceventResetEvent(state:peventstate);

begin
  NoThreadError;
end;

procedure nobasiceventSetEvent(state:peventstate);

begin
  NoThreadError;
end;

function  nobasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

begin
  NoThreadError;
end;

function  NORTLEventCreate :PRTLEvent;

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true
end;

procedure NORTLeventdestroy(state:pRTLEvent);

begin
  if IsMultiThread then
    NoThreadError
  else
    ThreadingAlreadyUsed:=true
end;

procedure NORTLeventSetEvent(state:pRTLEvent);

begin
  NoThreadError;
end;

procedure NORTLeventWaitFor(state:pRTLEvent);
  begin
    NoThreadError;
  end;


procedure NORTLeventWaitForTimeout(state:pRTLEvent;timeout : longint);
  begin
    NoThreadError;
  end;


procedure NORTLeventsync(m:trtlmethod;p:tprocedure);
  begin
    NoThreadError;
  end;

function NoSemaphoreInit: Pointer;
begin
  NoThreadError;
end;

procedure NoSemaphoreWait(const FSem: Pointer);
begin
  NoThreadError;
end;

procedure NoSemaphorePost(const FSem: Pointer);
begin
  NoThreadError;
end;

procedure NoSemaphoreDestroy(const FSem: Pointer);
begin
  NoThreadError;
end;

Var
  NoThreadManager : TThreadManager;

Procedure SetNoThreadManager;

begin
  With NoThreadManager do
    begin
    InitManager            :=Nil;
    DoneManager            :=Nil;
    BeginThread            :=@NoBeginThread;
    EndThread              :=@NoEndThread;
    SuspendThread          :=@NoThreadHandler;
    ResumeThread           :=@NoThreadHandler;
    KillThread             :=@NoThreadHandler;
    ThreadSwitch           :=@NoThreadSwitch;
    WaitForThreadTerminate :=@NoWaitForThreadTerminate;
    ThreadSetPriority      :=@NoThreadSetPriority;
    ThreadGetPriority      :=@NoThreadGetPriority;
    GetCurrentThreadId     :=@NoGetCurrentThreadId;
    InitCriticalSection    :=@NoCriticalSection;
    DoneCriticalSection    :=@NoCriticalSection;
    EnterCriticalSection   :=@NoCriticalSection;
    LeaveCriticalSection   :=@NoCriticalSection;
    InitThreadVar          :=@NoInitThreadVar;
    RelocateThreadVar      :=@NoRelocateThreadVar;
    AllocateThreadVars     :=@NoAllocateThreadVars;
    ReleaseThreadVars      :=@NoReleaseThreadVars;
    BasicEventCreate       :=@NoBasicEventCreate;
    basiceventdestroy      :=@Nobasiceventdestroy;
    basiceventResetEvent   :=@NobasiceventResetEvent;
    basiceventSetEvent     :=@NobasiceventSetEvent;
    basiceventWaitFor      :=@NobasiceventWaitFor;
    rtlEventCreate         :=@NortlEventCreate;
    rtleventdestroy        :=@Nortleventdestroy;
    rtleventSetEvent       :=@NortleventSetEvent;
    rtleventWaitFor        :=@NortleventWaitFor;
    rtleventsync           :=@Nortleventsync;
    rtleventwaitfortimeout :=@NortleventWaitForTimeout;
    // semaphores stuff
    SemaphoreInit          :=@NoSemaphoreInit;
    SemaphoreDestroy       :=@NoSemaphoreDestroy;
    SemaphoreWait          :=@NoSemaphoreWait;
    SemaphorePost          :=@NoSemaphorePost;
    end;
  SetThreadManager(NoThreadManager);
end;

Procedure InitSystemThreads;
begin
  { This should be changed to a real value during
    thread driver initialization if appropriate. }
  ThreadID := TThreadID(1);
  SetNoThreadManager;
end;

{$endif DISABLE_NO_THREAD_MANAGER}
