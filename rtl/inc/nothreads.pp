{$IFNDEF FPC_DOTTEDUNITS}
unit nothreads;
{$ENDIF}

{$mode objfpc}

interface

Procedure SetFakeThreadManager;

implementation

Const
  wrSignaled = 0;
  wrTimeout  = 1;
  wrAbandoned= 2;
  wrError    = 3;


var
  threadvarblocksize : dword = 0;
  threadvarblock : array[1..64*10240] of byte;
  threadcount : Integer;

procedure FakeInitThreadvar(var offset : dword;size : dword);
  begin
    offset:=threadvarblocksize;
    inc(threadvarblocksize,size);
  end;



procedure FakeAllocateThreadVars;
begin
end;


procedure FakethreadCleanup(p: pointer); cdecl;

begin
end;

procedure HookThread;
begin
  { Allocate local thread vars, this must be the first thing,
    because the exception management and io depends on threadvars }
  FakeAllocateThreadVars;
  InitThread(1000000000);
end;


function FakeRelocateThreadvar(offset : dword) : pointer;
begin
  FakeRelocateThreadvar:=@ThreadVarBlock;
end;


procedure FakeReleaseThreadVars;
begin
end;



Procedure InitCTLS;

begin
end;

function FakeBeginThread(sa : Pointer;stacksize : PtrUInt;
                     ThreadFunction : tthreadfunc;p : pointer;
                     creationFlags : dword; var ThreadId : TThreadId) : TThreadID;
begin
  Inc(ThreadCount);
  Result:=TThreadID(ThreadCount);
  ThreadFunction(P);
end;


procedure FakeEndThread(ExitCode : DWord);

begin
end;

function  FakeSuspendThread (threadHandle : TThreadID) : dword;
begin
  result:=dword(-1);
end;


function  FakeResumeThread  (threadHandle : TThreadID) : dword;
begin
  result:=dword(-1);
end;


procedure FakeThreadSwitch;  {give time to other threads}
begin
end;


function  FakeKillThread (threadHandle : TThreadID) : dword;
begin
  Result:= dword(-1);
end;

function FakeCloseThread (threadHandle : TThreadID) : dword;
begin
  result:=0;
end;

function  FakeWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
begin
  Result:=0
end;

function  FakeThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  result:=false;
end;


function  FakeThreadGetPriority (threadHandle : TThreadID): Integer;
begin
  result:=0;
end;


function  FakeGetCurrentThreadId : TThreadID;
begin
  Result:=TThreadID(0);
end;


procedure FakeSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);

begin
end;


procedure FakeSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);

begin
end;


{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

procedure FakeInitCriticalSection(var CS);

begin
  PLongint(@cs)^:=0;
end;
 
procedure FakeEnterCriticalSection(var CS);
begin
end;

function FakeTryEnterCriticalSection(var CS):longint;
begin
  Result:=0;
end;

procedure FakeLeaveCriticalSection(var CS);
begin
end;

procedure FakeDoneCriticalSection(var CS);
begin
end;


{*****************************************************************************
                           Semaphore routines
*****************************************************************************}

var
  Dummy : Integer;

function FakeBasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
begin
  Result:=pEventState(@Dummy);
end;

procedure FakeBasiceventdestroy(state:peventstate);

begin
end;

procedure FakeBasiceventResetEvent(state:peventstate);
begin
end;

procedure FakeBasiceventSetEvent(state:peventstate);
begin
end;

function FakeBasiceventWaitFor(Timeout : Cardinal;state:peventstate;FUseComWait : Boolean=False) : longint;
begin
  Result:=wrSignaled;
end;

function FakeRTLEventCreate: PRTLEvent;

begin
  result:=PRTLEVENT(@Dummy);
end;

procedure FakeRTLEventDestroy(AEvent: PRTLEvent);


begin
end;

procedure FakeRTLEventSetEvent(AEvent: PRTLEvent);

begin

end;

procedure FakeRTLEventResetEvent(AEvent: PRTLEvent);

begin
end;


procedure FakeRTLEventWaitFor(AEvent: PRTLEvent);

begin
end;

procedure FakeRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);

begin
end;


Function FakeInitThreads : Boolean;

begin
  Result:=True;
end;

Function FakeDoneThreads : Boolean;

begin
  Result:=True;
end;


Var
  FakeThreadManager : TThreadManager;

Procedure SetFakeThreadManager;

begin
  With FakeThreadManager do begin
    InitManager            :=@FakeInitThreads;
    DoneManager            :=@FakeDoneThreads;
    BeginThread            :=@FakeBeginThread;
    EndThread              :=@FakeEndThread;
    SuspendThread          :=@FakeSuspendThread;
    ResumeThread           :=@FakeResumeThread;
    KillThread             :=@FakeKillThread;
    ThreadSwitch           :=@FakeThreadSwitch;
    CloseThread	           :=@FakeCloseThread;
    WaitForThreadTerminate :=@FakeWaitForThreadTerminate;
    ThreadSetPriority      :=@FakeThreadSetPriority;
    ThreadGetPriority      :=@FakeThreadGetPriority;
    GetCurrentThreadId     :=@FakeGetCurrentThreadId;
    SetThreadDebugNameA    :=@FakeSetThreadDebugNameA;
    SetThreadDebugNameU    :=@FakeSetThreadDebugNameU;
    InitCriticalSection    :=@FakeInitCriticalSection;
    DoneCriticalSection    :=@FakeDoneCriticalSection;
    EnterCriticalSection   :=@FakeEnterCriticalSection;
    TryEnterCriticalSection:=@FakeTryEnterCriticalSection;
    LeaveCriticalSection   :=@FakeLeaveCriticalSection;
    InitThreadVar          :=@FakeInitThreadVar;
    RelocateThreadVar      :=@FakeRelocateThreadVar;
    AllocateThreadVars     :=@FakeAllocateThreadVars;
    ReleaseThreadVars      :=@FakeReleaseThreadVars;
    BasicEventCreate       :=@FakeBasicEventCreate;
    BasicEventDestroy      :=@FakeBasicEventDestroy;
    BasicEventResetEvent   :=@FakeBasicEventResetEvent;
    BasicEventSetEvent     :=@FakeBasicEventSetEvent;
    BasiceventWaitFor      :=@FakeBasiceventWaitFor;
    rtlEventCreate         :=@FakertlEventCreate;
    rtlEventDestroy        :=@FakertlEventDestroy;
    rtlEventSetEvent       :=@FakertlEventSetEvent;
    rtlEventResetEvent     :=@FakertlEventResetEvent;
    rtleventWaitForTimeout :=@FakertleventWaitForTimeout;
    rtleventWaitFor        :=@FakertleventWaitFor;
  end;
  SetThreadManager(FakeThreadManager);
end;


initialization
  Writeln('Initializing unit fakethreads');
  SetFakeThreadManager;
  Writeln('Done Initializing unit fakethreads');
  
finalization

end.