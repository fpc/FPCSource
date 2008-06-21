{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2000 by the Free Pascal development team

    This file contains the OS indenpendend declartions for multi
    threading support in FPC

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  { including 16384 margin for stackchecking }
  DefaultStackSize = 32768;

type
  PEventState = pointer;
  PRTLEvent   = type pointer;   // Windows=thandle, other=pointer to record.
  TThreadFunc = function(parameter : pointer) : ptrint;
  trtlmethod  = procedure of object;

  // Function prototypes for TThreadManager Record.
  TBeginThreadHandler = Function (sa : Pointer;stacksize : PtrUInt; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword; var ThreadId : TThreadID) : TThreadID;
  TEndThreadHandler = Procedure (ExitCode : DWord);
  // Used for Suspend/Resume/Kill
  TThreadHandler = Function (threadHandle : TThreadID) : dword;
  TThreadSwitchHandler = Procedure;
  TWaitForThreadTerminateHandler = Function (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
  TThreadSetPriorityHandler = Function (threadHandle : TThreadID; Prio: longint): boolean;            {-15..+15, 0=normal}
  TThreadGetPriorityHandler = Function (threadHandle : TThreadID): longint;
  TGetCurrentThreadIdHandler = Function : TThreadID;
  TCriticalSectionHandler = Procedure (var cs);
  TInitThreadVarHandler = Procedure(var offset : dword;size : dword);
  TRelocateThreadVarHandler = Function(offset : dword) : pointer;
  TAllocateThreadVarsHandler = Procedure;
  TReleaseThreadVarsHandler = Procedure;
  TBasicEventHandler        = procedure(state:peventstate);
  TBasicEventWaitForHandler = function (timeout:cardinal;state:peventstate):longint;
  TBasicEventCreateHandler  = function (EventAttributes :Pointer;  AManualReset,InitialState : Boolean;const Name:ansistring):pEventState;
  TRTLEventHandler          = procedure(AEvent:PRTLEvent);
  TRTLEventHandlerTimeout   = procedure(AEvent:PRTLEvent;timeout : longint);
  TRTLCreateEventHandler    = function:PRTLEvent;
  TRTLEventSyncHandler      = procedure (m:trtlmethod;p:tprocedure);
  // semaphores stuff
  TSempahoreInitHandler     = function: Pointer;
  TSemaphoreDestroyHandler  = procedure (const sem: Pointer);
  TSemaphorePostHandler     = procedure (const sem: Pointer);
  TSemaphoreWaitHandler     = procedure (const sem: Pointer);

  // TThreadManager interface.
  TThreadManager = Record
    InitManager            : Function : Boolean;
    DoneManager            : Function : Boolean;
    BeginThread            : TBeginThreadHandler;
    EndThread              : TEndThreadHandler;
    SuspendThread          : TThreadHandler;
    ResumeThread           : TThreadHandler;
    KillThread             : TThreadHandler;
    ThreadSwitch           : TThreadSwitchHandler;
    WaitForThreadTerminate : TWaitForThreadTerminateHandler;
    ThreadSetPriority      : TThreadSetPriorityHandler;
    ThreadGetPriority      : TThreadGetPriorityHandler;
    GetCurrentThreadId     : TGetCurrentThreadIdHandler;
    InitCriticalSection    : TCriticalSectionHandler;
    DoneCriticalSection    : TCriticalSectionHandler;
    EnterCriticalSection   : TCriticalSectionHandler;
    LeaveCriticalSection   : TCriticalSectionHandler;
    InitThreadVar          : TInitThreadVarHandler;
    RelocateThreadVar      : TRelocateThreadVarHandler;
    AllocateThreadVars     : TAllocateThreadVarsHandler;
    ReleaseThreadVars      : TReleaseThreadVarsHandler;
    BasicEventCreate       : TBasicEventCreateHandler;      // left in for a while.
    BasicEventDestroy      : TBasicEventHandler;            // we might need BasicEvent
    BasicEventResetEvent   : TBasicEventHandler;            // for a real TEvent
    BasicEventSetEvent     : TBasicEventHandler;
    BasiceventWaitFOr      : TBasicEventWaitForHandler;
    RTLEventCreate         : TRTLCreateEventHandler;
    RTLEventDestroy        : TRTLEventHandler;
    RTLEventSetEvent       : TRTLEventHandler;
    RTLEventResetEvent     : TRTLEventHandler;
    RTLEventWaitFor        : TRTLEventHandler;
    RTLEventSync           : TRTLEventSyncHandler;
    RTLEventWaitForTimeout : TRTLEventHandlerTimeout;
    // semaphores stuff
    SemaphoreInit          : TSempahoreInitHandler;
    SemaphoreDestroy       : TSemaphoreDestroyHandler;
    SemaphorePost          : TSemaphorePostHandler;
    SemaphoreWait          : TSemaphoreWaitHandler;
  end;

{*****************************************************************************
                         Thread Handler routines
*****************************************************************************}


Function GetThreadManager(Var TM : TThreadManager) : Boolean;
Function SetThreadManager(Const NewTM : TThreadManager; Var OldTM : TThreadManager) : Boolean;
Function SetThreadManager(Const NewTM : TThreadManager) : Boolean;
{$ifndef DISABLE_NO_THREAD_MANAGER}
{$endif DISABLE_NO_THREAD_MANAGER}
// Needs to be exported, so the manager can call it.
procedure InitThreadVars(RelocProc : Pointer);
procedure InitThread(stklen:SizeUInt);
procedure DoneThread;

{*****************************************************************************
                         Multithread Handling
*****************************************************************************}

function BeginThread(sa : Pointer;stacksize : SizeUInt;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : TThreadID) : TThreadID;

{ add some simplfied forms which make lifer easier and porting }
{ to other OSes too ...                                        }
function BeginThread(ThreadFunction : tthreadfunc) : TThreadID;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : TThreadID;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : TThreadID) : TThreadID;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer;
                     var ThreadId : TThreadID; const stacksize: SizeUInt) : TThreadID;

procedure EndThread(ExitCode : DWord);
procedure EndThread;

{some thread support functions}
procedure FlushThread;
function  SuspendThread (threadHandle : TThreadID) : dword;
function  ResumeThread  (threadHandle : TThreadID) : dword;
procedure ThreadSwitch;                                                                {give time to other threads}
function  KillThread (threadHandle : TThreadID) : dword;
function  WaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
function  ThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean;            {-15..+15, 0=normal}
function  ThreadGetPriority (threadHandle : TThreadID): longint;
function  GetCurrentThreadId : TThreadID;


{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
procedure DoneCriticalsection(var cs : TRTLCriticalSection);
procedure EnterCriticalsection(var cs : TRTLCriticalSection);
procedure LeaveCriticalsection(var cs : TRTLCriticalSection);

function  BasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
procedure basiceventdestroy(state:peventstate);
procedure basiceventResetEvent(state:peventstate);
procedure basiceventSetEvent(state:peventstate);
function  basiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

function  RTLEventCreate :PRTLEvent;
procedure RTLeventdestroy(state:pRTLEvent);
procedure RTLeventSetEvent(state:pRTLEvent);
procedure RTLeventResetEvent(state:pRTLEvent);
procedure RTLeventWaitFor(state:pRTLEvent);
procedure RTLeventWaitFor(state:pRTLEvent;timeout : longint);
procedure RTLeventsync(m:trtlmethod;p:tprocedure);

