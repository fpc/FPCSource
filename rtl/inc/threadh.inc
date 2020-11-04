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
{$ifndef FPC_USE_SMALL_DEFAULTSTACKSIZE}
  { includes 16384 bytes margin for stackchecking }
  DefaultStackSize = 4*1024*1024;
{$else i.e. FPC_USE_SMALL_DEFAULTSTACKSIZE}
  { Special value of Default stack size }
  DefaultStackSize = 16 * 1024;
{$endif not FPC_USE_SMALL_DEFAULTSTACKSIZE}

{ every platform can have it's own implementation of GetCPUCount; use the define
  HAS_GETCPUCOUNT to disable the default implementation which simply returns 1 }
function GetCPUCount: LongWord;

property CPUCount: LongWord read GetCPUCount;

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
  TThreadSetThreadDebugNameHandlerA = procedure(threadHandle: TThreadID; const ThreadName: AnsiString);
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
  TThreadSetThreadDebugNameHandlerU = procedure(threadHandle: TThreadID; const ThreadName: UnicodeString);
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}
  TCriticalSectionHandler = Procedure (var cs);
  TCriticalSectionHandlerTryEnter = function (var cs):longint;
  TInitThreadVarHandler = Procedure(var offset : {$ifdef cpu16}word{$else}dword{$endif};size : dword);
  TRelocateThreadVarHandler = Function(offset : {$ifdef cpu16}word{$else}dword{$endif}) : pointer;
  TAllocateThreadVarsHandler = Procedure;
  TReleaseThreadVarsHandler = Procedure;
  TBasicEventHandler        = procedure(state:peventstate);
  TBasicEventWaitForHandler = function (timeout:cardinal;state:peventstate):longint;
  TBasicEventCreateHandler  = function (EventAttributes :Pointer;  AManualReset,InitialState : Boolean;const Name:ansistring):pEventState;
  TRTLEventHandler          = procedure(AEvent:PRTLEvent);
  TRTLEventHandlerTimeout   = procedure(AEvent:PRTLEvent;timeout : longint);
  TRTLCreateEventHandler    = function:PRTLEvent;
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
    CloseThread            : TThreadHandler;
    ThreadSwitch           : TThreadSwitchHandler;
    WaitForThreadTerminate : TWaitForThreadTerminateHandler;
    ThreadSetPriority      : TThreadSetPriorityHandler;
    ThreadGetPriority      : TThreadGetPriorityHandler;
    GetCurrentThreadId     : TGetCurrentThreadIdHandler;
    SetThreadDebugNameA    : TThreadSetThreadDebugNameHandlerA;
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
    SetThreadDebugNameU    : TThreadSetThreadDebugNameHandlerU;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}
    InitCriticalSection    : TCriticalSectionHandler;
    DoneCriticalSection    : TCriticalSectionHandler;
    EnterCriticalSection   : TCriticalSectionHandler;
    TryEnterCriticalSection: TCriticalSectionHandlerTryEnter;
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
    RTLEventWaitForTimeout : TRTLEventHandlerTimeout;
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
{$ifndef FPC_SECTION_THREADVARS}
procedure InitThreadVars(RelocProc : TRelocateThreadVarHandler);
{$endif FPC_SECTION_THREADVARS}
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
function  CloseThread   (threadHandle : TThreadID) : dword;
procedure ThreadSwitch;                                                                {give time to other threads}
function  KillThread (threadHandle : TThreadID) : dword;
function  WaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
function  ThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean;            {-15..+15, 0=normal}
function  ThreadGetPriority (threadHandle : TThreadID): longint;
function  GetCurrentThreadId : TThreadID;
procedure SetThreadDebugName(threadHandle: TThreadID; const ThreadName: AnsiString);
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
procedure SetThreadDebugName(threadHandle: TThreadID; const ThreadName: UnicodeString);
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}


{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
procedure DoneCriticalSection(var cs : TRTLCriticalSection);
procedure EnterCriticalSection(var cs : TRTLCriticalSection);
procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
function  TryEnterCriticalSection(var cs : TRTLCriticalSection):longint;
function  BasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
procedure BasicEventDestroy(state:peventstate);
procedure BasicEventResetEvent(state:peventstate);
procedure BasicEventSetEvent(state:peventstate);
function  BasicEventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

function  RTLEventCreate :PRTLEvent;
procedure RTLEventDestroy(state:pRTLEvent);
procedure RTLEventSetEvent(state:pRTLEvent);
procedure RTLEventResetEvent(state:pRTLEvent);
procedure RTLEventWaitFor(state:pRTLEvent);
procedure RTLEventWaitFor(state:pRTLEvent;timeout : longint);

{ lazy thread initialization support }
procedure RegisterLazyInitThreadingProc(const proc: TProcedure);
{ do not call LazyInitThreading directly}
procedure LazyInitThreading;
