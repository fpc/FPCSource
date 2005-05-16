{
    $Id: threadh.inc,v 1.32 2005/04/20 06:32:47 hajny Exp $
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
   DefaultStackSize = 32768; { including 16384 margin for stackchecking }


type
  PEventState = pointer;
  PRTLEvent   = pointer;   // Windows=thandle, other=pointer to record.
  TThreadFunc = function(parameter : pointer) : ptrint;
  trtlmethod  = procedure of object;

  // Function prototypes for TThreadManager Record.
  TBeginThreadHandler = Function (sa : Pointer;stacksize : dword; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword; var ThreadId : TThreadID) : DWord;
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
    RTLEventStartWait      : TRTLEventHandler;
    RTLEventWaitFor        : TRTLEventHandler;
    RTLEventSync           : TRTLEventSyncHandler;
    RTLEventWaitForTimeout : TRTLEventHandlerTimeout;
  end;

{*****************************************************************************
                         Thread Handler routines
*****************************************************************************}


Function GetThreadManager(Var TM : TThreadManager) : Boolean;
Function SetThreadManager(Const NewTM : TThreadManager; Var OldTM : TThreadManager) : Boolean;
Function SetThreadManager(Const NewTM : TThreadManager) : Boolean;
{$ifndef DISABLE_NO_THREAD_MANAGER}
Procedure SetNoThreadManager;
{$endif DISABLE_NO_THREAD_MANAGER}
// Needs to be exported, so the manager can call it.
{$ifdef HASTHREADVAR}
procedure InitThreadVars(RelocProc : Pointer);
{$endif HASTHREADVAR}
procedure InitThread(stklen:cardinal);

{*****************************************************************************
                         Multithread Handling
*****************************************************************************}

function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : TThreadID) : DWord;

{ add some simplfied forms which make lifer easier and porting }
{ to other OSes too ...                                        }
function BeginThread(ThreadFunction : tthreadfunc) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : TThreadID) : DWord;

procedure EndThread(ExitCode : DWord);
procedure EndThread;

{some thread support functions}
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
procedure RTLeventStartWait(state:pRTLEvent);
procedure RTLeventWaitFor(state:pRTLEvent);
procedure RTLeventWaitFor(state:pRTLEvent;timeout : longint);
procedure RTLeventsync(m:trtlmethod;p:tprocedure);

{
  $Log: threadh.inc,v $
  Revision 1.32  2005/04/20 06:32:47  hajny
    * yet another TThreadID fix

  Revision 1.31  2005/04/14 20:42:14  florian
    * fixed more TThreadID stuff

  Revision 1.30  2005/04/13 20:10:50  florian
    + TThreadID

  Revision 1.29  2005/04/09 17:26:08  florian
    + classes.mainthreadid is set now
    + rtleventresetevent
    + rtleventwairfor with timeout
    + checksynchronize with timeout
    * race condition in synchronize fixed

  Revision 1.28  2005/02/25 22:02:48  florian
    * another "transfer to linux"-commit

  Revision 1.27  2005/02/14 17:13:29  peter
    * truncate log

  Revision 1.26  2005/02/06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

  Revision 1.25  2005/01/21 21:43:12  armin
  * applied patch to compile go32v2 from Tomas (tested by John)

}
