{
    $Id$
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2000 by the Free Pascal development team

    This File contains the OS indenpendend declartions for multi
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
  TThreadFunc = function(parameter : pointer) : longint;

  // Function prototypes for TThreadManager Record.
  TBeginThreadHandler = Function (sa : Pointer;stacksize : dword; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword; var ThreadId : THandle) : DWord;
  TEndThreadHandler = Procedure (ExitCode : DWord);
  // Used for Suspend/Resume/Kill
  TThreadHandler = Function (threadHandle : dword) : dword;
  TThreadSwitchHandler = Procedure;
  TWaitForThreadTerminateHandler = Function (threadHandle : dword; TimeoutMs : longint) : dword;  {0=no timeout}
  TThreadSetPriorityHandler = Function (threadHandle : dword; Prio: longint): boolean;            {-15..+15, 0=normal}
  TThreadGetPriorityHandler = Function (threadHandle : dword): Integer;
  TGetCurrentThreadIdHandler = Function : dword;
  TCriticalSectionHandler = Procedure (var cs);
  TInitThreadVarHandler = Procedure(var offset : dword;size : dword);
  TRelocateThreadVarHandler = Function(offset : dword) : pointer;
  TAllocateThreadVarsHandler = Procedure;
  TReleaseThreadVarsHandler = Procedure;
  TBasicEventHandler	    = procedure(state:peventstate);
  TBasicEventWaitForHandler = function (timeout:cardinal;state:peventstate):longint;
  TBasicEventCreateHandler  = function (EventAttributes :Pointer;  AManualReset,InitialState : Boolean;const Name:ansistring):pEventState;

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
    BasicEventCreate	   : TBasicEventCreateHandler;
    BasicEventDestroy	   : TBasicEventHandler;
    BasicEventResetEvent   : TBasicEventHandler;
    BasicEventSetEvent     : TBasicEventHandler;
    BasiceventWaitFOr	   : TBasicEventWaitForHandler;
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
  var ThreadId : THandle) : DWord;
{$ifndef CPU64}
{$ifndef unix}
{ Delphi uses a longint for threadid }
function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : Longint) : DWord;
{$endif unix}
{$endif CPU64}

{ add some simplfied forms which make lifer easier and porting }
{ to other OSes too ...                                        }
function BeginThread(ThreadFunction : tthreadfunc) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : THandle) : DWord;
{$ifndef CPU64}
{$ifndef unix}
function BeginThread(ThreadFunction : tthreadfunc;p : pointer; var ThreadId : Longint) : DWord;
{$endif unix}
{$endif CPU64}

procedure EndThread(ExitCode : DWord);
procedure EndThread;

{some thread support functions}
function  SuspendThread (threadHandle : dword) : dword;
function  ResumeThread  (threadHandle : dword) : dword;
procedure ThreadSwitch;                                                                {give time to other threads}
function  KillThread (threadHandle : dword) : dword;
function  WaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;  {0=no timeout}
function  ThreadSetPriority (threadHandle : dword; Prio: longint): boolean;            {-15..+15, 0=normal}
function  ThreadGetPriority (threadHandle : dword): Integer;
function  GetCurrentThreadId : dword;


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

{
  $Log$
  Revision 1.19  2004-09-19 18:55:30  armin
  * added define DISABLE_NO_THREAD_MANAGER to avoid warnings if thread manager is always present

  Revision 1.18  2004/05/23 20:26:20  marco
   * wrappers and nothread prototypes for the basic* functions

  Revision 1.17  2004/05/23 15:30:29  marco
   * first try basicevent

  Revision 1.16  2004/02/22 23:22:49  florian
    * fixed BeginThread on unix

  Revision 1.15  2004/02/22 16:48:39  florian
    * several 64 bit issues fixed

  Revision 1.14  2003/11/29 17:29:32  michael
  + Added overloaded version of SetThreadManager without old parameter

  Revision 1.13  2003/11/27 10:28:41  michael
  + Patch from peter to fix make cycle

  Revision 1.12  2003/11/26 20:10:59  michael
  + New threadmanager implementation

  Revision 1.11  2003/10/01 21:00:09  peter
    * GetCurrentThreadHandle renamed to GetCurrentThreadId

  Revision 1.10  2003/03/27 17:14:27  armin
  * more platform independent thread routines, needs to be implemented for unix

  Revision 1.9  2002/10/16 19:04:27  michael
  + More system-independent thread routines

  Revision 1.8  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.7  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.6  2002/07/28 20:43:48  florian
    * several fixes for linux/powerpc
    * several fixes to MT
}
