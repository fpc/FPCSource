{
    $Id$
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

    procedure InitThread(stklen:cardinal);
      begin
        SysResetFPU;
        { ExceptAddrStack and ExceptObjectStack are threadvars       }
        { so every thread has its on exception handling capabilities }
        SysInitExceptions;
        { Open all stdio fds again }
        SysInitStdio;
        InOutRes:=0;
        // ErrNo:=0;
        { Stack checking }
        StackLength:=stklen;
        StackBottom:=Sptr - StackLength;
        ThreadID := CurrentTM.GetCurrentThreadID();
      end;

{*****************************************************************************
                            Overloaded functions
*****************************************************************************}
{$ifndef CPU64}
{$ifndef unix}
    function BeginThread(sa : Pointer;stacksize : dword;
                         ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
                         var ThreadId : Longint) : DWord;
      begin
        BeginThread:=BeginThread(nil,StackSize,ThreadFunction,p,creationFlags,THandle(THreadId));
      end;
{$endif unix}
{$endif CPU64}

    function BeginThread(ThreadFunction : tthreadfunc) : DWord;
      var
        dummy : THandle;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,nil,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer) : DWord;
      var
        dummy : THandle;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,dummy);
      end;


    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : THandle) : DWord;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,ThreadId);
      end;


{$ifndef CPU64}
{$ifndef unix}
    function BeginThread(ThreadFunction : tthreadfunc;p : pointer;var ThreadId : Longint) : DWord;
      begin
        BeginThread:=BeginThread(nil,DefaultStackSize,ThreadFunction,p,0,THandle(ThreadId));
      end;
{$endif unix}
{$endif CPU64}

    procedure EndThread;
      begin
        EndThread(0);
      end;

function BeginThread(sa : Pointer;stacksize : dword; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;  var ThreadId : THandle) : DWord;

begin
  Result:=CurrentTM.BeginThread(sa,stacksize,threadfunction,P,creationflags,ThreadID);
end;

procedure EndThread(ExitCode : DWord);

begin
  CurrentTM.EndThread(ExitCode);
end;

function  SuspendThread (threadHandle : dword) : dword;

begin
  Result:=CurrentTM.SuspendThread(ThreadHandle);
end;

function ResumeThread  (threadHandle : dword) : dword;

begin
  Result:=CurrentTM.ResumeThread(ThreadHandle);
end;

procedure ThreadSwitch;

begin
  CurrentTM.ThreadSwitch;
end;

function  KillThread (threadHandle : dword) : dword;

begin
  Result:=CurrentTM.KillThread(ThreadHandle);
end;

function  WaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;

begin
  Result:=CurrentTM.WaitForThreadTerminate(ThreadHandle,TimeOutMS);
end;

function  ThreadSetPriority (threadHandle : dword; Prio: longint): boolean;
begin
  Result:=CurrentTM.ThreadSetPriority(ThreadHandle,Prio);
end;

function  ThreadGetPriority (threadHandle : dword): Integer;

begin
  Result:=CurrentTM.ThreadGetPriority(ThreadHandle);
end;

function  GetCurrentThreadId : dword;

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

procedure RTLeventStartWait(state:pRTLEvent);

begin
  currenttm.rtleventStartWait(state);
end;

procedure RTLeventWaitFor(state:pRTLEvent);

begin
  currenttm.rtleventWaitFor(state);
end;

procedure RTLeventsync(m:trtlmethod;p:tprocedure);

begin
  currenttm.rtleventsync(m,p);
end;

procedure RTLchecksynchronize;

begin
 currenttm.rtlchksyncunix;
end;


{ ---------------------------------------------------------------------
    ThreadManager which gives run-time error. Use if no thread support.
  ---------------------------------------------------------------------}

{$ifndef DISABLE_NO_THREAD_MANAGER}

Resourcestring
  SNoThreads = 'This binary has no thread support compiled in.';
  SRecompileWithThreads = 'Recompile the application with a thread-driver in the program uses clause before other units using thread.';

Procedure NoThreadError;

begin
  If IsConsole then
    begin
    Writeln(StdErr,SNoThreads);
    Writeln(StdErr,SRecompileWithThreads);
    end;
  RunError(232)
end;

function NoBeginThread(sa : Pointer;stacksize : dword;
                     ThreadFunction : tthreadfunc;p : pointer;
                     creationFlags : dword; var ThreadId : THandle) : DWord;
begin
  NoThreadError;
end;

procedure NoEndThread(ExitCode : DWord);
begin
  NoThreadError;
end;

function  NoThreadHandler (threadHandle : dword) : dword;
begin
  NoThreadError;
end;

procedure NoThreadSwitch;  {give time to other threads}
begin
  NoThreadError;
end;

function  NoWaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;  {0=no timeout}
begin
  NoThreadError;
end;

function  NoThreadSetPriority (threadHandle : dword; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  NoThreadError;
end;

function  NoThreadGetPriority (threadHandle : dword): Integer;
begin
  NoThreadError;
end;

function  NoGetCurrentThreadId : dword;
begin
  NoThreadError;
end;

procedure NoCriticalSection(var CS);

begin
  if IsMultiThread then
    NoThreadError;
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
  NoThreadError;
end;

procedure NORTLeventdestroy(state:pRTLEvent);

begin
  NoThreadError;
end;

procedure NORTLeventSetEvent(state:pRTLEvent);

begin
  NoThreadError;
end;

procedure NORTLeventStartWait(state:pRTLEvent);

begin
  NoThreadError;
end;

procedure NORTLeventWaitFor(state:pRTLEvent);

begin
  NoThreadError;
end;

procedure NORTLeventsync(m:trtlmethod;p:tprocedure);

begin
  NoThreadError;
end;


procedure NORTLChkSyncUnix;

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
    rtleventStartWait      :=@NortleventStartWait;
    rtleventWaitFor        :=@NortleventWaitFor;
    rtleventsync	   :=@Nortleventsync;
    rtlchksyncunix	   :=@nortlchksyncunix;
    end;
  SetThreadManager(NoThreadManager);
end;
{$endif DISABLE_NO_THREAD_MANAGER}

{
  $Log$
  Revision 1.18  2005-01-16 14:46:57  florian
    * critical sections can be used in programs without threading driver, they have no effect then

  Revision 1.17  2004/12/28 14:20:03  marco
   * tthread patch from neli

  Revision 1.16  2004/12/27 15:28:40  marco
   * checksynchronize now in interface win32 uses the default impl.
       unix uses systhrds, rest empty implementation.

  Revision 1.15  2004/12/23 20:58:22  peter
    * fix rtlcreateevent

  Revision 1.14  2004/12/23 15:08:58  marco
   * 2nd synchronize attempt. cthreads<->systhrds difference was not ok, but
     only showed on make install should be fixed now.

  Revision 1.13  2004/12/22 21:29:24  marco
   * rtlevent kraam. Checked (compile): Linux, FreeBSD, Darwin, Windows
  	Check work: ask Neli.

  Revision 1.12  2004/09/19 18:55:30  armin
  * added define DISABLE_NO_THREAD_MANAGER to avoid warnings if thread manager is always present

  Revision 1.11  2004/05/23 20:26:20  marco
   * wrappers and nothread prototypes for the basic* functions

  Revision 1.10  2004/02/22 23:22:49  florian
    * fixed BeginThread on unix

  Revision 1.9  2004/02/22 16:48:39  florian
    * several 64 bit issues fixed

  Revision 1.8  2004/01/21 20:11:06  peter
    * fixed compile for unix

  Revision 1.7  2004/01/20 23:13:53  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.6  2003/11/29 17:33:09  michael
  + Removed dummy variable from SetNothreadManager

  Revision 1.5  2003/11/29 17:29:32  michael
  + Added overloaded version of SetThreadManager without old parameter

  Revision 1.4  2003/11/26 20:10:59  michael
  + New threadmanager implementation

  Revision 1.3  2002/11/14 12:40:06  jonas
    * the BeginThread() variant that allowed you to specify the stacksize
      still passed DefaultStackSize to the OS-specific routines

  Revision 1.2  2002/10/16 19:04:27  michael
  + More system-independent thread routines

  Revision 1.1  2002/10/14 19:39:17  peter
    * threads unit added for thread support

}
