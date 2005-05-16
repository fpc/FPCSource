{
    $Id: systhrd.inc,v 1.3 2005/04/09 17:26:08 florian Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    Linux (pthreads) threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Local WINApi imports
*****************************************************************************}

const
  { GlobalAlloc, GlobalFlags  }
  GMEM_FIXED = 0;
  GMEM_ZEROINIT = 64;

function TlsAlloc : DWord;
  stdcall;external 'kernel32' name 'TlsAlloc';
function TlsGetValue(dwTlsIndex : DWord) : pointer;
  stdcall;external 'kernel32' name 'TlsGetValue';
function TlsSetValue(dwTlsIndex : DWord;lpTlsValue : pointer) : LongBool;
  stdcall;external 'kernel32' name 'TlsSetValue';
function TlsFree(dwTlsIndex : DWord) : LongBool;
  stdcall;external 'kernel32' name 'TlsFree';
function CreateThread(lpThreadAttributes : pointer;
  dwStackSize : DWord; lpStartAddress : pointer;lpParameter : pointer;
  dwCreationFlags : DWord;var lpThreadId : DWord) : Dword;
  stdcall;external 'kernel32' name 'CreateThread';
procedure ExitThread(dwExitCode : DWord);
  stdcall;external 'kernel32' name 'ExitThread';
function GlobalAlloc(uFlags:DWord; dwBytes:DWORD):Pointer;
  stdcall;external 'kernel32' name 'GlobalAlloc';
function GlobalFree(hMem : Pointer):Pointer; stdcall;external 'kernel32' name 'GlobalFree';
procedure Sleep(dwMilliseconds: DWord); stdcall;external 'kernel32' name 'Sleep';
function  WinSuspendThread (threadHandle : dword) : dword; stdcall;external 'kernel32' name 'SuspendThread';
function  WinResumeThread  (threadHandle : dword) : dword; stdcall;external 'kernel32' name 'ResumeThread';
function  TerminateThread  (threadHandle : dword; var exitCode : dword) : boolean; stdcall;external 'kernel32' name 'TerminateThread';
function  WaitForSingleObject (hHandle,Milliseconds: dword): dword; stdcall;external 'kernel32' name 'WaitForSingleObject';
function  WinThreadSetPriority (threadHandle : dword; Prio: longint): boolean; stdcall;external 'kernel32' name 'SetThreadPriority';
function  WinThreadGetPriority (threadHandle : dword): LongInt; stdcall;external 'kernel32' name 'GetThreadPriority';
function  WinGetCurrentThreadId : dword; stdcall;external 'kernel32' name 'GetCurrentThread';
function  CreateEvent(lpEventAttributes:pointer;bManualReset:longbool;bInitialState:longbool;lpName:pchar):CARDINAL; stdcall; external 'kernel32' name 'CreateEventA';
function  ResetEvent(hEvent:CARDINAL):LONGBOOL; stdcall; external 'kernel32' name 'ResetEvent';
function  SetEvent(hEvent:CARDINAL):LONGBOOL; stdcall; external 'kernel32' name 'SetEvent';

CONST
   WAIT_OBJECT_0 = 0;
   WAIT_ABANDONED_0 = $80;
   WAIT_TIMEOUT = $102;
   WAIT_IO_COMPLETION = $c0;
   WAIT_ABANDONED = $80;
   WAIT_FAILED = $ffffffff;


{*****************************************************************************
                             Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}
    const
      threadvarblocksize : dword = 0;

    var
      TLSKey : Dword;

    procedure SysInitThreadvar(var offset : dword;size : dword);
      begin
        offset:=threadvarblocksize;
        inc(threadvarblocksize,size);
      end;


    function SysRelocateThreadvar(offset : dword) : pointer;
      begin
        SysRelocateThreadvar:=TlsGetValue(tlskey)+Offset;
      end;


    procedure SysAllocateThreadVars;
      var
        dataindex : pointer;
      begin
        { we've to allocate the memory from system  }
        { because the FPC heap management uses      }
        { exceptions which use threadvars but       }
        { these aren't allocated yet ...            }
        { allocate room on the heap for the thread vars }
        dataindex:=pointer(GlobalAlloc(GMEM_FIXED or GMEM_ZEROINIT,threadvarblocksize));
        TlsSetValue(tlskey,dataindex);
      end;


    procedure SysReleaseThreadVars;
      begin
        GlobalFree(TlsGetValue(tlskey));
      end;

{$endif HASTHREADVAR}


{*****************************************************************************
                            Thread starting
*****************************************************************************}

    type
      pthreadinfo = ^tthreadinfo;
      tthreadinfo = record
        f : tthreadfunc;
        p : pointer;
        stklen : cardinal;
      end;

    procedure DoneThread;
      begin
        { Release Threadvars }
{$ifdef HASTHREADVAR}
        SysReleaseThreadVars;
{$endif HASTHREADVAR}
      end;


    function ThreadMain(param : pointer) : Longint; stdcall;
      var
        ti : tthreadinfo;
      begin
{$ifdef HASTHREADVAR}
        { Allocate local thread vars, this must be the first thing,
          because the exception management and io depends on threadvars }
        SysAllocateThreadVars;
{$endif HASTHREADVAR}
        { Copy parameter to local data }
{$ifdef DEBUG_MT}
        writeln('New thread started, initialising ...');
{$endif DEBUG_MT}
        ti:=pthreadinfo(param)^;
        dispose(pthreadinfo(param));
        { Initialize thread }
        InitThread(ti.stklen);
        { Start thread function }
{$ifdef DEBUG_MT}
        writeln('Jumping to thread function');
{$endif DEBUG_MT}
        ThreadMain:=ti.f(ti.p);
      end;


    function SysBeginThread(sa : Pointer;stacksize : dword;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword; var ThreadId : DWord) : DWord;
      var
        ti : pthreadinfo;
      begin
{$ifdef DEBUG_MT}
        writeln('Creating new thread');
{$endif DEBUG_MT}
        { Initialize multithreading if not done }
        if not IsMultiThread then
         begin
{$ifdef HASTHREADVAR}
           { We're still running in single thread mode, setup the TLS }
           TLSKey:=TlsAlloc;
           InitThreadVars(@SysRelocateThreadvar);
{$endif HASTHREADVAR}
           IsMultiThread:=true;
         end;
        { the only way to pass data to the newly created thread
          in a MT safe way, is to use the heap }
        new(ti);
        ti^.f:=ThreadFunction;
        ti^.p:=p;
        ti^.stklen:=stacksize;
        { call pthread_create }
{$ifdef DEBUG_MT}
        writeln('Starting new thread');
{$endif DEBUG_MT}
        SysBeginThread:=CreateThread(sa,stacksize,@ThreadMain,ti,creationflags,threadid);
      end;


    procedure SysEndThread(ExitCode : DWord);
      begin
        DoneThread;
        ExitThread(ExitCode);
      end;


    procedure SysThreadSwitch;
    begin
      Sleep(0);
    end;


    function  SysSuspendThread (threadHandle : dword) : dword;
    begin
      SysSuspendThread:=WinSuspendThread(threadHandle);
    end;


    function  SysResumeThread  (threadHandle : dword) : dword;
    begin
      SysResumeThread:=WinResumeThread(threadHandle);
    end;


    function  SysKillThread (threadHandle : dword) : dword;
    var exitCode : dword;
    begin
      if not TerminateThread (threadHandle, exitCode) then
        SysKillThread := GetLastError
      else
        SysKillThread := 0;
    end;

    function  SysWaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;
    begin
      if timeoutMs = 0 then dec (timeoutMs);  // $ffffffff is INFINITE
      SysWaitForThreadTerminate := WaitForSingleObject(threadHandle, TimeoutMs);
    end;


    function  SysThreadSetPriority (threadHandle : dword; Prio: longint): boolean;            {-15..+15, 0=normal}
    begin
      SysThreadSetPriority:=WinThreadSetPriority(threadHandle,Prio);
    end;


    function  SysThreadGetPriority (threadHandle : dword): longint;
    begin
      SysThreadGetPriority:=WinThreadGetPriority(threadHandle);
    end;

    function  SysGetCurrentThreadId : dword;
    begin
      SysGetCurrentThreadId:=Win32GetCurrentThreadId;
    end;

{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

procedure WinInitCriticalSection(var cs : TRTLCriticalSection);
  stdcall;external 'kernel32' name 'InitializeCriticalSection';

procedure WinDoneCriticalSection(var cs : TRTLCriticalSection);
  stdcall;external 'kernel32' name 'DeleteCriticalSection';

procedure WinEnterCriticalSection(var cs : TRTLCriticalSection);
  stdcall;external 'kernel32' name 'EnterCriticalSection';

procedure WinLeaveCriticalSection(var cs : TRTLCriticalSection);
  stdcall;external 'kernel32' name 'LeaveCriticalSection';

procedure SySInitCriticalSection(var cs);
begin
  WinInitCriticalSection(PRTLCriticalSection(@cs)^);
end;


procedure SysDoneCriticalSection(var cs);
begin
  WinDoneCriticalSection(PRTLCriticalSection(@cs)^);
end;


procedure SysEnterCriticalSection(var cs);
begin
  WinEnterCriticalSection(PRTLCriticalSection(@cs)^);
end;


procedure SySLeaveCriticalSection(var cs);
begin
  WinLeaveCriticalSection(PRTLCriticalSection(@cs)^);
end;


{*****************************************************************************
                           Heap Mutex Protection
*****************************************************************************}

    var
      HeapMutex : TRTLCriticalSection;

    procedure Win32HeapMutexInit;
      begin
         InitCriticalSection(heapmutex);
      end;

    procedure Win32HeapMutexDone;
      begin
         DoneCriticalSection(heapmutex);
      end;

    procedure Win32HeapMutexLock;
      begin
         EnterCriticalSection(heapmutex);
      end;

    procedure Win32HeapMutexUnlock;
      begin
         LeaveCriticalSection(heapmutex);
      end;

    const
      Win32MemoryMutexManager : TMemoryMutexManager = (
        MutexInit : @Win32HeapMutexInit;
        MutexDone : @Win32HeapMutexDone;
        MutexLock : @Win32HeapMutexLock;
        MutexUnlock : @Win32HeapMutexUnlock;
      );

    procedure InitHeapMutexes;
      begin
        SetMemoryMutexManager(Win32MemoryMutexManager);
      end;

Const
        wrSignaled = 0;
        wrTimeout  = 1;
        wrAbandoned= 2;
        wrError    = 3;

type Tbasiceventstate=record
                        fhandle    : THandle;
                        flasterror : longint;
                       end;
     plocaleventrec= ^tbasiceventstate;

function intBasicEventCreate(EventAttributes : Pointer;
AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;

begin
  new(plocaleventrec(result));
  plocaleventrec(result)^.FHandle := CreateEvent(EventAttributes, AManualReset, InitialState,PChar(Name));
end;

procedure intbasiceventdestroy(state:peventstate);

begin
  closehandle(plocaleventrec(state)^.fhandle);
  dispose(plocaleventrec(state));
end;

procedure intbasiceventResetEvent(state:peventstate);

begin
  ResetEvent(plocaleventrec(state)^.FHandle)
end;

procedure intbasiceventSetEvent(state:peventstate);

begin
  SetEvent(plocaleventrec(state)^.FHandle);
end;

function intbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

begin
  case WaitForSingleObject(plocaleventrec(state)^.fHandle, Timeout) of
    WAIT_ABANDONED: Result := wrAbandoned;
    WAIT_OBJECT_0: Result := wrSignaled;
    WAIT_TIMEOUT: Result := wrTimeout;
    WAIT_FAILED:
        begin
        Result := wrError;
        plocaleventrec(state)^.FLastError := GetLastError;
       end;
  else
    Result := wrError;
  end;
end;

function intRTLEventCreate: PRTLEvent;
begin
  Result := PRTLEVENT(CreateEvent(nil, false, false, nil));
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);
begin
  CloseHandle(THANDLE(AEvent));
end;

procedure intRTLEventSetEvent(AEvent: PRTLEvent);
begin
  SetEvent(THANDLE(AEvent));
end;

procedure intRTLEventResetEvent(AEvent: PRTLEvent);
begin
  ResetEvent(THANDLE(AEvent));
end;

procedure intRTLEventStartWait(AEvent: PRTLEvent);
begin
  { this is to get at least some common behaviour on unix and win32:
    events before startwait are lost on unix, so reset the event on
    win32 as well }
  ResetEvent(THANDLE(AEvent));
end;

procedure intRTLEventWaitFor(AEvent: PRTLEvent);
const
  INFINITE=-1;
begin
  WaitForSingleObject(THANDLE(AEvent), INFINITE);
end;

procedure intRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);
begin
  WaitForSingleObject(THANDLE(AEvent), timeout);
end;


Var
  WinThreadManager : TThreadManager;

Procedure InitSystemThreads;
begin
  With WinThreadManager do
    begin
    InitManager            :=Nil;
    DoneManager            :=Nil;
    BeginThread            :=@SysBeginThread;
    EndThread              :=@SysEndThread;
    SuspendThread          :=@SysSuspendThread;
    ResumeThread           :=@SysResumeThread;
    KillThread             :=@SysKillThread;
    ThreadSwitch           :=@SysThreadSwitch;
    WaitForThreadTerminate :=@SysWaitForThreadTerminate;
    ThreadSetPriority      :=@SysThreadSetPriority;
    ThreadGetPriority      :=@SysThreadGetPriority;
    GetCurrentThreadId     :=@SysGetCurrentThreadId;
    InitCriticalSection    :=@SysInitCriticalSection;
    DoneCriticalSection    :=@SysDoneCriticalSection;
    EnterCriticalSection   :=@SysEnterCriticalSection;
    LeaveCriticalSection   :=@SysLeaveCriticalSection;
{$ifdef HASTHREADVAR}
    InitThreadVar          :=@SysInitThreadVar;
    RelocateThreadVar      :=@SysRelocateThreadVar;
    AllocateThreadVars     :=@SysAllocateThreadVars;
    ReleaseThreadVars      :=@SysReleaseThreadVars;
{$endif HASTHREADVAR}
    BasicEventCreate       :=@intBasicEventCreate;
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    RTLEventCreate         :=@intRTLEventCreate;
    RTLEventDestroy        :=@intRTLEventDestroy;
    RTLEventSetEvent       :=@intRTLEventSetEvent;
    RTLEventResetEvent     :=@intRTLEventResetEvent;
    RTLEventStartWait      :=@intRTLEventStartWait;
    RTLEventWaitFor        :=@intRTLEventWaitFor;
    RTLEventWaitForTimeout :=@intRTLEventWaitForTimeout;
    end;
  SetThreadManager(WinThreadManager);
  InitHeapMutexes;
  ThreadID := GetCurrentThreadID;
end;


{
  $Log: systhrd.inc,v $
  Revision 1.3  2005/04/09 17:26:08  florian
    + classes.mainthreadid is set now
    + rtleventresetevent
    + rtleventwairfor with timeout
    + checksynchronize with timeout
    * race condition in synchronize fixed

  Revision 1.2  2005/02/08 16:28:27  peter
  pulseevent -> setevent

  Revision 1.1  2005/02/06 13:06:20  peter
    * moved file and dir functions to sysfile/sysdir
    * win32 thread in systemunit

}

