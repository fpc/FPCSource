{
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
  { LocalAlloc flags  }
  LMEM_FIXED = 0;
  LMEM_ZEROINIT = 64;

{$ifndef WINCE}
function TlsAlloc : DWord;
  stdcall;external KernelDLL name 'TlsAlloc';
function TlsFree(dwTlsIndex : DWord) : LongBool;
  stdcall;external KernelDLL name 'TlsFree';
{$endif WINCE}
function TlsGetValue(dwTlsIndex : DWord) : pointer;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TlsGetValue';
function TlsSetValue(dwTlsIndex : DWord;lpTlsValue : pointer) : LongBool;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TlsSetValue';
function CreateThread(lpThreadAttributes : pointer;
  dwStackSize : SIZE_T; lpStartAddress : pointer;lpParameter : pointer;
  dwCreationFlags : DWord;var lpThreadId : DWord) : THandle;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'CreateThread';
procedure ExitThread(dwExitCode : DWord);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'ExitThread';
function LocalAlloc(uFlags:DWord; dwBytes:DWORD):Pointer;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'LocalAlloc';
function LocalFree(hMem : Pointer):Pointer; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'LocalFree';
procedure Sleep(dwMilliseconds: DWord); {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'Sleep';
function  WinSuspendThread (threadHandle : THandle) : dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SuspendThread';
function  WinResumeThread  (threadHandle : THandle) : dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'ResumeThread';
function  TerminateThread  (threadHandle : THandle; var exitCode : dword) : boolean; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TerminateThread';
function  WaitForSingleObject (hHandle : THandle;Milliseconds: dword): dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'WaitForSingleObject';
function  WinThreadSetPriority (threadHandle : THandle; Prio: longint): boolean; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SetThreadPriority';
function  WinThreadGetPriority (threadHandle : THandle): LongInt; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetThreadPriority';
{$ifndef WINCE}
function  CreateEvent(lpEventAttributes:pointer;bManualReset:longbool;bInitialState:longbool;lpName:pchar): THandle; stdcall; external KernelDLL name 'CreateEventA';
function  ResetEvent(hEvent:THandle):LONGBOOL; stdcall; external KernelDLL name 'ResetEvent';
function  SetEvent(hEvent:THandle):LONGBOOL; stdcall; external KernelDLL name 'SetEvent';
{$endif WINCE}

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

    const
      threadvarblocksize : dword = 0;

    const
      TLSKey : DWord = $ffffffff;

    procedure SysInitThreadvar(var offset : dword;size : dword);
      begin
        offset:=threadvarblocksize;
      {$ifdef CPUARM}
        // Data must be allocated at 4 bytes boundary for ARM
        size:=(size + 3) and not dword(3);
      {$endif CPUARM}
        inc(threadvarblocksize,size);
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
        dataindex:=pointer(LocalAlloc(LMEM_FIXED or LMEM_ZEROINIT,threadvarblocksize));
        TlsSetValue(tlskey,dataindex);
      end;


    function SysRelocateThreadvar(offset : dword) : pointer;
      var
        dataindex : pointer;
        errorsave : dword;
      begin	    
{$ifdef dummy}
        { it least in the on windows 7 x64, this still doesn't not work, fs:(0x2c) is
          self referencing on this system (FK) }
        asm
          movl TLSKey,%edx
          movl %fs:(0x2c),%eax
          orl  %eax,%eax
          jnz  .LAddressInEAX
		  { this works on Windows 7, but I don't know if it works on other OSes (FK) }
          movl %fs:(0x18),%eax
          movl 0xe10(%eax,%edx,4),%eax
          jmp  .LToDataIndex
          .LAddressInEAX:
           movl (%eax,%edx,4),%eax
          .LToDataIndex:
          movl %eax,dataindex
        end;
        if DataIndex=nil then
          begin
            errorsave:=GetLastError;
            SysAllocateThreadVars;
            DataIndex:=TlsGetValue(tlskey);
            SetLastError(errorsave);
          end;
{$else win32}
        errorsave:=GetLastError;
        dataindex:=TlsGetValue(tlskey);
        if dataindex=nil then
          begin
            SysAllocateThreadVars;
            dataindex:=TlsGetValue(tlskey);
          end;
        SetLastError(errorsave);
{$endif win32}
        SysRelocateThreadvar:=DataIndex+Offset;
      end;


    procedure SysReleaseThreadVars;
      begin
        LocalFree(TlsGetValue(tlskey));
        TlsSetValue(tlskey, nil);
      end;


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

    function ThreadMain(param : pointer) : Longint; {$ifdef wince}cdecl{$else}stdcall{$endif};
      var
        ti : tthreadinfo;
      begin
        { Allocate local thread vars, this must be the first thing,
          because the exception management and io depends on threadvars }
        SysAllocateThreadVars;

        { Copy parameter to local data }
        ti:=pthreadinfo(param)^;
        dispose(pthreadinfo(param));

        { Initialize thread }
        InitThread(ti.stklen);

        { Start thread function }
{$ifdef DEBUG_MT}
        writeln('Jumping to thread function of thread ',Win32GetCurrentThreadId);
{$endif DEBUG_MT}
        ThreadMain:=ti.f(ti.p);
      end;

    procedure SysInitMultithreading;
      begin
        { do not check IsMultiThread, as program could have altered it, out of Delphi habit }
        
        { the thread attach/detach code uses locks to avoid multiple calls of this }
        if TLSKey=$ffffffff then
         begin
           { We're still running in single thread mode, setup the TLS }
           TLSKey:=TlsAlloc;
           InitThreadVars(@SysRelocateThreadvar);
		   { allocate the thread vars for the main thread }
           IsMultiThread:=true;
         end;
      end;

    procedure SysFiniMultithreading;
      begin
        if IsMultiThread then
          begin
            TlsFree(TLSKey);
            TLSKey:=$ffffffff;
          end;
      end;

    function SysBeginThread(sa : Pointer;stacksize : ptruint;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword;var ThreadId : TThreadID) : TThreadID;
      var
        ti : pthreadinfo;
        _threadid : dword;
      begin
{$ifdef DEBUG_MT}
        writeln('Creating new thread');
{$endif DEBUG_MT}
        { Initialize multithreading if not done }
        SysInitMultithreading;

        { the only way to pass data to the newly created thread
          in a MT safe way, is to use the heap }
        new(ti);
        ti^.f:=ThreadFunction;
        ti^.p:=p;
        ti^.stklen:=stacksize;
{$ifdef DEBUG_MT}
        writeln('Starting new thread');
{$endif DEBUG_MT}
        _threadid:=0;
        SysBeginThread:=CreateThread(sa,stacksize,@ThreadMain,ti,creationflags,_threadid);

        { creation failed? if yes, we dispose the parameter record }
        if SysBeginThread=0 then
          begin
{$ifdef DEBUG_MT}
            writeln('Thread creation failed');
{$endif DEBUG_MT}
            dispose(ti);
          end;

        ThreadID:=_threadid;
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


    function  SysSuspendThread (threadHandle : TThreadID) : dword;
    begin
      SysSuspendThread:=WinSuspendThread(threadHandle);
    end;


    function  SysResumeThread  (threadHandle : TThreadID) : dword;
    begin
      SysResumeThread:=WinResumeThread(threadHandle);
    end;


    function  SysKillThread (threadHandle : TThreadID) : dword;
    var exitCode : dword;
    begin
      if not TerminateThread (threadHandle, exitCode) then
        SysKillThread := GetLastError
      else
        SysKillThread := 0;
    end;

    function  SysWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;
    begin
      if timeoutMs = 0 then dec (timeoutMs);  // $ffffffff is INFINITE
      SysWaitForThreadTerminate := WaitForSingleObject(threadHandle, TimeoutMs);
    end;


    function  SysThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean;            {-15..+15, 0=normal}
    begin
      SysThreadSetPriority:=WinThreadSetPriority(threadHandle,Prio);
    end;


    function  SysThreadGetPriority (threadHandle : TThreadID): longint;
    begin
      SysThreadGetPriority:=WinThreadGetPriority(threadHandle);
    end;

    function  SysGetCurrentThreadId : TThreadID;
    begin
      SysGetCurrentThreadId:=Win32GetCurrentThreadId;
    end;

{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

procedure WinInitCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'InitializeCriticalSection';

procedure WinDoneCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'DeleteCriticalSection';

procedure WinEnterCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'EnterCriticalSection';

procedure WinLeaveCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'LeaveCriticalSection';

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

procedure intRTLEventWaitFor(AEvent: PRTLEvent);
const
  INFINITE=dword(-1);
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
    InitThreadVar          :=@SysInitThreadVar;
    RelocateThreadVar      :=@SysRelocateThreadVar;
    AllocateThreadVars     :=@SysAllocateThreadVars;
    ReleaseThreadVars      :=@SysReleaseThreadVars;
    BasicEventCreate       :=@intBasicEventCreate;
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    RTLEventCreate         :=@intRTLEventCreate;
    RTLEventDestroy        :=@intRTLEventDestroy;
    RTLEventSetEvent       :=@intRTLEventSetEvent;
    RTLEventResetEvent     :=@intRTLEventResetEvent;
    RTLEventWaitFor        :=@intRTLEventWaitFor;
    RTLEventWaitForTimeout :=@intRTLEventWaitForTimeout;
    end;
  SetThreadManager(WinThreadManager);
  ThreadID := GetCurrentThreadID;
end;
