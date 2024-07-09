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
procedure Sleep(dwMilliseconds: DWord); {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'Sleep';
function  WinSuspendThread (threadHandle : THandle) : dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SuspendThread';
function  WinResumeThread  (threadHandle : THandle) : dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'ResumeThread';
function  WinCloseHandle  (threadHandle : THandle) : dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'CloseHandle';
function  TerminateThread  (threadHandle : THandle; var exitCode : dword) : boolean; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TerminateThread';
function  WaitForSingleObject (hHandle : THandle;Milliseconds: dword): dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'WaitForSingleObject';
function  WinThreadSetPriority (threadHandle : THandle; Prio: longint): boolean; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SetThreadPriority';
function  WinThreadGetPriority (threadHandle : THandle): LongInt; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetThreadPriority';
{$ifndef WINCE}
function WinGetCurrentThread: THandle; stdcall; external KernelDLL name 'GetCurrentThread';
function WinOpenThread(dwDesiredAccess: DWord; bInheritHandle: Boolean; dwThreadId: DWord): THandle; stdcall; external KernelDLL name 'OpenThread';
function WinIsDebuggerPresent: Boolean; stdcall; external KernelDLL name 'IsDebuggerPresent';
type
  TSetThreadDescription = function(threadHandle: THandle; lpThreadDescription: PWideChar): HResult; stdcall;
var
  WinSetThreadDescription: TSetThreadDescription;
function  CreateEvent(lpEventAttributes:pointer;bManualReset:longbool;bInitialState:longbool;lpName:PAnsiChar): THandle; stdcall; external KernelDLL name 'CreateEventA';
function  ResetEvent(hEvent:THandle):LONGBOOL; stdcall; external KernelDLL name 'ResetEvent';
function  SetEvent(hEvent:THandle):LONGBOOL; stdcall; external KernelDLL name 'SetEvent';
{$endif WINCE}

procedure WinInitCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'InitializeCriticalSection';

procedure WinDoneCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'DeleteCriticalSection';

procedure WinEnterCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'EnterCriticalSection';

procedure WinLeaveCriticalSection(var cs : TRTLCriticalSection);
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'LeaveCriticalSection';

CONST
   WAIT_OBJECT_0 = 0;
   WAIT_TIMEOUT = $102;

function WinTryEnterCriticalSection(var cs : TRTLCriticalSection):longint;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TryEnterCriticalSection';

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

    var
      // public names are used by heaptrc unit
      threadvarblocksize : dword; public name '_FPC_TlsSize';
      {$ifdef FPC_HAS_INDIRECT_ENTRY_INFORMATION}
      TLSKey : PDword = nil; public name '_FPC_TlsKey';
      {$else FPC_HAS_INDIRECT_ENTRY_INFORMATION}
      TLSKeyVar : DWord = $ffffffff;
      TLSKey : PDWord = @TLSKeyVar; public name '_FPC_TlsKey';
      {$endif FPC_HAS_INDIRECT_ENTRY_INFORMATION}

    var
      MainThreadIdWin32 : DWORD;

    procedure SysInitThreadvar(var offset : dword;size : dword);
      begin
        offset:=threadvarblocksize;
      {$ifdef CPUARM}
        // Data must be allocated at 4 bytes boundary for ARM
        size:=(size + 3) and not dword(3);
      {$endif CPUARM}
        inc(threadvarblocksize,size);
      end;


    procedure SysAllocateThreadVars; public name '_FPC_SysAllocateThreadVars';
      var
        dataindex : pointer;
        errorsave : dword;
      begin
        { we've to allocate the memory from system  }
        { because the FPC heap management uses      }
        { exceptions which use threadvars but       }
        { these aren't allocated yet ...            }
        { allocate room on the heap for the thread vars }
        errorsave:=GetLastError;
        if tlskey^=$ffffffff then
          RunError(226);
        dataindex:=TlsGetValue(tlskey^);
        if dataindex=nil then
          begin
            dataindex:=HeapAlloc(GetProcessHeap,HEAP_ZERO_MEMORY,threadvarblocksize);
            if dataindex=nil then
              RunError(226);
            TlsSetValue(tlskey^,dataindex);
          end;
        SetLastError(errorsave);
      end;

    function SysRelocateThreadvar(offset : dword) : pointer; forward;

    procedure SysInitTLS;
      begin
        { do not check IsMultiThread, as program could have altered it, out of Delphi habit }

        { the thread attach/detach code uses locks to avoid multiple calls of this }
        if TLSKey^=$ffffffff then
         begin
           { We're still running in single thread mode, setup the TLS }
           TLSKey^:=TlsAlloc;
           InitThreadVars(@SysRelocateThreadvar);
         end;
      end;


    procedure SysFiniMultithreading;
      begin
        if TLSKey^<>$ffffffff then
          TlsFree(TLSKey^);
        TLSKey^:=$ffffffff;
      end;


{ Directly access thread environment block (TEB). If there is a value, use it. If there is not, jump to TrulyRelocateThreadvar that can allocate it.
  TrulyRelocateThreadvar is several (5+) times slower by itself; shortcutting SetLastError on errorsave = 0 helps a bit (reduces to 3.5× maybe :D).

  General info (in particular, stories about de facto stability guarantees):
  https://en.wikipedia.org/wiki/Win32_Thread_Information_Block

  TEB layout:
  https://github.com/wine-mirror/wine/blob/badaed641928edb8f2426d9f12d16c88b479e1e8/include/winternl.h#L431

  “Why load fs:[0x18] into a register and then dereference that, instead of just going for fs:[n] directly?”
  https://devblogs.microsoft.com/oldnewthing/20220919-00/?p=107195
  TL;DR: even in Windows sources, TlsGetValue is written in relatively high-level manner and not overly optimized. }

{$undef FPC_HAS_SYSRELOCATETHREADVAR_ASM}

{$ifndef wince} { Don’t know a thing, maybe WinCE TEB is compatible... :D https://stackoverflow.com/questions/1099311/windows-ce-internals-teb-thread-environment-block }
{$if defined(cpui386)}
  {$define FPC_HAS_SYSRELOCATETHREADVAR_ASM}
    function TrulyRelocateThreadvar(offset : dword) : pointer; forward;

    function SysRelocateThreadvar(offset : dword) : pointer; assembler; nostackframe;
      { eax = offset }
      const
        TlsSlots = $E10; { void* TlsSlots[64] @ fs:[E10h]. }
        TlsExpansionSlots = $F94; { void** TlsExpansionSlots @ fs:[F94h] }
      asm
        mov  TLSKey, %edx
        mov  (%edx), %edx { edx = TLSKey^. }

        cmp  $0x40, %edx { There are 64 static slots + 1024 dynamic slots. }
        jae  .LExp
        mov  %fs:TlsSlots(,%edx,4), %edx { Read TLSKey^-th slot. }
        test %edx, %edx
        jz   .LOops
        add  %edx, %eax { result := TlsGetValue(TLSKey^) + offset. }
        ret

.LOops: jmp  TrulyRelocateThreadvar { Save on relative jumps :) }

.LExp:  cmp  $0x440, %edx
        jae  .LOops { Will fail as 0x440 = 1088 = 64 static + 1024 dynamic is the limit on TLS indices. }
        mov  %fs:TlsExpansionSlots, %ecx { ecx = TlsExpansionSlots. }
        test %ecx, %ecx
        jz   .LOops { No TlsExpansionSlots allocated. }
        mov  -0x100(%ecx,%edx,4), %edx { Read (TLSKey^ − 64)-th dynamic slot. }
        test %edx, %edx
        jz   .LOops
        add  %edx, %eax { result := TlsGetValue(TLSKey^) + offset. }
      end;
{$elseif defined(cpux86_64)}
  {$define FPC_HAS_SYSRELOCATETHREADVAR_ASM}
    function TrulyRelocateThreadvar(offset : dword) : pointer; forward;

    function SysRelocateThreadvar(offset : dword) : pointer; assembler; nostackframe;
      { ecx = offset }
      const { Same as above but 64-bit: TEB pointer is in GS register, different offsets. }
        TlsSlots = $1480;
        TlsExpansionSlots = $1780;
      asm
        mov  TLSKey(%rip), %rdx
        mov  (%rdx), %edx { edx = TLSKey^. }

        cmp  $0x40, %edx
        jae  .LExp
        mov  %gs:TlsSlots(,%rdx,8), %rax
        test %rax, %rax
        jz   .LOops
        add  %rcx, %rax { Hopefully offset is zero-extended on entry. }
        ret

.LOops: jmp  TrulyRelocateThreadvar

.LExp:  cmp  $0x440, %edx
        jae  .LOops
        mov  %gs:TlsExpansionSlots, %rax
        test %rax, %rax
        jz   .LOops
        mov  -0x200(%rax,%rdx,8), %rax
        test %rax, %rax
        jz   .LOops
        add  %rcx, %rax
      end;
{$endif implement SysRelocateThreadvar with assembly}
{$endif not wince}


    function {$ifdef FPC_HAS_SYSRELOCATETHREADVAR_ASM} TrulyRelocateThreadvar {$else} SysRelocateThreadvar {$endif} (offset : dword) : pointer;
      var
        dataindex : pointer;
        errorsave : dword;
      begin
        errorsave:=GetLastError;
        dataindex:=TlsGetValue(tlskey^);
        if dataindex=nil then
          begin
            SysAllocateThreadVars;
            dataindex:=TlsGetValue(tlskey^);
            InitThread($1000000);
          end;
        SetLastError(errorsave);
        Result:=DataIndex+Offset;
      end;


    procedure SysReleaseThreadVars;
      var
        p: pointer;
      begin
        if TLSKey^<>$ffffffff then
          begin
            p:=TlsGetValue(tlskey^);
            HeapFree(GetProcessHeap,0,p); { HeapFree is OK with nil. }
            TlsSetValue(tlskey^, nil);
          end;
      end;


{*****************************************************************************
                            Thread starting
*****************************************************************************}

    type
      pthreadinfo = ^tthreadinfo;
      tthreadinfo = record
        f : tthreadfunc;
        p : pointer;
        stklen : ptruint;
      end;

    function ThreadMain(param : pointer) : Longint; {$ifdef wince}cdecl{$else}stdcall{$endif};
      var
        ti : tthreadinfo;
      begin
        { Copy parameter to local data }
        ti:=pthreadinfo(param)^;

        { Handle all possible threadvar models:
          - dynamic threadvars: initialized either in DllMain,
            or upon accessing the threadvar ThreadID;
          - static threadvars+TLS callback: initialized in TLS callback;
          - static threadvars, no callback: ThreadID remains 0 and
            initialization happens here. }
        if ThreadID=TThreadID(0) then
          begin
            InitThread(ti.stklen);
{$ifndef wince}
{$ifdef win32}
            if Assigned(SetThreadStackGuarantee) then
{$endif win32}
              SetThreadStackGuaranteeTo(StackMargin);
{$endif wince}            
          end;

        dispose(pthreadinfo(param));

        { Start thread function }
{$ifdef DEBUG_MT}
        writeln('Jumping to thread function of thread ',Win32GetCurrentThreadId);
{$endif DEBUG_MT}
{$if defined(FPC_USE_WIN64_SEH) or defined(FPC_USE_WIN32_SEH)}
        { use special 'top-level' exception handler around the thread function }
        ThreadMain:=main_wrapper(ti.p,pointer(ti.f));
{$else FPC_USE_WIN64_SEH}
        ThreadMain:=ti.f(ti.p);
{$endif FPC_USE_WIN64_SEH or FPC_USE_WIN32_SEH}
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
        SysInitTLS;
        if not IsMultiThread then
          begin
           { lazy initialize thread support }
            LazyInitThreading;
            IsMultiThread:=true;
          end;

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

    function  SysCloseThread (threadHandle : TThreadID) : dword;
    begin
      SysCloseThread:=winCloseHandle(threadHandle);
    end;

    function  SysWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;
    begin
      // shouldn't this be a msgwait in case the thread creates "Windows"  See comment in waitforsingle?
      if timeoutMs = 0 then dec (timeoutMs);  // $ffffffff is INFINITE
      // does waiting on thread require cowait too ?
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

{$ifndef WINCE}
    { following method is supported on older Windows versions AND currently only supported method by GDB }
    procedure RaiseMSVCExceptionMethod(threadHandle: TThreadID; const ThreadName: AnsiString);
    const
      MS_VC_EXCEPTION: DWord = $406D1388;
    type
      THREADNAME_INFO = record
        dwType: DWord; // Must be 0x1000.
        szName: PAnsiChar; // Pointer to name (in user addr space).
        dwThreadID: DWord; // Thread ID (-1=caller thread).
        dwFlags: DWord; // Reserved for future use, must be zero.
      end;
    var
      thrdinfo: THREADNAME_INFO;
    begin
      thrdinfo.dwType:=$1000;
      thrdinfo.szName:=@ThreadName[1];
      thrdinfo.dwThreadID:=threadHandle;
      thrdinfo.dwFlags:=0;
      try
        RaiseException(MS_VC_EXCEPTION, 0, SizeOf(thrdinfo) div SizeOf(PtrUInt), @thrdinfo);
      except
        {do nothing}
      end;
    end;

    { following method needs at least Windows 10 version 1607 or Windows Server 2016 }
    procedure SetThreadDescriptionMethod(threadHandle: TThreadID; const ThreadName: UnicodeString);
    var
      thrdhandle: THandle;
      ClosingNeeded: Boolean;
    begin
      if threadHandle=TThreadID(-1) then
      begin
        thrdhandle:=WinGetCurrentThread;
        ClosingNeeded:=False;
      end
      else
      begin
        thrdhandle:=WinOpenThread($0400, False, threadHandle);
        ClosingNeeded:=True;
      end;

      WinSetThreadDescription(thrdhandle, @ThreadName[1]);

      if ClosingNeeded then
      begin
        CloseHandle(thrdhandle);
      end;
    end;
{$endif WINCE}

    procedure SysSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);
    begin
{$ifndef WINCE}
      if ThreadName = '' then
        Exit;

      if WinIsDebuggerPresent then
      begin
        RaiseMSVCExceptionMethod(threadHandle, ThreadName);
      end;

      if Assigned(WinSetThreadDescription) then
      begin
        SetThreadDescriptionMethod(threadHandle, UnicodeString(ThreadName));
      end;
{$else WINCE}
      {$Warning SetThreadDebugNameA needs to be implemented}
{$endif WINCE}
    end;

    procedure SysSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);
    begin
{$ifndef WINCE}
      if ThreadName = '' then
        Exit;

      if WinIsDebuggerPresent then
      begin
        RaiseMSVCExceptionMethod(threadHandle, AnsiString(ThreadName));
      end;

      if Assigned(WinSetThreadDescription) then
      begin
        SetThreadDescriptionMethod(threadHandle, ThreadName);
      end;
{$else WINCE}
      {$Warning SetThreadDebugNameU needs to be implemented}
{$endif WINCE}
    end;

{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

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

function SysTryEnterCriticalSection(var cs):longint;
begin
  result:=WinTryEnterCriticalSection(PRTLCriticalSection(@cs)^);
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

function intBasicEventCreate(EventAttributes : Pointer;
AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
begin
  Result := PEventState(CreateEvent(EventAttributes, AManualReset, InitialState,Pointer(Name)));
end;

procedure intbasiceventdestroy(state:peventstate);

begin
  closehandle(THandle(state));
end;

procedure intbasiceventResetEvent(state:peventstate);

begin
  ResetEvent(THandle(state))
end;

procedure intbasiceventSetEvent(state:peventstate);

begin
  SetEvent(THandle(state));
end;

function FirstCoWaitForMultipleHandles(dwFlags, dwTimeout: DWORD; cHandles: uint32; pHandles: PWOHandleArray; out lpdwindex: DWORD): HRESULT; stdcall; forward;

var
  Ole32Dll: THandle = 0; { Unloaded at win32 & win64 system_exit. }
  CoWaitForMultipleHandlesImpl: function(dwFlags, dwTimeout: DWORD; cHandles: uint32; pHandles: PWOHandleArray; out lpdwindex: DWORD): HRESULT; stdcall
    = @FirstCoWaitForMultipleHandles;

function FirstCoWaitForMultipleHandles(dwFlags, dwTimeout: DWORD; cHandles: uint32; pHandles: PWOHandleArray; out lpdwindex: DWORD): HRESULT; stdcall;
var
  LocalOle32Dll: THandle;
begin
  if Ole32Dll = 0 then
  begin
    LocalOle32Dll := WinLoadLibraryW('ole32.dll');
    if InterlockedCompareExchange(Pointer(Ole32Dll), Pointer(LocalOle32Dll), nil) <> nil then
      WinFreeLibrary(LocalOle32Dll);
  end;
  CodePointer(CoWaitForMultipleHandlesImpl) := WinGetProcAddress(Ole32Dll, 'CoWaitForMultipleHandles');
  Result := CoWaitForMultipleHandlesImpl(dwFlags, dwTimeout, cHandles, pHandles, lpdwindex);
end;

function CoWaitForMultipleHandles(dwFlags, dwTimeout: DWORD; cHandles: uint32; pHandles: PWOHandleArray; out lpdwindex: DWORD): HRESULT;
begin
  Result := CoWaitForMultipleHandlesImpl(dwFlags, dwTimeout, cHandles, pHandles, lpdwindex);
end;

function intbasiceventWaitFor(Timeout : Cardinal;state:peventstate;UseCOMWait: Boolean = False) : longint;
const COWAIT_DEFAULT = 0;
      RPC_S_CALLPENDING = HRESULT($80010115);
var SignaledIndex : DWORD;
begin
   if UseComWait Then
     case CoWaitForMultipleHandles(COWAIT_DEFAULT, Timeout, 1, PWOHandleArray(@state), SignaledIndex) of
       S_OK: Result := wrSignaled;
       RPC_S_CALLPENDING: Result := wrTimeout;
       else Result := wrError;
     end
   else 
     case WaitForSingleObject(THandle(state), Timeout) of
       WAIT_OBJECT_0: Result := wrSignaled;
       WAIT_TIMEOUT: Result := wrTimeout;
       else result := wrError; { WAIT_FAILED or any other value. Note that only mutex waits can return WAIT_ABANDONED. }
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


Procedure InitSystemThreads;public name '_FPC_InitSystemThreads';
var
  WinThreadManager : TThreadManager;
{$ifndef WINCE}
  KernelHandle : THandle;
{$endif}
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
    CloseThread            :=@SysCloseThread;
    WaitForThreadTerminate :=@SysWaitForThreadTerminate;
    ThreadSetPriority      :=@SysThreadSetPriority;
    ThreadGetPriority      :=@SysThreadGetPriority;
    GetCurrentThreadId     :=@SysGetCurrentThreadId;
    SetThreadDebugNameA    :=@SysSetThreadDebugNameA;
    SetThreadDebugNameU    :=@SysSetThreadDebugNameU;
    InitCriticalSection    :=@SysInitCriticalSection;
    DoneCriticalSection    :=@SysDoneCriticalSection;
    EnterCriticalSection   :=@SysEnterCriticalSection;
    TryEnterCriticalSection:=@SysTryEnterCriticalSection;
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
{$ifndef FPC_USE_TLS_DIRECTORY}
  if IsLibrary then
{$endif}
    SysInitTLS;

{$ifndef WINCE}
  KernelHandle:=GetModuleHandle(KernelDLL);
{$endif}

{$ifndef WINCE}
  if KernelHandle<>0 then
  begin
    WinSetThreadDescription:=TSetThreadDescription(WinGetProcAddress(KernelHandle, 'SetThreadDescription'));
  end;
{$endif WINCE}
end;

