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
function  WinCloseHandle  (threadHandle : THandle) : dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'CloseHandle';
function  TerminateThread  (threadHandle : THandle; var exitCode : dword) : boolean; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TerminateThread';
function  WaitForSingleObject (hHandle : THandle;Milliseconds: dword): dword; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'WaitForSingleObject';
{$ifndef wince}
function GetCurrentThread: THandle; external KernelDLL name 'GetCurrentThread';
function  DuplicateHandle (hSourceProcessHandle : THandle; hSourceHandle : THandle; hTargetProcessHandle : THandle; var lpTargetHandle : THandle; dwDesiredAccess : dword; bInheritHandle : longbool; dwOptions : dword) : longbool;{$ifdef wince}cdecl{$else}stdcall{$endif}; external KernelDLL name 'DuplicateHandle';
{$endif}
function  WinThreadSetPriority (threadHandle : THandle; Prio: longint): boolean; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'SetThreadPriority';
function  WinThreadGetPriority (threadHandle : THandle): LongInt; {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'GetThreadPriority';
{$ifndef WINCE}
function  CreateEvent(lpEventAttributes:pointer;bManualReset:longbool;bInitialState:longbool;lpName:pchar): THandle; stdcall; external KernelDLL name 'CreateEventA';
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
   WAIT_ABANDONED_0 = $80;
   WAIT_TIMEOUT = $102;
   WAIT_IO_COMPLETION = $c0;
   WAIT_ABANDONED = $80;
   WAIT_FAILED = $ffffffff;
{$ifndef wince}
   DUPLICATE_SAME_ACCESS = $00000002;
{$endif}

{$ifndef SUPPORT_WIN95}
function WinTryEnterCriticalSection(var cs : TRTLCriticalSection):longint;
  {$ifdef wince}cdecl{$else}stdcall{$endif};external KernelDLL name 'TryEnterCriticalSection';
{$else SUPPORT_WIN95}
type
  TTryEnterCriticalSection = function(var cs : TRTLCriticalSection):longint; stdcall;
var
  WinTryEnterCriticalSection : TTryEnterCriticalSection;
{$endif SUPPORT_WIN95}

{$ifndef wince}
{*****************************************************************************
                         external Threads support
*****************************************************************************}

    const
      ExternalThreadStkLen = $1000000; // some big number
      CollectorWaitTime = 1000; // milliseconds

    type
      PExternalThreadInfo = ^TExternalThreadInfo;
      TExternalThreadInfo = record
        Handle: THandle; // handle of the external thread
                         // this needs to be closed with CloseHandle at the end
        ThreadVarArea: Pointer; // pointer to the threadvar area that was
                                // allocated for this thread
        Next: PExternalThreadInfo; // the next thread info of this list
      end;

    var
      ExternalThreadsList: PExternalThreadInfo = nil; // the head of the list
      ExternalThreadLock: TRTLCriticalSection; // this lock is used to
                                               // serialize access to
                                               // ExternalThreadsList
     TerminateCollector: Boolean = False; // set this to true to terminate the
                                           // collector thread and to disable
                                           // the registration of new external
                                           // threads
      ExternalThreadsDetected: boolean = False; // this is set to true once the
                                                // first external thread is
                                                // detected
      RegisterExternalLock: TRTLCriticalSection; // lock for serializing access
                                                 // to HelperThreads... this is
                                                 // initialized on startup to
                                                 // eliminate potential race
                                                 // condition
      FakeThreadVars: Pointer = nil;  // set to non-Nil to fake a thread in
                                      // SysRelocateThreadVar
      FakeProtection: TRTLCriticalSection; // enter this critical section to
                                           // secure the faking process

{$define debug_collector}

    procedure SysInitCriticalSection(var cs); forward;
    procedure SysEnterCriticalSection(var cs); forward;
    procedure SysLeaveCriticalSection(var cs); forward;

{$ifdef debug_collector}
    procedure WritelnDirect(aStr: ShortString);
    var
      dummy: longint;
    begin
      dummy := 0;
      aStr := aStr + LineEnding;
      WriteFile(StdOutputHandle, @aStr[1], Length(aStr), dummy, Nil);
    end;
{$endif}

    function ExternalThreadCollector(parameter : pointer) : ptrint;
    var
      last, current, tmp, tmplast: PExternalThreadInfo;
      res: LongWord;
    begin
{$ifdef debug_collector}
      Writeln('Collector thread id: 0x', HexStr(Pointer(GetCurrentThreadID)));
{$endif}
      while not TerminateCollector do begin
        Sleep(CollectorWaitTime);
        { copy the list head during a lock }
{$ifdef debug_collector}
        Writeln('Getting list head');
{$endif}
        SysEnterCriticalSection(ExternalThreadLock);
        current := ExternalThreadsList;
{$ifdef debug_collector}
        Writeln('Current: ', hexstr(current), ' Head: ',
          hexstr(ExternalThreadsList));
        Writeln('Current list: ');
        tmp := current;
        while (tmp <> nil) and not TerminateCollector do begin
          Write(hexstr(tmp));
          if tmp^.Next <> nil then
            Write(' -> ')
          else
            Writeln;
          tmp := tmp^.Next;
        end;
{$endif}
        SysLeaveCriticalSection(ExternalThreadLock);

        last := nil;
        { now walk the thread list and check whether a thread terminated }
        while (current <> nil) and not TerminateCollector do begin
{$ifdef debug_collector}
          Writeln('Checking thread ', hexstr(Pointer(current^.Handle)));
{$endif}
          res := WaitForSingleObject(current^.Handle, 0);
          if res = WAIT_OBJECT_0 then begin
            { now "impersonate" the terminated thread }
{$ifdef debug_collector}
            Writeln('Thread ', hexstr(Pointer(current^.Handle)), ' signaled; ',
              'impersonating it');
{$endif}
            SysEnterCriticalSection(FakeProtection);
            FakeThreadVars := current^.ThreadVarArea;
            { don't use EndThread here, as it calls ExitThread and I want to
              avoid another check for impersonation ^^ }
            //FlushThread;
            DoneThread;
            FakeThreadVars := nil;
            SysLeaveCriticalSection(FakeProtection);
            CloseHandle(current^.Handle);

{$ifdef debug_collector}
            Writeln('Fixing pointer list');
{$endif}
            { fix up the list }
            if last = nil then begin
              { this was the list head }
              SysEnterCriticalSection(ExternalThreadLock);
{$ifdef debug_collector}
              Writeln('Current: ', hexstr(current), ' Head: ',
                hexstr(ExternalThreadsList));
{$endif}
              if current = ExternalThreadsList then begin
{$ifdef debug_collector}
                Writeln('Current item is still list head');
{$endif}
                { this is still the head, so keep it simple }
                ExternalThreadsList := current^.Next
              end else begin
{$ifdef debug_collector}
                Writeln('Searching current item again');
{$endif}
                { we need to walk the list again -.- }
                tmplast := nil;
                tmp := ExternalThreadsList;
                while (tmp <> nil) and (tmp <> current) do begin
                  tmplast := tmp;
                  tmp := tmp^.Next;
                end;
                if tmp <> nil then
                  tmplast^.Next := tmp^.Next;
              end;
              SysLeaveCriticalSection(ExternalThreadLock);
            end else begin
              { just fix up the last entry }
{$ifdef debug_collector}
              Writeln('Fixing last entry');
{$endif}
              last^.Next := current^.Next;
            end;

{$ifdef debug_collector}
            Writeln('List fixed; freeing item');
{$endif}
            { the list is fixed, so free the entry }
            tmp := current;
            current := current^.Next;
            LocalFree(tmp);
          end else begin
{$ifdef debug_collector}
            Writeln('Thread not signaled (', res, '); moving to next item');
{$endif}
            last := current;
            current := current^.Next;
          end;
        end;
{$ifdef debug_collector}
        Writeln('Complete list processed - sleeping');
{$endif}
      end;

{$ifdef debug_collector}
      Writeln('Freeing complete list');
{$endif}
      { no threads will be attached now, so we can do a shutdown without the
        lock being enabled }
      current := ExternalThreadsList;
      while current <> nil do begin
        { SB: EndThread this thread? }
        CloseHandle(current^.Handle);
        tmp := current;
        current := current^.Next;
        LocalFree(tmp);
      end;
      ExternalThreadsList := nil;

{$ifdef debug_collector}
      Writeln('Ending collector');
{$endif}
      EndThread;
      Result := 0;
    end;

    procedure RegisterExternalThread(aThread: THandle; aDataArea: pointer);
    var
      entry: PExternalThreadInfo;
    begin
      SysEnterCriticalSection(RegisterExternalLock);
      if not ExternalThreadsDetected then begin
        { from now on we are running in external thread mode }
        ExternalThreadsDetected := True;
        SysInitCriticalSection(FakeProtection);
        SysInitCriticalSection(ExternalThreadLock);
        { start the collector }
        BeginThread(@ExternalThreadCollector);
      end;
      SysLeaveCriticalSection(RegisterExternalLock);

      { we must not use FPC's heap, because the memory would be allocated in the
        external thread's context which is not what we want }
      entry := Pointer(LocalAlloc(LMEM_FIXED, SizeOf(TExternalThreadInfo)));
      if entry = nil then
        { oh damn }
        Exit;

      if not DuplicateHandle(GetCurrentProcess, aThread, GetCurrentProcess,
          entry^.Handle, 0, False, DUPLICATE_SAME_ACCESS) then begin
        { oh damn 2nd }
        LocalFree(entry);
        Exit;
      end;

      entry^.ThreadVarArea := aDataArea;

      { now prepend the entry to the list }
      SysEnterCriticalSection(ExternalThreadLock);
{$ifdef debug_collector}
      WritelnDirect('Adding new entry ' + hexstr(entry) + ' (' +
        hexstr(Pointer(entry^.Handle)) + ') to list with head ' +
        hexstr(ExternalThreadsList));
{$endif}
      entry^.Next := ExternalThreadsList;
      ExternalThreadsList := entry;
      SysLeaveCriticalSection(ExternalThreadLock);
    end;
{$endif}

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

    const
      threadvarblocksize : dword = 0;

    const
      TLSKey : DWord = $ffffffff;
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

    function SysRelocateThreadvar(offset : dword) : pointer; forward;

    procedure SysInitMultithreading;
      begin
        { do not check IsMultiThread, as program could have altered it, out of Delphi habit }

        { the thread attach/detach code uses locks to avoid multiple calls of this }
        if TLSKey=$ffffffff then
         begin
           { We're still running in single thread mode, setup the TLS }
           TLSKey:=TlsAlloc;
           InitThreadVars(@SysRelocateThreadvar);

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

    function SysRelocateThreadvar(offset : dword) : pointer;
      var
        dataindex : pointer;
        errorsave : dword;
{$ifndef wince}
        usecs : Boolean;
{$endif}
      begin
{$ifdef dummy}
        { it least in the on windows 7 x64, this still doesn't not work, fs:(0x2c) is
          self referencing on this system (FK)
          MVC: It also does not work on Windows Vista 32-bit, Home Premium, SP 1. Results in a crash}
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
{$ifndef wince}
        { once external threads have been detected we have to use the
          FakeProtection critical section, so that only the collector will
          access FakeThreadVars }
        usecs := ExternalThreadsDetected and (FakeThreadVars <> Nil)
                   and not IsLibrary;
        if usecs then
          SysEnterCriticalSection(FakeProtection);
        if FakeThreadVars <> nil then begin
{$ifdef debug_collector}
          WritelnDirect('SysRelocateThreadVar: Impersonating thread');
{$endif}
          { use the faked area }
          dataindex := FakeThreadVars
        end else begin
          { use our normal thread var area }
          errorsave:=GetLastError;
          dataindex:=TlsGetValue(tlskey);
          if dataindex=nil then
            begin
              { this is a thread that was not created by FPC's RTL... so we need
                to initalize the RTL for this thread and do some ugly
                bookkeeping so that we can finalize the RTL at the end. }
{$ifdef debug_collector}
              WritelnDirect('SysRelocateThreadVar: dataindex is Nil');
{$endif}
              SysAllocateThreadVars;
              dataindex:=TlsGetValue(tlskey);
{$ifdef debug_collector}
              WritelnDirect('SysRelocateThreadVar: Initializing external ' +
                'thread');
{$endif}
              InitThread(ExternalThreadStkLen);
              { in a DLL an external thread will be finalized by DllMain
                called with DLL_THREAD_DETACH }
              if not IsLibrary then
                { attach this thread to collector thread }
                RegisterExternalThread(GetCurrentThread, dataindex);
            end;
          SetLastError(errorsave);
        end;
        if usecs then
          SysLeaveCriticalSection(FakeProtection);
{$else}
        errorsave:=GetLastError;
        dataindex:=TlsGetValue(tlskey);
        if dataindex=nil then
          begin
            SysAllocateThreadVars;
            dataindex:=TlsGetValue(tlskey);
            InitThread($1000000);
          end;
        SetLastError(errorsave);
{$endif}
{$endif win32}
        SysRelocateThreadvar:=DataIndex+Offset;
      end;


    procedure SysReleaseThreadVars;
      begin
{$ifndef wince}
        if ExternalThreadsDetected and (FakeThreadVars <> nil) then
          { finally free the memory area }
          LocalFree(FakeThreadVars)
        else begin
{$endif}
          LocalFree(TlsGetValue(tlskey));
          TlsSetValue(tlskey, nil);
{$ifndef wince}
        end;
{$endif}
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
        { Copy parameter to local data }
        ti:=pthreadinfo(param)^;
        { In a DLL the thread was already initialized during a call to DllMain
          with DLL_THREAD_ATTACH }
        if not IsLibrary {and (TlsGetValue(TLSKey) = nil)} then begin
{$ifdef debug_collector}
          WritelnDirect('ThreadMain: Initializing thread');
{$endif}
          { Allocate local thread vars, this must be the first thing,
            because the exception management and io depends on threadvars }
          SysAllocateThreadVars;

          dispose(pthreadinfo(param));

          { Initialize thread }
          InitThread(ti.stklen);
        end else
          dispose(pthreadinfo(param));

        { Start thread function }
{$ifdef DEBUG_MT}
        writeln('Jumping to thread function of thread ',Win32GetCurrentThreadId);
{$endif DEBUG_MT}
        ThreadMain:=ti.f(ti.p);
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

    function  SysCloseThread (threadHandle : TThreadID) : dword;
    begin
      SysCloseThread:=winCloseHandle(threadHandle);
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

{$ifdef SUPPORT_WIN95}
function Win95TryEnterCriticalSection(var cs : TRTLCriticalSection):longint;stdcall;
var
  MyThreadID : DWORD;
begin
  MyThreadId:=GetCurrentThreadId();
  if InterlockedIncrement(cs.LockCount)=0 then
    begin
      cs.OwningThread:=MyThreadId;
      cs.RecursionCount:=1;
      result:=1;
    end
  else
    begin
      if cs.OwningThread=MyThreadId then
        begin
          InterlockedDecrement(cs.LockCount);
          InterlockedIncrement(cs.RecursionCount);
          result:=1;
        end
      else
        begin
          InterlockedDecrement(cs.LockCount);
          result:=0;
        end;
    end;
end;
{$endif SUPPORT_WIN95}

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
{$IFDEF SUPPORT_WIN95}
var
  KernelHandle : THandle;
{$ENDIF SUPPORT_WIN95}
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
    CloseThread		   :=@SysCloseThread;
    WaitForThreadTerminate :=@SysWaitForThreadTerminate;
    ThreadSetPriority      :=@SysThreadSetPriority;
    ThreadGetPriority      :=@SysThreadGetPriority;
    GetCurrentThreadId     :=@SysGetCurrentThreadId;
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
  if IsLibrary then
    SysInitMultithreading;
{$ifndef wince}
  SysInitCriticalSection(RegisterExternalLock);
{$endif}
{$IFDEF SUPPORT_WIN95}
  { Try to find TryEnterCriticalSection function }
  KernelHandle:=LoadLibrary(KernelDLL);
  if KernelHandle<>0 then
    begin
      WinTryEnterCriticalSection:=TTryEnterCriticalSection(GetProcAddress(KernelHandle,'TryEnterCriticalSection'));
      FreeLibrary(KernelHandle);
    end;
  if not assigned(WinTryEnterCriticalSection) then
    WinTryEnterCriticalSection:=@Win95TryEnterCriticalSection;
{$ENDIF SUPPORT_WIN95}
end;

{$ifndef wince}
procedure FiniSystemThreads;
begin
  TerminateCollector := True;
end;
{$endif}

