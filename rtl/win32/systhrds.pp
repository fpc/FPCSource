{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    Win32 threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit systhrds;
interface

{$S-}

  type
    { the fields of this record are os dependent  }
    { and they shouldn't be used in a program     }
    { only the type TCriticalSection is important }
    PRTLCriticalSection = ^TRTLCriticalSection;
    TRTLCriticalSection = packed record
      DebugInfo : pointer;
      LockCount : longint;
      RecursionCount : longint;
      OwningThread : DWord;
      LockSemaphore : DWord;
      Reserved : DWord;
    end;

{ Include generic thread interface }
{$i threadh.inc}


implementation

{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include generic overloaded routines }
{$i thread.inc}


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
function  GetLastError : dword; stdcall;external 'kernel32' name 'GetLastError';
function  WaitForSingleObject (hHandle,Milliseconds: dword): dword; stdcall;external 'kernel32' name 'WaitForSingleObject';
function  WinThreadSetPriority (threadHandle : dword; Prio: longint): boolean; stdcall;external 'kernel32' name 'SetThreadPriority';
function  WinThreadGetPriority (threadHandle : dword): Integer; stdcall;external 'kernel32' name 'GetThreadPriority';
function  WinGetCurrentThreadId : dword; stdcall;external 'kernel32' name 'GetCurrentThread';

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

{ Include OS independent Threadvar initialization }
{$i threadvr.inc}

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


    function ThreadMain(param : pointer) : pointer;cdecl;
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
        ThreadMain:=pointer(ti.f(ti.p));
      end;


    function BeginThread(sa : Pointer;stacksize : dword;
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
        BeginThread:=CreateThread(sa,stacksize,@ThreadMain,ti,creationflags,threadid);
      end;


    procedure EndThread(ExitCode : DWord);
      begin
        DoneThread;
        ExitThread(ExitCode);
      end;


    procedure ThreadSwitch;
    begin
      Sleep(0);
    end;


    function  SuspendThread (threadHandle : dword) : dword;
    begin
      SuspendThread:=WinSuspendThread(threadHandle);
    end;


    function  ResumeThread  (threadHandle : dword) : dword;
    begin
      ResumeThread:=WinResumeThread(threadHandle);
    end;


    function  KillThread (threadHandle : dword) : dword;
    var exitCode : dword;
    begin
      if not TerminateThread (threadHandle, exitCode) then
        KillThread := GetLastError
      else
        KillThread := 0;
    end;

    function  WaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;
    begin
      if timeoutMs = 0 then dec (timeoutMs);  // $ffffffff is INFINITE
      WaitForThreadTerminate := WaitForSingleObject(threadHandle, TimeoutMs);
    end;


    function  ThreadSetPriority (threadHandle : dword; Prio: longint): boolean;            {-15..+15, 0=normal}
    begin
      ThreadSetPriority:=WinThreadSetPriority(threadHandle,Prio);
    end;


    function  ThreadGetPriority (threadHandle : dword): Integer;
    begin
      ThreadGetPriority:=WinThreadGetPriority(threadHandle);
    end;


    function  GetCurrentThreadId : dword;
    begin
      GetCurrentThreadId:=WinGetCurrentThreadId;
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

procedure InitCriticalSection(var cs : TRTLCriticalSection);
begin
  WinInitCriticalSection(cs);
end;


procedure DoneCriticalSection(var cs : TRTLCriticalSection);
begin
  WinDoneCriticalSection(cs);
end;


procedure EnterCriticalSection(var cs : TRTLCriticalSection);
begin
  WinEnterCriticalSection(cs);
end;


procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
begin
  WinLeaveCriticalSection(cs);
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


initialization
  InitHeapMutexes;
end.
{
  $Log$
  Revision 1.6  2003-10-01 21:00:09  peter
    * GetCurrentThreadHandle renamed to GetCurrentThreadId

  Revision 1.5  2003/09/17 15:06:36  peter
    * stdcall patch

  Revision 1.4  2003/03/27 17:14:27  armin
  * more platform independent thread routines, needs to be implemented for unix

  Revision 1.3  2003/03/24 16:12:01  jonas
    * BeginThread() now returns the thread handle instead of the threadid
      (needed because you have to free the handle after your thread is
       finished, and the threadid is already returned via a var-parameter)

  Revision 1.2  2002/10/31 13:45:44  carl
    * threadvar.inc -> threadvr.inc

  Revision 1.1  2002/10/16 06:27:30  michael
  + Renamed thread unit to systhrds

  Revision 1.1  2002/10/14 19:39:18  peter
    * threads unit added for thread support

}

