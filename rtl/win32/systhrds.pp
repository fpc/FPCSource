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
  external 'kernel32' name 'TlsAlloc';
function TlsGetValue(dwTlsIndex : DWord) : pointer;
  external 'kernel32' name 'TlsGetValue';
function TlsSetValue(dwTlsIndex : DWord;lpTlsValue : pointer) : LongBool;
  external 'kernel32' name 'TlsSetValue';
function TlsFree(dwTlsIndex : DWord) : LongBool;
  external 'kernel32' name 'TlsFree';
function CreateThread(lpThreadAttributes : pointer;
  dwStackSize : DWord; lpStartAddress : pointer;lpParameter : pointer;
  dwCreationFlags : DWord;var lpThreadId : DWord) : Dword;
  external 'kernel32' name 'CreateThread';
procedure ExitThread(dwExitCode : DWord);
  external 'kernel32' name 'ExitThread';
function GlobalAlloc(uFlags:DWord; dwBytes:DWORD):Pointer;
  external 'kernel32' name 'GlobalAlloc';
function GlobalFree(hMem : Pointer):Pointer; external 'kernel32' name 'GlobalFree';

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
{$i threadvar.inc}

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
        BeginThread:=threadid;
      end;


    procedure EndThread(ExitCode : DWord);
      begin
        DoneThread;
        ExitThread(ExitCode);
      end;


{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

{ we implement these procedures for win32 by importing them }
{ directly from windows                                     }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'InitializeCriticalSection';

procedure DoneCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'DeleteCriticalSection';

procedure EnterCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'EnterCriticalSection';

procedure LeaveCriticalSection(var cs : TRTLCriticalSection);
  external 'kernel32' name 'LeaveCriticalSection';


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
  Revision 1.1  2002-10-16 06:27:30  michael
  + Renamed thread unit to systhrds

  Revision 1.1  2002/10/14 19:39:18  peter
    * threads unit added for thread support

}
  