{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by the Free Pascal development team.

    OS/2 threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit threads;
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
                           Local Api imports
*****************************************************************************}

const
 pag_Read = 1;
 pag_Write = 2;
 pag_Execute = 4;
 pag_Guard = 8;
 pag_Commit = $10;
 obj_Tile = $40;
 sem_Indefinite_Wait = -1;
 dtSuspended = 1;
 dtStack_Commited = 2;

type
 TThreadInfo = record
  F: TThreadFunc;
  P: pointer;
 end;
 PThreadInfo = ^TThreadInfo;

{ import the necessary stuff from the OS }
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): longint;
                                          cdecl; external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): longint; cdecl;
                                                 external 'DOSCALLS' index 455;

function DosCreateThread (var TID: longint; Address: pointer;
(* TThreadFunc *)
        aParam: pointer; Flags: longint; StackSize: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 311;

procedure DosExit (Action, Result: longint); cdecl;
                                                 external 'DOSCALLS' index 234;

function DosCreateMutExSem (Name: PChar; var Handle: longint; Attr: longint;
                State: boolean): longint; cdecl; external 'DOSCALLS' index 331;

function DosCloseMutExSem (Handle: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 333;

function DosQueryMutExSem (Handle: longint; var PID, TID, Count: longint):
                                 longint; cdecl; external 'DOSCALLS' index 336;

function DosRequestMutExSem (Handle, Timeout: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 335;

function DosAllocMem (var P: pointer; Size, Flag: longint): longint; cdecl;
                                                 external 'DOSCALLS' index 299;

function DosFreeMem (P: pointer): longint; cdecl;
                                                 external 'DOSCALLS' index 304;

function DosEnterCritSec:longint; cdecl; external 'DOSCALLS' index 232;

function DosExitCritSec:longint; cdecl; external 'DOSCALLS' index 233;


{*****************************************************************************
                             Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}
const
 ThreadVarBlockSize: dword = 0;

var
(* Pointer to an allocated dword space within the local thread *)
(* memory area. Pointer to the real memory block allocated for *)
(* thread vars in this block is then stored in this dword.     *)
 DataIndex: PPointer;

procedure SysInitThreadvar (var Offset: dword; Size: dword);
begin
 Offset := ThreadVarBlockSize;
 Inc (ThreadVarBlockSize, Size);
end;

function SysRelocateThreadVar (Offset: dword): pointer;
begin
 SysRelocateThreadVar := DataIndex^ + Offset;
end;

procedure SysAllocateThreadVars;
begin
 { we've to allocate the memory from the OS }
 { because the FPC heap management uses     }
 { exceptions which use threadvars but      }
 { these aren't allocated yet ...           }
 { allocate room on the heap for the thread vars }
 if os_mode = osOS2 then
 begin
  if DosAllocMem (DataIndex^, ThreadVarBlockSize, pag_Read or pag_Write
                                      or pag_Commit) <> 0 then HandleError (8);
 end else
 begin
  (* Allocate the DOS memory here. *)

 end;
end;

procedure SysReleaseThreadVars;
begin
 { release thread vars }
 if os_mode = osOS2 then DosFreeMem (DataIndex^) else
 begin
  (* Deallocate the DOS memory here. *)

 end;
end;

{ Include OS independent Threadvar initialization }
{$i threadvar.inc}

    procedure InitThreadVars;
      begin
        { We're still running in single thread mode, setup the TLS }
        TLSKey:=TlsAlloc;
        { initialize threadvars }
        init_all_unit_threadvars;
        { allocate mem for main thread threadvars }
        SysAllocateThreadVars;
        { copy main thread threadvars }
        copy_all_unit_threadvars;
        { install threadvar handler }
        fpc_threadvar_relocate_proc:=@SysRelocateThreadvar;
      end;

{$endif HASTHREADVAR}


{*****************************************************************************
                            Thread starting
*****************************************************************************}

    const
      DefaultStackSize = 32768; { including 16384 margin for stackchecking }

    type
      pthreadinfo = ^tthreadinfo;
      tthreadinfo = record
        f : tthreadfunc;
        p : pointer;
        stklen : cardinal;
      end;

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
           InitThreadVars;
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


{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include generic overloaded routines }
{$i thread.inc}

initialization
  InitHeapMutexes;
end.
{
  $Log$
  Revision 1.1  2002-11-17 16:22:54  hajny
    + RTL for emx target

  Revision 1.1  2002/10/14 19:39:18  peter
    * threads unit added for thread support

}
  