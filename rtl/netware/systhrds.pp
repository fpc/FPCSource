{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2002 by the Free Pascal development team.

    Multithreading implementation for NetWare

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit systhrds;
interface

{$S-}

{ Multithreading for netware, armin 16 Mar 2002
  - threads are basicly tested and working
  - TRTLCriticalSections are working but NEVER call Enter or
    LeaveCriticalSection with uninitialized CriticalSections.
    Critial Sections are based on local semaphores and the
    Server will abend if the semaphore handles are invalid. There
    are basic tests in the rtl but this will not work in every case.
    Not closed semaphores will be closed by the rtl on program
    termination because some versions of netware will abend if there
    are open semaphores on nlm unload.
}
{ Include generic thread interface }
{$i threadh.inc }

{Delphi/Windows compatible priority constants, they are also defined for Unix and Win32}
const
   THREAD_PRIORITY_IDLE          = -15;
   THREAD_PRIORITY_LOWEST        = -2;
   THREAD_PRIORITY_BELOW_NORMAL  = -1;
   THREAD_PRIORITY_NORMAL        = 0;
   THREAD_PRIORITY_ABOVE_NORMAL  = 1;
   THREAD_PRIORITY_HIGHEST       = 2;
   THREAD_PRIORITY_TIME_CRITICAL = 15;

implementation

{$i thread.inc }

{ some declarations for Netware API calls }
{$I nwsys.inc}

{  define DEBUG_MT}

const
   threadvarblocksize : dword = 0;     // total size of allocated threadvars
   thredvarsmainthread: pointer = nil; // to free the threadvars in the signal handler


procedure SysInitThreadvar (var offset : dword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];
begin
  offset:=threadvarblocksize;
  inc(threadvarblocksize,size);
  {$ifdef DEBUG_MT}
  ConsolePrintf3(#13'init_threadvar, new offset: (%d), Size:%d'#13#10,offset,size,0);
  {$endif DEBUG_MT}
end;


{$ifdef DEBUG_MT}
var dummy_buff : array [0..255] of char;  // to avoid abends (for current compiler error that not all threadvars are initialized)
{$endif}

function SysRelocateThreadvar (offset : dword) : pointer;
var p : pointer;
begin
 {$ifdef DEBUG_MT}
//   ConsolePrintf(#13'relocate_threadvar, offset: (%d)'#13#10,offset);
   if offset > threadvarblocksize then
   begin
//     ConsolePrintf(#13'relocate_threadvar, invalid offset'#13#10,0);
     SysRelocateThreadvar := @dummy_buff;
     exit;
   end;
 {$endif DEBUG_MT}
 SysRelocateThreadvar:= _GetThreadDataAreaPtr + offset;
end;

procedure SysAllocateThreadVars;

  var
     threadvars : pointer;

  begin
     { we've to allocate the memory from netware }
     { because the FPC heap management uses      }
     { exceptions which use threadvars but       }
     { these aren't allocated yet ...            }
     { allocate room on the heap for the thread vars }
     threadvars := _malloc (threadvarblocksize);
     fillchar (threadvars^, threadvarblocksize, 0);
     _SaveThreadDataAreaPtr (threadvars);
     {$ifdef DEBUG_MT}
       ConsolePrintf3(#13'threadvars allocated at (%x), size: %d'#13#10,longint(threadvars),threadvarblocksize,0);
     {$endif DEBUG_MT}
     if thredvarsmainthread = nil then
       thredvarsmainthread := threadvars;
  end;

procedure SysReleaseThreadVars;
var threadvars : pointer;
begin
   { release thread vars }
   if threadvarblocksize > 0 then
   begin
     threadvars:=_GetThreadDataAreaPtr;
     if threadvars <> nil then
     begin
       {$ifdef DEBUG_MT}
        ConsolePrintf (#13'free threadvars'#13#10,0);
       {$endif DEBUG_MT}
       _Free (threadvars);
       _SaveThreadDataAreaPtr (nil);
     end;
  end;
end;


{ Include OS independent Threadvar initialization }
{$i threadvr.inc}


{*****************************************************************************
                            Thread starting
*****************************************************************************}

type
   tthreadinfo = record
      f : tthreadfunc;
      p : pointer;
      stklen: cardinal;
   end;
   pthreadinfo = ^tthreadinfo;



procedure DoneThread;

  begin
     { release thread vars }
     SysReleaseThreadVars;
  end;


function ThreadMain(param : pointer) : dword; cdecl;

  var
     ti : tthreadinfo;

  begin
     SysAllocateThreadVars;
{$ifdef DEBUG_MT}
     ConsolePrintf(#13'New thread started, initialising ...'#13#10);
{$endif DEBUG_MT}
     ti:=pthreadinfo(param)^;
     InitThread(ti.stklen);
     dispose(pthreadinfo(param));
{$ifdef DEBUG_MT}
     ConsolePrintf(#13'Jumping to thread function'#13#10);
{$endif DEBUG_MT}
     ThreadMain:=ti.f(ti.p);
     DoneThread;
  end;


function BeginThread(sa : Pointer;stacksize : dword;
  ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword;
  var ThreadId : DWord) : DWord;

  var ti : pthreadinfo;

  begin
{$ifdef DEBUG_MT}
     ConsolePrintf(#13'Creating new thread'#13#10);
{$endif DEBUG_MT}
     if not IsMultiThread then
     begin
       InitThreadVars(@SysRelocateThreadvar);
       IsMultithread:=true;
     end;
     { the only way to pass data to the newly created thread }
     { in a MT safe way, is to use the heap                  }
     new(ti);
     ti^.f:=ThreadFunction;
     ti^.p:=p;
     ti^.stklen:=stacksize;
{$ifdef DEBUG_MT}
     ConsolePrintf(#13'Starting new thread'#13#10);
{$endif DEBUG_MT}
     BeginThread :=
       _BeginThread (@ThreadMain,NIL,Stacksize,ti);
  end;


procedure EndThread(ExitCode : DWord);
begin
  DoneThread;
  ExitThread(ExitCode , TSR_THREAD);
end;


{*****************************************************************************
                            Thread handling
*****************************************************************************}


function __SuspendThread (threadId : dword) : dword; cdecl; external 'clib' name 'SuspendThread';
function __ResumeThread (threadId : dword) : dword; cdecl; external 'clib' name 'ResumeThread';
procedure __ThreadSwitchWithDelay; cdecl; external 'clib' name 'ThreadSwitchWithDelay';

{redefined because the interface has not cdecl calling convention}
function SuspendThread (threadHandle : dword) : dword;
begin
  SuspendThread := __SuspendThread (threadHandle);
end;


function ResumeThread (threadHandle : dword) : dword;
begin
  ResumeThread := __ResumeThread (threadHandle);
end;


procedure ThreadSwitch;
begin
  __ThreadSwitchWithDelay;
end;


function  KillThread (threadHandle : dword) : dword;
begin
  KillThread := 1;  {not supported for netware}
end;

function GetThreadName  (threadId : longint; var threadName) : longint; cdecl; external 'clib' name 'GetThreadName';
//function __RenameThread (threadId : longint; threadName:pchar) : longint; cdecl; external 'clib' name 'RenameThread';

function  WaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;
var
  status : longint;
  buf : array [0..50] of char;
begin
  {$warning timeout needs to be implemented}
  repeat
    status := GetThreadName (ThreadHandle,Buf); {should return EBADHNDL if thread is terminated}
    ThreadSwitch;
  until status <> 0;
  WaitForThreadTerminate:=0;
end;

function  ThreadSetPriority (threadHandle : dword; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  ThreadSetPriority := true;
end;

function  ThreadGetPriority (threadHandle : dword): Integer;
begin
  ThreadGetPriority := 0;
end;

function GetThreadID : dword; cdecl; external 'clib' name 'GetThreadID';

function  GetCurrentThreadHandle : dword;
begin
  GetCurrentThreadHandle := GetThreadID;
end;


{ netware requires all allocated semaphores }
{ to be closed before terminating the nlm, otherwise }
{ the server will abend (except for netware 6 i think) }

TYPE TSemaList = ARRAY [1..1000] OF LONGINT;
     PSemaList = ^TSemaList;

CONST NumSemaOpen   : LONGINT = 0;
      NumEntriesMax : LONGINT = 0;
      SemaList      : PSemaList = NIL;

PROCEDURE SaveSema (Handle : LONGINT);
BEGIN
  {$ifdef DEBUG_MT}
     ConsolePrintf(#13'new Semaphore allocated (%x)'#13#10,Handle);
  {$endif DEBUG_MT}
  _EnterCritSec;
  IF NumSemaOpen = NumEntriesMax THEN
  BEGIN
    IF SemaList = NIL THEN
    BEGIN
      SemaList := _malloc (32 * SIZEOF (TSemaList[0]));
      NumEntriesMax := 32;
    END ELSE
    BEGIN
      INC (NumEntriesMax, 16);
      SemaList := _realloc (SemaList, NumEntriesMax * SIZEOF (TSemaList[0]));
    END;
  END;
  INC (NumSemaOpen);
  SemaList^[NumSemaOpen] := Handle;
  _ExitCritSec;
END;

PROCEDURE ReleaseSema (Handle : LONGINT);
VAR I : LONGINT;
BEGIN
  {$ifdef DEBUG_MT}
     ConsolePrintf(#13'Semaphore released (%x)'#13#10,Handle);
  {$endif DEBUG_MT}
  _EnterCritSec;
  IF SemaList <> NIL then
    if NumSemaOpen > 0 then
    begin
      for i := 1 to NumSemaOpen do
        if SemaList^[i] = Handle then
        begin
          if i < NumSemaOpen then
            SemaList^[i] := SemaList^[NumSemaOpen];
          dec (NumSemaOpen);
          _ExitCritSec;
          exit;
        end;
    end;
  _ExitCritSec;
  ConsolePrintf (#13'fpc-rtl: ReleaseSema, Handle not found'#13#10,0);
END;


PROCEDURE CloseAllRemainingSemaphores;
var i : LONGINT;
begin
  IF SemaList <> NIL then
  begin
    if NumSemaOpen > 0 then
      for i := 1 to NumSemaOpen do
        _CloseLocalSemaphore (SemaList^[i]);
     _free (SemaList);
     SemaList := NIL;
     NumSemaOpen := 0;
     NumEntriesMax := 0;
  end;
end;

{ this allows to do a lot of things in MT safe way }
{ it is also used to make the heap management      }
{ thread safe                                      }
procedure InitCriticalSection(var cs : TRTLCriticalSection);
begin
  cs.SemaHandle := _OpenLocalSemaphore (1);
  if cs.SemaHandle <> 0 then
  begin
    cs.SemaIsOpen := true;
    SaveSema (cs.SemaHandle);
  end else
  begin
    cs.SemaIsOpen := false;
    ConsolePrintf (#13'fpc-rtl: InitCriticalsection, OpenLocalSemaphore returned error'#13#10,0);
  end;
end;

procedure DoneCriticalsection(var cs : TRTLCriticalSection);
begin
  if cs.SemaIsOpen then
  begin
    _CloseLocalSemaphore (cs.SemaHandle);
    ReleaseSema (cs.SemaHandle);
    cs.SemaIsOpen := FALSE;
  end;
end;

procedure EnterCriticalsection(var cs : TRTLCriticalSection);
begin
  if cs.SemaIsOpen then
    _WaitOnLocalSemaphore (cs.SemaHandle)
  else
    ConsolePrintf (#13'fpc-rtl: EnterCriticalsection, TRTLCriticalSection not open'#13#10,0);
end;

procedure LeaveCriticalsection(var cs : TRTLCriticalSection);
begin
  if cs.SemaIsOpen then
    _SignalLocalSemaphore (cs.SemaHandle)
  else
    ConsolePrintf (#13'fpc-rtl: LeaveCriticalsection, TRTLCriticalSection not open'#13#10,0);
end;


function SetThreadDataAreaPtr (newPtr:pointer):pointer;
begin
  SetThreadDataAreaPtr := _GetThreadDataAreaPtr;
  if newPtr = nil then
    newPtr := thredvarsmainthread;
  _SaveThreadDataAreaPtr     (newPtr);
end;



{*****************************************************************************
                           Heap Mutex Protection
*****************************************************************************}
			
var
  HeapMutex : TRTLCriticalSection;
				
procedure NWHeapMutexInit;
begin
  InitCriticalSection(heapmutex);
end;
							
procedure NWHeapMutexDone;
begin
  DoneCriticalSection(heapmutex);
end;
										
procedure NWHeapMutexLock;
begin
  EnterCriticalSection(heapmutex);
end;
														
procedure NWHeapMutexUnlock;
begin
  LeaveCriticalSection(heapmutex);
end;
																
const
  NWMemoryMutexManager : TMemoryMutexManager = (
           MutexInit : @NWHeapMutexInit;
           MutexDone : @NWHeapMutexDone;
           MutexLock : @NWHeapMutexLock;
    	   MutexUnlock : @NWHeapMutexUnlock;
  );
																							
procedure InitHeapMutexes;
begin
  SetMemoryMutexManager(NWMemoryMutexManager);
end;



initialization
  InitHeapMutexes;
  NWSysSetThreadFunctions (@CloseAllRemainingSemaphores,
                           @SysReleaseThreadVars,
                           @SetThreadDataAreaPtr);
end.

{
  $Log$
  Revision 1.2  2003-03-27 17:14:27  armin
  * more platform independent thread routines, needs to be implemented for unix

  Revision 1.1  2003/02/16 17:12:15  armin
  * systhrds fir netware added


}
