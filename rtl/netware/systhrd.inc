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


{*****************************************************************************
                             Threadvar support
*****************************************************************************}


const
   threadvarblocksize : dword = 0;     // total size of allocated threadvars
   thredvarsmainthread: pointer = nil; // to free the threadvars in the signal handler


procedure SysInitThreadvar (var offset : dword;size : dword);[public,alias: 'FPC_INIT_THREADVAR'];
begin
  offset:=threadvarblocksize;
  inc(threadvarblocksize,size);
  {$ifdef DEBUG_MT}
  ConsolePrintf(#13'init_threadvar, new offset: (%d), Size:%d'#13#10,offset,size,0);
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
       ConsolePrintf(#13'threadvars allocated at (%x), size: %d'#13#10,longint(threadvars),threadvarblocksize,0);
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



function ThreadMain(param : pointer) : dword; cdecl;

  var
     ti : tthreadinfo;

  begin
     { Allocate local thread vars, this must be the first thing,
       because the exception management and io depends on threadvars }
     SysAllocateThreadVars;
{$ifdef DEBUG_MT}
     ConsolePrintf(#13'New thread %x started, initialising ...'#13#10,_GetThreadID);
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

function SysBeginThread(sa : Pointer;stacksize : SizeUInt;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword; var ThreadId : DWord) : DWord;

  var ti : pthreadinfo;

  begin
{$ifdef DEBUG_MT}
     ConsolePrintf(#13'Creating new thread'#13#10);
{$endif DEBUG_MT}
     if not IsMultiThread then
     begin
       InitThreadVars(@SysRelocateThreadvar);
      { lazy initialize thread support }
       LazyInitThreading;
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
     SysBeginThread :=
       _BeginThread (@ThreadMain,NIL,Stacksize,ti);
  end;


procedure SysEndThread(ExitCode : DWord);
begin
  {$ifdef DEBUG_MT}
  ConsolePrintf (#13'SysEndThread %x'#13#10,_GetThreadID);
  {$endif}
  DoneThread;
  ExitThread(ExitCode , TSR_THREAD);
end;

{*****************************************************************************
                            Thread handling
*****************************************************************************}


function __SuspendThread (threadId : dword) : dword; cdecl; external 'clib' name 'SuspendThread';
function __ResumeThread (threadId : dword) : dword; cdecl; external 'clib' name 'ResumeThread';
procedure __ThreadSwitchWithDelay; cdecl; external 'clib' name 'ThreadSwitchWithDelay';

procedure SysThreadSwitch;
begin
  __ThreadSwitchWithDelay;
end;


{redefined because the interface has not cdecl calling convention}
function SysSuspendThread (threadHandle : dword) : dword;
begin
  SysSuspendThread := __SuspendThread (threadHandle);
end;


function SysResumeThread (threadHandle : dword) : dword;
begin
  SysResumeThread := __ResumeThread (threadHandle);
end;


function  SysKillThread (threadHandle : dword) : dword;
begin
  SysKillThread := 1;  {not supported for netware}
end;

function GetThreadName  (threadId : longint; var threadName) : longint; cdecl; external 'clib' name 'GetThreadName';
function CGetThreadID : dword; cdecl; external 'clib' name 'GetThreadID';
//function __RenameThread (threadId : longint; threadName:pchar) : longint; cdecl; external 'clib' name 'RenameThread';

function  SysWaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;
var
  status : longint;
  buf : array [0..50] of char;
begin
  {$warning timeout needs to be implemented}
  {$ifdef DEBUG_MT}
  ConsolePrintf (#13'SysWaitForThreadTerminate ThreadID:%x Handle:%x'#13#10,GetThreadID,threadHandle);
  {$endif}
  repeat
    status := GetThreadName (ThreadHandle,Buf); {should return EBADHNDL if thread is terminated}
    ThreadSwitch;
  until status <> 0;
  SysWaitForThreadTerminate:=0;
end;

function  SysThreadSetPriority (threadHandle : dword; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  SysThreadSetPriority := true;
end;

function  SysThreadGetPriority (threadHandle : dword): Longint;
begin
  SysThreadGetPriority := 0;
end;

function  SysGetCurrentThreadId : dword;
begin
  SysGetCurrentThreadId := CGetThreadID;
end;

procedure SysSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);
begin
  {$Warning SetThreadDebugName needs to be implemented}
end;

procedure SysSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);
begin
  {$Warning SetThreadDebugName needs to be implemented}
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


PROCEDURE SysCloseAllRemainingSemaphores;
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
procedure SysInitCriticalSection(var cs);// : TRTLCriticalSection);
begin
  with PRTLCriticalSection(@cs)^ do
  begin
    SemaHandle := _OpenLocalSemaphore (1);
    if SemaHandle <> 0 then
    begin
      SemaIsOpen := true;
      SaveSema (SemaHandle);
    end else
    begin
      SemaIsOpen := false;
      ConsolePrintf (#13'fpc-rtl: InitCriticalsection, OpenLocalSemaphore returned error'#13#10,0);
    end;
  end;
end;

procedure SysDoneCriticalsection(var cs);
begin
  with PRTLCriticalSection(@cs)^ do
  begin
    if SemaIsOpen then
    begin
      _CloseLocalSemaphore (SemaHandle);
      ReleaseSema (SemaHandle);
      SemaIsOpen := FALSE;
    end;
  end;
end;

procedure SysEnterCriticalsection(var cs);
begin
  with PRTLCriticalSection(@cs)^ do
  begin
    if SemaIsOpen then
      _WaitOnLocalSemaphore (SemaHandle)
    else
      ConsolePrintf (#13'fpc-rtl: EnterCriticalsection, TRTLCriticalSection not open'#13#10,0);
  end;
end;

procedure SysLeaveCriticalSection(var cs);
begin
  with PRTLCriticalSection(@cs)^ do
  begin
    if SemaIsOpen then
      _SignalLocalSemaphore (SemaHandle)
    else
      ConsolePrintf (#13'fpc-rtl: LeaveCriticalsection, TRTLCriticalSection not open'#13#10,0);
  end;
end;


function SysSetThreadDataAreaPtr (newPtr:pointer):pointer;
begin
  SysSetThreadDataAreaPtr := _GetThreadDataAreaPtr;
  if newPtr = nil then
    newPtr := thredvarsmainthread;
  _SaveThreadDataAreaPtr     (newPtr);
end;

function intBasicEventCreate(EventAttributes : Pointer;
AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
begin
  {$WARNING TODO! intBasicEventCreate}
end;

procedure intbasiceventdestroy(state:peventstate);
begin
  {$WARNING TODO! intbasiceventdestroy}
end;

procedure intbasiceventResetEvent(state:peventstate);
begin
  {$WARNING TODO! intbasiceventResetEvent}
end;

procedure intbasiceventSetEvent(state:peventstate);
begin
  {$WARNING TODO! intbasiceventSetEvent}
end;

function intbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;
begin
  {$WARNING TODO! intbasiceventWaitFor}
end;

function intRTLEventCreate: PRTLEvent;
begin
  {$WARNING TODO! intRTLEventCreate}
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);
begin
  {$WARNING TODO! intRTLEventDestroy}
end;

procedure intRTLEventSetEvent(AEvent: PRTLEvent);
begin
  {$WARNING TODO! intRTLEventSetEvent}
end;

procedure intRTLEventResetEvent(AEvent: PRTLEvent);
begin
  {$WARNING TODO! intRTLEventResetEvent}
end;

procedure intRTLEventWaitFor(AEvent: PRTLEvent);
begin
{$WARNING TODO! intRTLEventWaitFor}
end;

procedure intRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);
begin
  {$WARNING TODO! intRTLEventWaitForTimeout}
end;




Var
  NWThreadManager : TThreadManager;

Procedure InitSystemThreads;

begin
  With NWThreadManager do
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
    SetThreadDebugNameA    :=@SysSetThreadDebugNameA;
    SetThreadDebugNameU    :=@SysSetThreadDebugNameU;
    InitCriticalSection    :=@SysInitCriticalSection;
    DoneCriticalSection    :=@SysDoneCriticalSection;
    EnterCriticalSection   :=@SysEnterCriticalSection;
    LeaveCriticalSection   :=@SysLeaveCriticalSection;
    InitThreadVar          :=@SysInitThreadVar;
    RelocateThreadVar      :=@SysRelocateThreadVar;
    AllocateThreadVars     :=@SysAllocateThreadVars;
    ReleaseThreadVars      :=@SysReleaseThreadVars;
    BasicEventCreate       :=@intBasicEventCreate;
    basiceventdestroy      :=@intbasiceventdestroy;
    basiceventResetEvent   :=@intbasiceventResetEvent;
    basiceventSetEvent     :=@intbasiceventSetEvent;
    basiceventWaitFor      :=@intbasiceventWaitFor;
    RTLEventCreate         :=@intRTLEventCreate;
    RTLEventDestroy        :=@intRTLEventDestroy;
    RTLEventSetEvent       :=@intRTLEventSetEvent;
    RTLEventResetEvent     :=@intRTLEventResetEvent;
    RTLEventWaitFor        :=@intRTLEventWaitFor;
    RTLEventWaitForTimeout :=@intRTLEventWaitForTimeout;
    end;
  SetThreadManager(NWThreadManager);
  NWSysSetThreadFunctions (@SysCloseAllRemainingSemaphores,
                           @SysReleaseThreadVars,
                           @SysSetThreadDataAreaPtr);
end;



