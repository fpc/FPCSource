{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-5 by Tomas Hajny,
    member of the Free Pascal development team.

    OS/2 threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

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


{ import the necessary stuff from the OS }
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): cardinal;
                                          cdecl; external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): cardinal; cdecl;
                                                 external 'DOSCALLS' index 455;

function DosCreateThread (var TID: cardinal; Address: pointer;
(* TThreadFunc *)
     aParam: pointer; Flags: cardinal; StackSize: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 311;

function DosCreateMutExSem (Name: PChar; var Handle: longint; Attr: cardinal;
               State: boolean): cardinal; cdecl; external 'DOSCALLS' index 331;

function DosCloseMutExSem (Handle: longint): cardinal; cdecl;
                                                 external 'DOSCALLS' index 333;

function DosQueryMutExSem (Handle: longint; var PID, TID, Count: cardinal):
                                cardinal; cdecl; external 'DOSCALLS' index 336;

function DosRequestMutExSem (Handle:longint; Timeout: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: longint): cardinal; cdecl;
                                                 external 'DOSCALLS' index 335;

{
function DosEnterCritSec:cardinal; cdecl; external 'DOSCALLS' index 232;

function DosExitCritSec:cardinal; cdecl; external 'DOSCALLS' index 233;
}

procedure DosSleep (MSec: cardinal); cdecl; external 'DOSCALLS' index 229;


{*****************************************************************************
                             Threadvar support
*****************************************************************************}

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
 if DosAllocMem (DataIndex^, ThreadVarBlockSize, pag_Read or pag_Write
                                      or pag_Commit) <> 0 then HandleError (8);
end;

procedure SysReleaseThreadVars;
begin
  { release thread vars }
  DosFreeMem (DataIndex^);
  DosFreeThreadLocalMemory (DataIndex);
end;

(*    procedure InitThreadVars;
      begin
        { allocate one ThreadVar entry from the OS, we use this entry }
        { for a pointer to our threadvars                             }
        if DosAllocThreadLocalMemory (1, DataIndex) <> 0 then HandleError (8);
        { initialize threadvars }
        init_all_unit_threadvars;
        { allocate mem for main thread threadvars }
        SysAllocateThreadVars;
        { copy main thread threadvars }
        copy_all_unit_threadvars;
        { install threadvar handler }
        fpc_threadvar_relocate_proc := @SysRelocateThreadvar;
      end;
*)


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

(*    procedure InitThread(stklen:cardinal);
      begin
        SysResetFPU;
        SysInitFPU;
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
*)


    function ThreadMain(param : pointer) : pointer;cdecl;
      var
        ti : tthreadinfo;
      begin
        { Allocate local thread vars, this must be the first thing,
          because the exception management and io depends on threadvars }
        SysAllocateThreadVars;
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


    function SysBeginThread(sa : Pointer;stacksize : SizeUInt;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword; var ThreadId : TThreadID) : DWord;
      var
        TI: PThreadInfo;
      begin
{$ifdef DEBUG_MT}
        writeln('Creating new thread');
{$endif DEBUG_MT}
        { Initialize multithreading if not done }
        if not IsMultiThread then
         begin
           if DosAllocThreadLocalMemory (1, DataIndex) <> 0
             then RunError (8);
           InitThreadVars(@SysRelocateThreadVar);
           IsMultiThread:=true;
         end;
        { the only way to pass data to the newly created thread
          in a MT safe way, is to use the heap }
        New (TI);
        TI^.F := ThreadFunction;
        TI^.P := P;
        TI^.StkLen := StackSize;
        { call pthread_create }
{$ifdef DEBUG_MT}
        writeln('Starting new thread');
{$endif DEBUG_MT}
        if DosCreateThread (DWord (ThreadID), @ThreadMain, SA,
                                           CreationFlags, StackSize) = 0 then
        SysBeginThread := ThreadID else SysBeginThread := 0;
      end;


    procedure SysEndThread (ExitCode : DWord);
      begin
        DoneThread;
        DosExit (1, ExitCode);
      end;


    procedure SysThreadSwitch;
    begin
      DosSleep (0);
    end;


    function SysSuspendThread (ThreadHandle: dword): dword;
    begin
 {$WARNING TODO!}
{     SysSuspendThread := WinSuspendThread(threadHandle);
}
    end;


    function SysResumeThread (ThreadHandle: dword): dword;
    begin
{$WARNING TODO!}
{      SysResumeThread := WinResumeThread(threadHandle);
}
    end;


    function SysKillThread (ThreadHandle: dword): dword;
    var
      ExitCode: dword;
    begin
{$WARNING TODO!}
{
      if not TerminateThread (ThreadHandle, ExitCode) then
        SysKillThread := GetLastError
      else
        SysKillThread := 0;
}
    end;

    function SysWaitForThreadTerminate (ThreadHandle: dword;
                                                    TimeoutMs: longint): dword;
    begin
{$WARNING TODO!}
{
      if TimeoutMs = 0 then dec (timeoutMs);  // $ffffffff is INFINITE
      SysWaitForThreadTerminate := WaitForSingleObject(threadHandle, TimeoutMs);
}
    end;


    function SysThreadSetPriority (ThreadHandle: dword;
                                                       Prio: longint): boolean;
    {-15..+15, 0=normal}
    begin
{$WARNING TODO!}
{
      SysThreadSetPriority:=WinThreadSetPriority(threadHandle,Prio);
}
    end;


    function SysThreadGetPriority (ThreadHandle: dword): longint;
    begin
{$WARNING TODO!}
{
      SysThreadGetPriority:=WinThreadGetPriority(threadHandle);
}
    end;


    function SysGetCurrentThreadID: dword;
    begin
{$WARNING TODO!}
{
      SysGetCurrentThreadId:=WinGetCurrentThreadId;
}
    end;



{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

{ DosEnter/ExitCritSec have quite a few limitations, so let's try to avoid
  them. I'm not sure whether mutex semaphores are SMP-safe, though... :-(  }

procedure SysInitCriticalSection(var CS);
begin
{$WARNING TODO!}
end;


procedure SysDoneCriticalSection (var CS);
begin
{$WARNING TODO!}
end;

procedure SysEnterCriticalSection (var CS);
begin
{$WARNING TODO!}
end;

procedure SysLeaveCriticalSection (var CS);
begin
{$WARNING TODO!}
end;



{*****************************************************************************
                           Heap Mutex Protection
*****************************************************************************}

    var
      HeapMutex: TRTLCriticalSection;


    procedure OS2HeapMutexInit;
      begin
         InitCriticalSection (HeapMutex);
      end;


    procedure OS2HeapMutexDone;
      begin
         DoneCriticalSection (HeapMutex);
      end;


    procedure OS2HeapMutexLock;
      begin
         EnterCriticalSection (HeapMutex);
      end;


    procedure OS2HeapMutexUnlock;
      begin
         LeaveCriticalSection (HeapMutex);
      end;


    const
      OS2MemoryMutexManager : TMemoryMutexManager = (
        MutexInit : @OS2HeapMutexInit;
        MutexDone : @OS2HeapMutexDone;
        MutexLock : @OS2HeapMutexLock;
        MutexUnlock : @OS2HeapMutexUnlock;
      );


    procedure InitHeapMutexes;
      begin
        SetMemoryMutexManager (OS2MemoryMutexManager);
      end;


type
  TBasicEventState = record
                      FHandle: THandle;
                      FLastError: longint;
                     end;
  PLocalEventRec = ^TBasicEventState;


function IntBasicEventCreate (EventAttributes: Pointer;
     AManualReset, InitialState: Boolean; const Name: ansistring): PEventState;
begin
  New (PLocalEventRec (Result));
{$WARNING TODO!}
{
  PLocalEventrec (Result)^.FHandle :=
         CreateEvent (EventAttributes, AManualReset, InitialState,PChar(Name));
}
end;


procedure IntBasicEventDestroy (State: PEventState);
begin
{$WARNING TODO!}
{
  closehandle(plocaleventrec(state)^.fhandle);
}
  Dispose (PLocalEventRec (State));
end;


procedure IntBasicEventResetEvent (State: PEventState);
begin
{$WARNING TODO!}
{
  ResetEvent(plocaleventrec(state)^.FHandle)
}
end;


procedure IntBasicEventSetEvent (State: PEventState);
begin
{$WARNING TODO!}
{
  SetEvent(plocaleventrec(state)^.FHandle);
}
end;


function IntBasicEventWaitFor (Timeout: Cardinal; State: PEventState): longint;
begin
{$WARNING TODO!}
{
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
}
end;


function IntRTLEventCreate: PRTLEvent;
begin
{$WARNING TODO!}
{
  Result := PRTLEVENT(CreateEvent(nil, false, false, nil));
}
end;


procedure IntRTLEventDestroy (AEvent: PRTLEvent);
begin
{$WARNING TODO!}
{
  CloseHandle(THANDLE(AEvent));
}
end;


procedure IntRTLEventSetEvent (AEvent: PRTLEvent);
begin
{$WARNING TODO!}
{
  SetEvent(THANDLE(AEvent));
}
end;


CONST INFINITE=-1;

procedure IntRTLEventWaitFor (AEvent: PRTLEvent);
begin
{$WARNING TODO!}
{
  WaitForSingleObject(THANDLE(AEvent), INFINITE);
}
end;



var
  OS2ThreadManager: TThreadManager;


procedure InitSystemThreads;
begin
  with OS2ThreadManager do
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
    BasicEventCreate       :=@IntBasicEventCreate;
    BasicEventDestroy      :=@IntBasicEventDestroy;
    BasicEventResetEvent   :=@IntBasicEventResetEvent;
    BasicEventSetEvent     :=@IntBasicEventSetEvent;
    BasiceventWaitFor      :=@IntBasiceventWaitFor;
    RTLEventCreate         :=@IntRTLEventCreate;
    RTLEventDestroy        :=@IntRTLEventDestroy;
    RTLEventSetEvent       :=@IntRTLEventSetEvent;
    RTLEventWaitFor        :=@IntRTLEventWaitFor;
    end;
  SetThreadManager (OS2ThreadManager);
  InitHeapMutexes;
end;


