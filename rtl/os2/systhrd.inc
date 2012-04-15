{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002-2011 by Tomas Hajny,
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
 sem_Indefinite_Wait = cardinal (-1);
 dtSuspended = 1;
 dtStack_Commited = 2;
 deThread = 0; {DosExit - exit thread only}
 dcWW_Wait = 0;
 dcWW_NoWait = 1;
 dpThread = 2;
 dpSameClass = 0;
 dce_AutoReset = $1000;
 qs_End = 0;
 qs_Process = 1;
 qs_Thread = 256;

type
  PQSTRec = ^TQSTRec;
  TQSTRec = record
    RecType: cardinal;              { Record type }
    TID: word;                      { Thread ID }
    Slot: word;                     { "Unique" thread slot number }
    SleepID: cardinal;              { Sleep ID thread is sleeping on }
    case boolean of
     false: (
      Priority: cardinal;           { Thread priority (class + level) }
      SysTime: cardinal;            { Thread system time }
      UserTime: cardinal;           { Thread user time }
      State: byte;                  { Thread state }
      Pad: array [1..3] of byte);   { Padding for 32-bit alignment }
     true: (
      PrioLevel: byte;              { Thread priority level only }
      PrioClass: byte;              { Thread priority class only }
      Pad2: array [1..14] of byte);
  end;

  PQSPRec = ^TQSPRec;
  TQSPrec = record
    RecType: cardinal;     { Type of record being processed }
    PThrdRec: PQSTRec;     { (Far?) pointer to thread records for this process }
    PID: word;             { Process ID }
    PPID: word;            { Parent process ID }
    ProcType: cardinal;    { Process type }
    Stat: cardinal;        { Process status }
    SGID: cardinal;        { Process screen group }
    hMte: word;            { Program module handle for process }
    cTCB: word;            { Number of TCBs (Thread Control Blocks) in use }
    c32PSem: cardinal;     { Number of private 32-bit semaphores in use }
    p32SemRec: pointer;    { (Far?) pointer to head of 32-bit semaphores info }
    c16Sem: word;          { Number of 16 bit system semaphores in use }
    cLib: word;            { Number of runtime linked libraries }
    cShrMem: word;         { Number of shared memory handles }
    cFH: word;             { Number of open files }
                           { NOTE: cFH is size of active part of   }
                           { the handle table if QS_FILE specified }
    p16SemRec: word;       { Far pointer? to head of 16-bit semaphores info }
    pLibRec: word;         { Far pointer? to list of runtime libraries }
    pShrMemRec: word;      { Far pointer? to list of shared memory handles }
    pFSRec: word;          { Far pointer to list of file handles; }
                           { 0xFFFF means it's closed, otherwise }
                           { it's an SFN if non-zero }
  end;

(* Simplified version here to avoid need for all record types copied here. *)
  PQSPtrRec = ^TQSPtrRec;
  TQSPtrRec = record
    PGlobalRec: pointer;
    PProcRec: PQSPRec;          { Pointer to head of process records }
    P16SemRec: pointer;
    P32SemRec: pointer;
    PMemRec: pointer;
    PLibRec: pointer;
    PShrMemRec: pointer;
    PFSRec: pointer;
  end;

{ import the necessary stuff from the OS }
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): cardinal;
                                          cdecl; external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): cardinal; cdecl;
                                                 external 'DOSCALLS' index 455;

function DosCreateThread (var TID: cardinal; Address: pointer;
(* TThreadFunc *)
     aParam: pointer; Flags: cardinal; StackSize: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 311;

function DosCreateMutExSem (Name: PChar; var Handle: THandle; Attr: cardinal;
              State: cardinal): cardinal; cdecl; external 'DOSCALLS' index 331;

function DosCloseMutExSem (Handle: THandle): cardinal; cdecl;
                                                 external 'DOSCALLS' index 333;

function DosRequestMutExSem (Handle: THandle; Timeout: cardinal): cardinal;
                                          cdecl; external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: THandle): cardinal; cdecl;
                                                 external 'DOSCALLS' index 335;

function DosSuspendThread (TID:cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 238;

function DosResumeThread (TID: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 237;

function DosKillThread (TID: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 111;

function DosWaitThread (var TID: cardinal; Option: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 349;

procedure DosSleep (MSec: cardinal); cdecl; external 'DOSCALLS' index 229;

{
procedure DosExit (Action, Result: cardinal); cdecl;
                                                 external 'DOSCALLS' index 234;

Already declared in the main part of system.pas...
}

function DosSetPriority (Scope, TrClass: cardinal; Delta: longint;
                                           PortID: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 236;

function DosCreateEventSem (Name: PChar; var Handle: THandle;
                             Attr: cardinal; State: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 324;

function DosCloseEventSem (Handle: THandle): cardinal; cdecl;
                                                 external 'DOSCALLS' index 326;

function DosResetEventSem (Handle: THandle; var PostCount: cardinal): cardinal;
                                          cdecl; external 'DOSCALLS' index 327;

function DosPostEventSem (Handle: THandle): cardinal; cdecl;
                                                 external 'DOSCALLS' index 328;

function DosWaitEventSem (Handle: THandle; Timeout: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 329;

function DosQueryEventSem (Handle: THandle; var Posted: cardinal): cardinal;
                                          cdecl; external 'DOSCALLS' index 330;

function DosQuerySysState (EntityList, EntityLevel, PID, TID: cardinal;
                                var Buffer; BufLen: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 368;



{*****************************************************************************
                             Threadvar support
*****************************************************************************}

const
 ThreadVarBlockSize: dword = 0;


const
(* Pointer to an allocated dword space within the local thread *)
(* memory area. Pointer to the real memory block allocated for *)
(* thread vars in this block is then stored in this dword.     *)
 DataIndex: PPointer = nil;


procedure SysInitThreadvar (var Offset: dword; Size: dword);
begin
 Offset := ThreadVarBlockSize;
 Inc (ThreadVarBlockSize, Size);
end;


procedure SysAllocateThreadVars;
begin
 { we've to allocate the memory from the OS }
 { because the FPC heap management uses     }
 { exceptions which use threadvars but      }
 { these aren't allocated yet ...           }
 { allocate room on the heap for the thread vars }
 if DosAllocMem (DataIndex^, ThreadVarBlockSize, pag_Read or pag_Write
                                                       or pag_Commit) <> 0 then
  HandleError (8);
{ The Windows API apparently provides a way to fill the allocated memory with }
{ zeros; we probably need to do it ourselves for compatibility. }
 FillChar (DataIndex^^, 0, ThreadVarBlockSize);
end;


function SysRelocateThreadVar (Offset: dword): pointer;
begin
{ DataIndex itself not checked for not being nil - expected that this should }
{ not be necessary because the equivalent check (i.e. TlsKey not being set)  }
{ is note performed by the Windows implementation.                           }
  if DataIndex^ = nil then
   begin
    SysAllocateThreadVars;
    InitThread ($1000000);
   end;
  SysRelocateThreadVar := DataIndex^ + Offset;
end;


procedure SysInitMultithreading;
begin
  { do not check IsMultiThread, as program could have altered it, out of Delphi habit }

  { the thread attach/detach code uses locks to avoid multiple calls of this }
  if DataIndex = nil then
   begin
    { We're still running in single thread mode, setup the TLS }
    if DosAllocThreadLocalMemory (1, DataIndex) <> 0 then RunError (8);
    InitThreadVars (@SysRelocateThreadvar);
    IsMultiThread := true;
   end;
end;


procedure SysFiniMultithreading;
begin
  if IsMultiThread then
   begin
    if DosFreeThreadLocalMemory (DataIndex) <> 0 then
     begin
{??? What to do if releasing fails?}
     end;
    DataIndex := nil;
   end;
end;


procedure SysReleaseThreadVars;
begin
  DosFreeMem (DataIndex^);
  DataIndex^ := nil;
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


function SysBeginThread (SA: pointer; StackSize : PtrUInt;
                         ThreadFunction: TThreadFunc; P: pointer;
                         CreationFlags: cardinal; var ThreadId: TThreadID): DWord;
var
  TI: PThreadInfo;
begin
{ WriteLn is not a good idea before thread initialization...
  $ifdef DEBUG_MT
  WriteLn ('Creating new thread');
 $endif DEBUG_MT}
{ Initialize multithreading if not done }
  SysInitMultithreading;
{ the only way to pass data to the newly created thread
  in a MT safe way, is to use the heap }
  New (TI);
  TI^.F := ThreadFunction;
  TI^.P := P;
  TI^.StkLen := StackSize;
  ThreadID := 0;
{$ifdef DEBUG_MT}
  WriteLn ('Starting new thread');
{$endif DEBUG_MT}
  if DosCreateThread (cardinal (ThreadID), @ThreadMain, TI,
                                             CreationFlags, StackSize) = 0 then
   SysBeginThread := ThreadID
  else
   begin
    SysBeginThread := 0;
{$IFDEF DEBUG_MT}
    WriteLn ('Thread creation failed');
{$ENDIF DEBUG_MT}
    Dispose (TI);
   end;
end;


procedure SysEndThread (ExitCode: cardinal);
begin
  DoneThread;
  DosExit (0, ExitCode);
end;


procedure SysThreadSwitch;
begin
  DosSleep (0);
end;


function SysSuspendThread (ThreadHandle: dword): dword;
begin
{$WARNING Check expected return value}
  SysSuspendThread := DosSuspendThread (ThreadHandle);
end;


function SysResumeThread (ThreadHandle: dword): dword;
begin
{$WARNING Check expected return value}
  SysResumeThread := DosResumeThread (ThreadHandle);
end;


function SysKillThread (ThreadHandle: dword): dword;
begin
  SysKillThread := DosKillThread (ThreadHandle);
end;

function SysCloseThread (ThreadHandle: TThreadID): dword;
begin
{ Probably not relevant under OS/2? }
//      SysCloseThread:=CloseHandle(threadHandle);
end;

function SysWaitForThreadTerminate (ThreadHandle: dword;
                                                    TimeoutMs: longint): dword;
var
  RC: cardinal;
const
{ Wait at most 100 ms before next check for thread termination }
  WaitTime = 100;
begin
  if TimeoutMs = 0 then
   RC := DosWaitThread (ThreadHandle, dcWW_Wait)
  else
   repeat
    RC := DosWaitThread (ThreadHandle, dcWW_NoWait);
    if RC = 294 then
     begin
      if TimeoutMs > WaitTime then
       DosSleep (WaitTime)
      else
       begin
        DosSleep (TimeoutMs);
        DosWaitThread (ThreadHandle, dcWW_NoWait);
       end;
      Dec (TimeoutMs, WaitTime);
     end;
   until (RC <> 294) or (TimeoutMs <= 0);
  SysWaitForThreadTerminate := RC;
end;


function GetOS2ThreadPriority (ThreadHandle: dword): cardinal;
const
  BufSize = 32768; (* Sufficient space for > 1000 threads (for one process!) *)
var
  PPtrRec: PQSPtrRec;
  PTRec: PQSTRec;
  BufEnd: PtrUInt;
  RC: cardinal;
begin
  GetOS2ThreadPriority := cardinal (-1);
  GetMem (PPtrRec, BufSize);
  if PPtrRec = nil then
   begin
    FreeMem (PPtrRec, BufSize);
    FPC_ThreadError;
   end
  else
   begin
    RC := DosQuerySysState (qs_Process, 0, ProcessID, 0, PPtrRec^, BufSize);
    if (RC = 0) and (PPtrRec^.PProcRec <> nil)
                                  and (PPtrRec^.PProcRec^.PThrdRec <> nil) then
     begin
      BufEnd := PtrUInt (PPtrRec) + BufSize;
      PTRec := PPtrRec^.PProcRec^.PThrdRec;
      while (PTRec^.RecType = qs_Thread) and (PTRec^.TID <> ThreadHandle) and
                                (PtrUInt (PTRec) + SizeOf (PTRec^) < BufEnd) do
       Inc (PTRec);
      if (PTRec^.RecType = qs_Thread) and (PTRec^.TID = ThreadHandle) then
       GetOS2ThreadPriority := PTRec^.Priority;
     end;
    FreeMem (PPtrRec, BufSize);
   end;
end;


type
  TPrio = packed record
    PrioLevel: byte;
    PrioClass: byte;
    Padding: word;
  end;


function SysThreadSetPriority (ThreadHandle: dword; Prio: longint): boolean;
{-15..+15, 0=normal}
var
  Delta: longint;
  Priority: cardinal;
begin
  Priority := GetOS2ThreadPriority (ThreadHandle);
  if Priority > High (word) then
   SysThreadSetPriority := false
  else
   begin
    Delta := Prio * 2;
    if Delta + TPrio (Priority).PrioLevel < 0 then
     Delta := - TPrio (Priority).PrioLevel
    else if Delta + TPrio (Priority).PrioLevel > 31 then
     Delta := 31 - TPrio (Priority).PrioLevel;
    SysThreadSetPriority :=
               DosSetPriority (dpThread, dpSameClass, Delta, ThreadHandle) = 0;
   end;
end;


function SysThreadGetPriority (ThreadHandle: dword): longint;
var
  Priority: cardinal;
begin
  Priority := GetOS2ThreadPriority (ThreadHandle);
(*
  Windows priority levels follow a fairly strange logic; let's mimic at least
  the part related to the idle priority returning negative numbers.
  Result range (based on Windows behaviour) is -15..+15.
*)
  if TPrio (Priority).PrioClass = 1 then
   SysThreadGetPriority := TPrio (Priority).PrioLevel div 2 - 15
  else
   SysThreadGetPriority := TPrio (Priority).PrioLevel div 2;
end;


function SysGetCurrentThreadID: dword;
var
  TIB: PThreadInfoBlock;
begin
  DosGetInfoBlocks (@TIB, nil);
  SysGetCurrentThreadID := TIB^.TIB2^.TID;
end;



{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

procedure SysInitCriticalSection (var CS);
begin
  if DosCreateMutExSem (nil, THandle (CS), 0, 0) <> 0 then
   FPC_ThreadError;
end;

procedure SysDoneCriticalSection (var CS);
begin
(* Trying to release first since this might apparently be the expected  *)
(* behaviour in Delphi according to comment in the Unix implementation. *)
  repeat
  until DosReleaseMutExSem (THandle (CS)) <> 0;
  if DosCloseMutExSem (THandle (CS)) <> 0 then
   FPC_ThreadError;
end;

procedure SysEnterCriticalSection (var CS);
begin
  if DosRequestMutExSem (THandle (CS), cardinal (-1)) <> 0 then
   FPC_ThreadError;
end;

function SysTryEnterCriticalSection (var CS): longint;
begin
  if DosRequestMutExSem (THandle (CS), 0) = 0 then
   Result := 1
  else
   Result := 0;
end;

procedure SysLeaveCriticalSection (var CS);
begin
  if DosReleaseMutExSem (THandle (CS)) <> 0 then
   FPC_ThreadError;
end;



type
  TBasicEventState = record
                      FHandle: THandle;
                      FLastError: longint;
                     end;
  PLocalEventRec = ^TBasicEventState;


const
  wrSignaled  = 0;
  wrTimeout   = 1;
  wrAbandoned = 2;  (* This cannot happen for an event semaphore with OS/2? *)
  wrError     = 3;
  Error_Timeout = 640;
  OS2SemNamePrefix = '\SEM32\';

function SysBasicEventCreate (EventAttributes: Pointer;
     AManualReset, InitialState: boolean; const Name: ansistring): PEventState;
var
  RC: cardinal;
  Name2: ansistring;
  Attr: cardinal;
begin
  New (PLocalEventRec (Result));
  if (Name <> '') and (UpCase (Copy (Name, 1, 7)) <> OS2SemNamePrefix) then
   Name2 := OS2SemNamePrefix + Name
  else
   Name2 := Name;
  if AManualReset then
   Attr := 0
  else
   Attr := DCE_AutoReset;
  if Name2 = '' then
   RC := DosCreateEventSem (nil, PLocalEventRec (Result)^.FHandle,
                                                 Attr, cardinal (InitialState))
  else
   RC := DosCreateEventSem (PChar (Name2), PLocalEventRec (Result)^.FHandle,
                                                Attr, cardinal (InitialState));
  if RC <> 0 then
   begin
    Dispose (PLocalEventRec (Result));
    FPC_ThreadError;
   end;
end;


procedure SysBasicEventDestroy (State: PEventState);
begin
  if State = nil then
   FPC_ThreadError
  else
   begin
    DosCloseEventSem (PLocalEventRec (State)^.FHandle);
    Dispose (PLocalEventRec (State));
   end;
end;


procedure SysBasicEventResetEvent (State: PEventState);
var
  PostCount: cardinal;
begin
  if State = nil then
   FPC_ThreadError
  else
(* In case of later addition of error checking:    *)
(* RC 300 = Error_Already_Reset which would be OK. *)
   DosResetEventSem (PLocalEventRec (State)^.FHandle, PostCount);
end;


procedure SysBasicEventSetEvent (State: PEventState);
begin
  if State = nil then
   FPC_ThreadError
  else
   DosPostEventSem (PLocalEventRec (State)^.FHandle);
end;


function SysBasicEventWaitFor (Timeout: Cardinal; State: PEventState): longint;
var
  RC: cardinal;
begin
  if State = nil then
   FPC_ThreadError
  else
   begin
    RC := DosWaitEventSem (PLocalEventRec (State)^.FHandle, Timeout);
    case RC of
     0: Result := wrSignaled;
     Error_Timeout: Result := wrTimeout;
    else
     begin
      Result := wrError;
      PLocalEventRec (State)^.FLastError := RC;
     end;
    end;
   end;
end;


function SysRTLEventCreate: PRTLEvent;
begin
  Result := PRTLEvent (-1);
  DosCreateEventSem (nil, THandle (Result), dce_AutoReset, 0);
end;


procedure SysRTLEventDestroy (AEvent: PRTLEvent);
begin
  DosCloseEventSem (THandle (AEvent));
end;


procedure SysRTLEventSetEvent (AEvent: PRTLEvent);
begin
  DosPostEventSem (THandle (AEvent));
end;


procedure SysRTLEventWaitFor (AEvent: PRTLEvent);
begin
  DosWaitEventSem (THandle (AEvent), cardinal (-1));
end;


procedure SysRTLEventWaitForTimeout (AEvent: PRTLEvent; Timeout: longint);
begin
  DosWaitEventSem (THandle (AEvent), Timeout);
end;


procedure SysRTLEventResetEvent (AEvent: PRTLEvent);
var
  PostCount: cardinal;
begin
  DosResetEventSem (THandle (AEvent), PostCount);
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
    CloseThread		   :=@SysCloseThread;
    ThreadSwitch           :=@SysThreadSwitch;
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
    BasicEventCreate       :=@SysBasicEventCreate;
    BasicEventDestroy      :=@SysBasicEventDestroy;
    BasicEventSetEvent     :=@SysBasicEventSetEvent;
    BasicEventResetEvent   :=@SysBasicEventResetEvent;
    BasiceventWaitFor      :=@SysBasiceventWaitFor;
    RTLEventCreate         :=@SysRTLEventCreate;
    RTLEventDestroy        :=@SysRTLEventDestroy;
    RTLEventSetEvent       :=@SysRTLEventSetEvent;
    RTLEventResetEvent     :=@SysRTLEventResetEvent;
    RTLEventWaitFor        :=@SysRTLEventWaitFor;
    RTLEventWaitForTimeout :=@SysRTLEventWaitForTimeout;
    end;
  SetThreadManager (OS2ThreadManager);
end;

