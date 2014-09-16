{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by Marcus Sackrow.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}



//type
//  TThreadEntryfunction = function(data: Pointer): Pointer; cdecl;

const
   threadvarblocksize : dword = 0;     // total size of allocated threadvars
   thredvarsmainthread: pointer = nil; // to free the threadvars in the signal handler

var
  ThreadsVarList: array of Pointer;

{$define THREAD_SYSTEM}
{$I arosthreads.inc}

// Thread manager:
procedure SysInitThreadvar(var offset : dword;size : dword);
begin
  //offset:=threadvarblocksize;
  //inc(threadvarblocksize,size);
end;

procedure SaveThreadVars(t: Pointer);
var
  Idx: Integer;
begin
  {Idx := AROSCurrentThread();
  if Idx >= 0 then
  begin
    if Idx > High(ThreadsVarList) then
      SetLength(ThreadsVarList, Idx + 1);
    ThreadsVarList[Idx] := t;  
  end;}
end;

function GetThreadV: Pointer;
var
  Idx: Integer;
begin
  {
  Result := nil;
  Idx := AROSCurrentThread();
  if (Idx >= 0) and (Idx <= High(ThreadsVarList)) then
  begin
    Result := ThreadsVarList[Idx];  
  end;
  }
end;

function SysRelocateThreadvar (offset: dword): Pointer;
begin
  //SysRelocateThreadvar:= GetThreadV + offset; 
end;

procedure SaveThreadV(t: Pointer);
var
  Idx: Integer;
begin
  {Idx := AROSCurrentThread();
  if Idx >= 0 then
  begin
    if Idx > High(ThreadsVarList) then
      SetLength(ThreadsVarList, Idx + 1);
    ThreadsVarList[Idx] := t;  
  end;}
end;

procedure SysAllocateThreadVars;
var
  threadvars: Pointer;
begin
  {threadvars := AllocPooled(AOS_heapPool, threadvarblocksize);
  FillChar(threadvars^, threadvarblocksize, 0);
  SaveThreadV(threadvars);
  if thredvarsmainthread = nil then
    thredvarsmainthread := threadvars;}
end;

procedure SysReleaseThreadVars;
var
  threadvars: Pointer;
begin
  { release thread vars }
  {
  if threadvarblocksize > 0 then
  begin
    threadvars := GetThreadV;
    if threadvars <> nil then
    begin
      FreePooled(AOS_heapPool, threadvars, threadvarblocksize);
      SaveThreadVars(nil);
    end;
  end;}
end;

type
   TThreadInfo = record
      F: TThreadfunc;
      P: Pointer;
   end;
   PThreadinfo = ^TThreadinfo;

function ThreadFunc(Data: Pointer): Pointer; cdecl;
var
  Ti: TThreadinfo; 
begin
  {SysAllocateThreadVars;
  ti := PThreadInfo(Data)^;
  Dispose(PThreadInfo(Data));
  // execute
  ThreadFunc := Pointer(Ti.f(Ti.p));
  DoneThread;} 
end;

function SysBeginThread(Sa: Pointer; StackSize: PtrUInt; ThreadFunction: TThreadfunc; p: Pointer; CreationFlags: dword; var ThreadId: TThreadID): TThreadID;
var
  Ti: PThreadinfo;
begin
  Result := 0;
  if not IsMultiThread then
  begin
    InitThreadVars(@SysRelocateThreadvar);
    IsMultithread:=true;
  end;
  New(Ti);
  Ti^.f := ThreadFunction;
  Ti^.p := p;
  SetLength(ThreadsVarList, 200);
  //SysBeginThread := CreateThread(@ThreadFunc, Ti);
  ThreadID := SysBeginThread;
end;


procedure SysEndThread(ExitCode : DWord);
begin 
  DoneThread;
  //ExitThread(Pointer(ExitCode));
end;


procedure SysThreadSwitch;
begin
  Delay(0);
end;

function SysSuspendThread(ThreadHandle: THandle): dword;
begin
  Result := 0;
end;


function SysResumeThread(ThreadHandle: THandle): dword;
begin
  Result := 0;
end;


function  SysKillThread(threadHandle: THandle): dword;
begin
  SysKillThread := 0;  {not supported for AROS}
end;

function SysWaitForThreadTerminate(threadHandle: THandle; TimeoutMs: LongInt): dword;
begin
  Result := 0; 
end;

function  SysThreadSetPriority (threadHandle : THandle; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  SysThreadSetPriority := true;
end;

function  SysThreadGetPriority (threadHandle : THandle): Longint;
begin
  SysThreadGetPriority := 0;
end;


function SysGetCurrentThreadId: LongInt;
begin
  SysGetCurrentThreadId := AROSCurrentThread;
end;

// Close all Semaphores
procedure SysCloseAllRemainingSemaphores;
var
  i: Integer;
begin
  ObtainSemaphore(@AROSThreadStruct^.MutexListSem);
  i := 0;
  for i := 0 to High(AROSThreadStruct^.MutexList) do
  begin
    if Assigned(AROSThreadStruct^.MutexList[i]) then
    begin
      Dispose(AROSThreadStruct^.MutexList[i]);
    end;  
  end;
  ReleaseSemaphore(@AROSThreadStruct^.MutexListSem);
end;

// Critical Sections (done by Mutex)
procedure SysInitCriticalSection(var cs: TRTLCriticalSection);
begin
  cs := CreateMutex;
  //DebugLn('Create Mutex');
end;

procedure SysDoneCriticalsection(var cs: TRTLCriticalSection);
begin
  //DebugLn('Destroy Mutex');
  if Assigned(cs) then
    DestroyMutex(TRTLCriticalSection(cs));
  cs := nil;  
end;

procedure SysEnterCriticalsection(var cs: TRTLCriticalSection);
begin
  //DebugLn('EnterMutex');
  if Assigned(cs) then
    LockMutex(cs);
end;

function SysTryEnterCriticalsection(var cs: TRTLCriticalSection): longint;
begin
  //DebugLn('TryEnter Mutex');
  Result := 0;
  if Assigned(cs) then
    Result := LongInt(TryLockMutex(cs));
end;

procedure SysLeaveCriticalsection(var cs: TRTLCriticalSection);
begin
  //DebugLn('Leave Mutex');
  if Assigned(cs) then
    UnlockMutex(cs);
end;

function SysSetThreadDataAreaPtr (newPtr:pointer):pointer;
begin
end;

function intBasicEventCreate(EventAttributes : Pointer;
AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
begin
end;

procedure intbasiceventdestroy(state:peventstate);
begin
end;

procedure intbasiceventResetEvent(state:peventstate);
begin
end;

procedure intbasiceventSetEvent(state:peventstate);
begin
end;

function intbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;
begin
end;

function intRTLEventCreate: PRTLEvent;
begin
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);
begin
end;

procedure intRTLEventSetEvent(AEvent: PRTLEvent);
begin
end;

procedure intRTLEventResetEvent(AEvent: PRTLEvent);
begin
end;

procedure intRTLEventWaitFor(AEvent: PRTLEvent);
begin
end;

procedure intRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);
begin
end;


function SysInitManager: Boolean;
begin
  InitThreadLib;
  Result := True;
end;

function SysDoneManager: Boolean;
begin
  FinishThreadLib;
  Result := True;
end;


Var
  AROSThreadManager : TThreadManager;

procedure InitSystemThreads;
begin
  
  with AROSThreadManager do
  begin
    InitManager            :=@SysInitManager;
    DoneManager            :=@SysDoneManager;
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
    InitCriticalSection    :=TCriticalSectionHandler(@SysInitCriticalSection);
    DoneCriticalSection    :=TCriticalSectionHandler(@SysDoneCriticalSection);
    EnterCriticalSection   :=TCriticalSectionHandler(@SysEnterCriticalSection);
    LeaveCriticalSection   :=TCriticalSectionHandler(@SysLeaveCriticalSection);
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
  SetThreadManager(AROSThreadManager); 
  
end;



