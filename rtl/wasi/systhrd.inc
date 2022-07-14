{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Nikolay Nikolov,
    member of the Free Pascal development team.

    WASI threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_WASM_THREADS}
  {$fatal This file shouldn't be included if thread support is disabled!}
{$endif FPC_WASM_THREADS}

var
  WasiThreadManager : TThreadManager;

function WasiInitManager: Boolean;
begin
  Result:=True;
end;

function WasiDoneManager: Boolean;
begin
  Result:=True;
end;

function WasiBeginThread(sa : Pointer;stacksize : PtrUInt; ThreadFunction : tthreadfunc;p : pointer;creationFlags : dword; var ThreadId : TThreadID) : TThreadID;
begin
  {todo:implement}
end;

procedure WasiEndThread(ExitCode : DWord);
begin
  {todo:implement}
end;

function WasiSuspendThread(threadHandle : TThreadID) : dword;
begin
  {todo:implement}
end;

function WasiResumeThread(threadHandle : TThreadID) : dword;
begin
  {todo:implement}
end;

function WasiKillThread(threadHandle : TThreadID) : dword;
begin
  {todo:implement}
end;

function WasiCloseThread(threadHandle : TThreadID) : dword;
begin
  {todo:implement}
end;

procedure WasiThreadSwitch;
begin
  {todo:implement}
end;

function WasiWaitForThreadTerminate(threadHandle : TThreadID; TimeoutMs : longint) : dword;
begin
  {todo:implement}
end;

function WasiThreadSetPriority(threadHandle : TThreadID; Prio: longint): boolean;
begin
  {todo:implement}
end;

function WasiThreadGetPriority(threadHandle : TThreadID): longint;
begin
  {todo:implement}
end;

function WasiGetCurrentThreadId : TThreadID;
begin
  {todo:implement}
end;

procedure WasiThreadSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);
begin
  {todo:implement}
end;
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
procedure WasiThreadSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);
begin
  {todo:implement}
end;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}

procedure WasiInitCriticalSection(var cs);
begin
  {todo:implement}
end;

procedure WasiDoneCriticalSection(var cs);
begin
  {todo:implement}
end;

procedure WasiEnterCriticalSection(var cs);
begin
  {todo:implement}
end;

function WasiCriticalSectionTryEnter(var cs):longint;
begin
  {todo:implement}
end;

procedure WasiLeaveCriticalSection(var cs);
begin
  {todo:implement}
end;

procedure WasiInitThreadVar(var offset : dword;size : dword);
begin
  {todo:implement}
end;

function WasiRelocateThreadVar(offset : dword) : pointer;
begin
  {todo:implement}
end;

procedure WasiAllocateThreadVars;
begin
  {todo:implement}
end;

procedure WasiReleaseThreadVars;
begin
  {todo:implement}
end;

function WasiBasicEventCreate(EventAttributes :Pointer;  AManualReset,InitialState : Boolean;const Name:ansistring):pEventState;
begin
  {todo:implement}
end;

procedure WasiBasicEventDestroy(state:peventstate);
begin
  {todo:implement}
end;

procedure WasiBasicEventResetEvent(state:peventstate);
begin
  {todo:implement}
end;

procedure WasiBasicEventSetEvent(state:peventstate);
begin
  {todo:implement}
end;

function WasiBasicEventWaitFor(timeout:cardinal;state:peventstate):longint;
begin
  {todo:implement}
end;

function WasiRTLCreateEvent:PRTLEvent;
begin
  {todo:implement}
end;

procedure WasiRTLEventDestroy(AEvent:PRTLEvent);
begin
  {todo:implement}
end;

procedure WasiRTLEventSetEvent(AEvent:PRTLEvent);
begin
  {todo:implement}
end;

procedure WasiRTLEventResetEvent(AEvent:PRTLEvent);
begin
  {todo:implement}
end;

procedure WasiRTLEventWaitFor(AEvent:PRTLEvent);
begin
  {todo:implement}
end;

procedure WasiRTLEventWaitForTimeout(AEvent:PRTLEvent;timeout : longint);
begin
  {todo:implement}
end;

procedure InitSystemThreads;public name '_FPC_InitSystemThreads';
begin
  with WasiThreadManager do
    begin
      InitManager            := @WasiInitManager;
      DoneManager            := @WasiDoneManager;
      BeginThread            := @WasiBeginThread;
      EndThread              := @WasiEndThread;
      SuspendThread          := @WasiSuspendThread;
      ResumeThread           := @WasiResumeThread;
      KillThread             := @WasiKillThread;
      CloseThread            := @WasiCloseThread;
      ThreadSwitch           := @WasiThreadSwitch;
      WaitForThreadTerminate := @WasiWaitForThreadTerminate;
      ThreadSetPriority      := @WasiThreadSetPriority;
      ThreadGetPriority      := @WasiThreadGetPriority;
      GetCurrentThreadId     := @WasiGetCurrentThreadId;
      SetThreadDebugNameA    := @WasiThreadSetThreadDebugNameA;
{$ifdef FPC_HAS_FEATURE_UNICODESTRINGS}
      SetThreadDebugNameU    := @WasiThreadSetThreadDebugNameU;
{$endif FPC_HAS_FEATURE_UNICODESTRINGS}
      InitCriticalSection    := @WasiInitCriticalSection;
      DoneCriticalSection    := @WasiDoneCriticalSection;
      EnterCriticalSection   := @WasiEnterCriticalSection;
      TryEnterCriticalSection:= @WasiCriticalSectionTryEnter;
      LeaveCriticalSection   := @WasiLeaveCriticalSection;
      InitThreadVar          := @WasiInitThreadVar;
      RelocateThreadVar      := @WasiRelocateThreadVar;
      AllocateThreadVars     := @WasiAllocateThreadVars;
      ReleaseThreadVars      := @WasiReleaseThreadVars;
      BasicEventCreate       := @WasiBasicEventCreate;
      BasicEventDestroy      := @WasiBasicEventDestroy;
      BasicEventResetEvent   := @WasiBasicEventResetEvent;
      BasicEventSetEvent     := @WasiBasicEventSetEvent;
      BasiceventWaitFOr      := @WasiBasicEventWaitFor;
      RTLEventCreate         := @WasiRTLCreateEvent;
      RTLEventDestroy        := @WasiRTLEventDestroy;
      RTLEventSetEvent       := @WasiRTLEventSetEvent;
      RTLEventResetEvent     := @WasiRTLEventResetEvent;
      RTLEventWaitFor        := @WasiRTLEventWaitFor;
      RTLEventWaitForTimeout := @WasiRTLEventWaitForTimeout;
    end;
  SetThreadManager(WasiThreadManager);
end;
