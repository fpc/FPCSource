{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2010 by Sven Barth

    Native NT threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                             Native NT API imports
*****************************************************************************}

const
  STATUS_NOT_IMPLEMENTED = LongInt($C0000002);

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

    const
      threadvarblocksize : dword = 0;

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
      begin
      end;

    procedure SysInitMultithreading;
      begin
      end;


    procedure SysFiniMultithreading;
      begin
      end;

    function SysRelocateThreadvar(offset : dword) : pointer;
      begin
        SysRelocateThreadvar:=Pointer(Offset);
      end;


    procedure SysReleaseThreadVars;
      begin
      end;


{*****************************************************************************
                            Thread starting
*****************************************************************************}

    function SysBeginThread(sa : Pointer;stacksize : ptruint;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword;var ThreadId : TThreadID) : TThreadID;
      begin
        ThreadId := 0;
        Result := 0;
      end;


    procedure SysEndThread(ExitCode : DWord);
      begin
        DoneThread;
      end;


    procedure SysThreadSwitch;
    begin
    end;


    function  SysSuspendThread (threadHandle : TThreadID) : dword;
    begin
      Result := STATUS_NOT_IMPLEMENTED;
    end;


    function  SysResumeThread  (threadHandle : TThreadID) : dword;
    begin
      Result := STATUS_NOT_IMPLEMENTED;
    end;


    function  SysKillThread (threadHandle : TThreadID) : dword;
    begin
      Result := STATUS_NOT_IMPLEMENTED;
    end;

    function  SysCloseThread (threadHandle : TThreadID) : dword;
    begin
      Result := STATUS_NOT_IMPLEMENTED;
    end;

    function  SysWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;
    begin
      Result := STATUS_NOT_IMPLEMENTED;
    end;


    function  SysThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean;            {-15..+15, 0=normal}
    begin
      Result := False;
    end;


    function  SysThreadGetPriority (threadHandle : TThreadID): longint;
    begin
      Result := 0;
    end;

    function  SysGetCurrentThreadId : TThreadID;
    begin
      Result := 0;
    end;

{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

procedure SysInitCriticalSection(var cs);
begin
  Pointer(cs) := GetMem(SizeOf(Pointer));
end;


procedure SysDoneCriticalSection(var cs);
begin
  FreeMem(Pointer(cs));
end;


procedure SysEnterCriticalSection(var cs);
begin
end;

function SysTryEnterCriticalSection(var cs):longint;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

procedure SysLeaveCriticalSection(var cs);
begin
end;


function intBasicEventCreate(EventAttributes : Pointer;
AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;
begin
  Result := GetMem(SizeOf(Pointer));
end;

procedure intbasiceventdestroy(state:peventstate);
begin
  FreeMem(state);
end;

procedure intbasiceventResetEvent(state:peventstate);
begin

end;

procedure intbasiceventSetEvent(state:peventstate);
begin
end;

function intbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function intRTLEventCreate: PRTLEvent;
begin
  Result := GetMem(SizeOf(Pointer));
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);
begin
  FreeMem(AEvent);
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


Var
  NTThreadManager : TThreadManager;

Procedure InitSystemThreads;
begin
  With NTThreadManager do
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
  SetThreadManager(NTThreadManager);
{  ThreadID := GetCurrentThreadID;
  if IsLibrary then
    SysInitMultithreading;}
end;

