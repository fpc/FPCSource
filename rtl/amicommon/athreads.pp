{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Karoly Balogh,
    member of the Free Pascal development team.

    native threadmanager implementation for Amiga-like systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
unit athreads;

interface

implementation

uses
  sysutils, exec, amigados, utility;

const
  threadvarblocksize : dword = 0;

{.$define DEBUG_MT}

type
  PThreadInfo = ^TThreadInfo;
  TThreadInfo = record
    threadVars: Pointer; { have threadvars ptr as first field,
                           so no offset is needed to access it (faster) }
    nextThread: PThreadInfo;
    threadID: TThreadID;
    stackLen: PtrUInt;
    exitCode: Pointer;
    f: TThreadFunc;
    p: Pointer;
    name: String;
    mainthread: boolean;
    exited: boolean;
    replyPort: PMsgPort;
    replyMsg: PMessage;
  end;

  PThreadMsg = ^TThreadMsg;
  TThreadMsg = record
    tm_MsgNode   : TMessage;
    tm_ThreadInfo: PThreadInfo;
  end;

var
  AThreadManager: TThreadManager;
  AThreadList: PThreadInfo;
  AThreadListLen: LongInt;
  AThreadListSemaphore: TSignalSemaphore;

{ Function to add a thread to the running threads list }
procedure AddToThreadList(var l: PThreadInfo; ti: PThreadInfo);
var
  p     : PThreadInfo;
  inList: Boolean;
begin
  inList:=False;
  ObtainSemaphore(@AThreadListSemaphore);
  if l = nil then
    { if the list is not yet allocated, the newly added
      threadinfo will be the first item }
    l:=ti
  else
    begin
      { otherwise, look for the last item and append }
      p:=l;
      while (p^.nextThread<>nil) do p:=p^.nextThread;
      p^.nextThread:=ti;
    end;
  inc(AThreadListLen);
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: thread ID:'+hexstr(Pointer(ti^.threadID))+' added, now '+inttostr(AThreadListLen)+' thread(s) in list.');
{$ENDIF}
  ReleaseSemaphore(@AThreadListSemaphore);
end;

{ Function to remove a thread from running threads list }
function RemoveFromThreadList(var l: PThreadInfo; threadID: TThreadID): boolean;
var
  p      : PThreadInfo;
  pprev  : PThreadInfo;
  inList : Boolean;
  tmpNext: PThreadInfo;
  tmpInfo: PThreadInfo;
begin
  inList:=False;
  if l=nil then
    begin
      RemoveFromThreadList:=inList;
      exit;
    end;

  ObtainSemaphore(@AThreadListSemaphore);
  p:=l;
  pprev:=nil;
  while (p <> nil) and (p^.threadID <> threadID) do
    begin
      pprev:=p;
      p:=p^.nextThread;
    end;

  if p <> nil then
    begin
      tmpNext:=p^.nextThread;
      if not p^.mainthread and p^.exited then
        begin
          while GetMsg(p^.replyPort) <> nil do begin end;
          DeleteMsgPort(p^.replyPort);
          dispose(p^.replyMsg);
          dispose(p);
          if pprev <> nil then
            pprev^.nextThread:=tmpNext;
          Dec(AThreadListLen);
        end
      else
        begin
{$IFDEF DEBUG_MT}
          SysDebugLn('FPC AThreads: Error! Attempt to remove threadID, which is the mainthread or not exited:'+hexStr(Pointer(threadID)));
{$ENDIF}
          inList:=false;
        end;
    end
{$IFDEF DEBUG_MT}
  else
    SysDebugLn('FPC AThreads: Error! Attempt to remove threadID, which is not in list:'+hexStr(Pointer(threadID)))
{$ENDIF}
  ;
  ReleaseSemaphore(@AThreadListSemaphore);

  RemoveFromThreadList:=inList;
end;

{ Function to return a function's ThreadInfo based on the threadID }
function GetThreadInfo(var l: PThreadInfo; threadID: TThreadID): PThreadInfo;
var
  p     : PThreadInfo;
  inList: Boolean;
begin
  inList:=False;
  GetThreadInfo:=nil;
  if l = nil then
    exit;

  ObtainSemaphore(@AThreadListSemaphore);
  p:=l;
  while (p <> nil) and (p^.threadID <> threadID) do 
    p:=p^.nextThread;
  GetThreadInfo:=p;
  ReleaseSemaphore(@AThreadListSemaphore);
end;

{ Returns the number of threads still not exited in our threadlist }
function CountRunningThreads(var l: PThreadInfo): LongInt;
var
  p: PThreadInfo;
begin
  CountRunningThreads:=0;
  ObtainSemaphore(@AThreadListSemaphore);
  p:=l;
  while p <> nil do
    begin
      inc(CountRunningThreads,ord(not p^.exited));
      p:=p^.nextThread;
    end;
  ReleaseSemaphore(@AThreadListSemaphore); 
end;


procedure AInitThreadvar(var offset : dword;size : dword);
begin
{$IFDEF DEBUG_MT}
  {SysDebugLn('FPC AThreads: InitThreadvar');}
{$ENDIF}
  offset:=threadvarblocksize;
  inc(threadvarblocksize,size);
end;


function ARelocateThreadvar(offset : dword) : pointer;
var
  userData: Pointer;
begin
{$IFDEF DEBUG_MT}
  {SysDebugLn('FPC AThreads: RelocateThreadvar');}
{$ENDIF}
  userData:=PProcess(FindTask(nil))^.pr_Task.tc_UserData;
  if userData = nil then
    result:=nil
  else
    result:=PThreadInfo(userData)^.threadVars + Offset;
end;


procedure AAllocateThreadVars;
var
  userData: pointer;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Allocating threadvars');
{$endif}
  { we've to allocate the memory from system  }
  { because the FPC heap management uses      }
  { exceptions which use threadvars but       }
  { these aren't allocated yet ...            }
  { allocate room on the heap for the thread vars }
  userData:=PProcess(FindTask(nil))^.pr_Task.tc_UserData;
  if userData <> nil then
    PThreadInfo(userData)^.threadVars:=AllocVec(threadvarblocksize,MEMF_CLEAR)
  else
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: AllocateThreadVars: tc_UserData of this process was nil!')
{$endif}
    end;
end;


procedure AReleaseThreadVars;
var
  userData: pointer;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Releasing threadvars');
{$endif}
  userData:=PProcess(FindTask(nil))^.pr_Task.tc_UserData;
  if userdata <> nil then
    FreeVec(PThreadInfo(userData)^.threadVars)
  else
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: ReleaseThreadVars: tc_UserData of this process was nil!')
{$endif}
    end;
end;


procedure InitAThreading;
var
  threadInfo: PThreadInfo;
  p: PProcess;
begin
  if (InterLockedExchange(longint(IsMultiThread),ord(true)) = 0) then
    begin
      { We're still running in single thread mode, setup the TLS }
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Entering multithreaded mode...');
{$endif}
      p:=PProcess(FindTask(nil));
      new(threadInfo);
      p^.pr_Task.tc_UserData:=threadInfo;
      threadInfo^.replyPort:=@p^.pr_MsgPort;
      threadInfo^.mainThread:=true;
      threadInfo^.exited:=false;
      threadInfo^.threadID:=TThreadID(p);
      threadInfo^.replyMsg:=nil;
      threadInfo^.f:=nil;
      threadInfo^.p:=nil;
      InitThreadVars(@ARelocateThreadvar);
      AddToThreadList(AThreadList,threadInfo);
    end;
end;


{$IFDEF DEBUG_MT}
{$PUSH}
{ Because the string concat in SysDebugLn causes exception frames }
{$IMPLICITEXCEPTIONS OFF}
{$ENDIF}
procedure ThreadFunc; cdecl;
var
  thisThread: PProcess;
  threadMsg: PThreadMsg;
  threadInfo: PThreadInfo;
begin
  thisThread:=PProcess(FindTask(nil));

  { wait for our start message to arrive, then fetch it }
  WaitPort(@thisThread^.pr_MsgPort);
  threadMsg:=PThreadMsg(GetMsg(@thisThread^.pr_MsgPort));

  { fetch existing threadinfo from the start message, and set 
    it to tc_userData, so we can proceed with threadvars }
  threadInfo:=threadMsg^.tm_ThreadInfo;
  thisThread^.pr_Task.tc_userData:=threadInfo;

  { Allocate local thread vars, this must be the first thing,
    because the exception management and io depends on threadvars }
  AAllocateThreadVars;

{$ifdef DEBUG_MT}
  { this line can't be before threadvar allocation }
  SysDebugLn('FPC AThreads: Entering Subthread function, ID:'+hexStr(thisThread));
{$endif}

  { Reply the message, so the calling thread could continue }
  { note that threadMsg was allocated on the caller's task, so }
  { it will be invalid below this point }
  ReplyMsg(PMessage(threadMsg));

  InitThread(threadInfo^.stackLen);
  threadInfo^.exitCode:=Pointer(threadInfo^.f(threadInfo^.p));
  DoneThread;

{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Exiting Subthread function, ID:'+hexStr(thisThread));
{$endif}
  Forbid();
  threadInfo^.exited:=true;
  with threadInfo^.replyMsg^ do
    begin
      mn_Node.ln_Type:=NT_MESSAGE;
      mn_Length:=SizeOf(TMessage);
      mn_ReplyPort:=nil;
    end;
  Forbid();
  threadInfo^.exited:=true;
  PutMsg(threadInfo^.replyPort,threadInfo^.replyMsg);
end;
{$IFDEF DEBUG_MT}
{$POP} { reset implicitexceptions state }
{$ENDIF}


function CreateNewProc(Tags : Array Of PtrUInt) : PProcess;
begin
  CreateNewProc:=CreateNewProcTagList(@Tags);
end;

function ABeginThread(sa : Pointer;stacksize : PtrUInt;
                      ThreadFunction : tthreadfunc;p : pointer;
                      creationFlags : dword; var ThreadId : TThreadId) : TThreadID;
var
  threadInfo: PThreadInfo;
  threadMsg: TThreadMsg;
  threadName: String;
  replyPort: PMsgPort;
  subThread: PProcess;
begin
  ABeginThread:=TThreadID(0);

{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Creating new thread...');
{$endif DEBUG_MT}
  { Initialize multithreading if not done }
  if not IsMultiThread then
    InitAThreading;
  { the only way to pass data to the newly created thread
    in a MT safe way, is to use the heap }
  new(threadInfo);
  threadInfo^.f:=ThreadFunction;
  threadInfo^.p:=p;
  threadInfo^.stackLen:=stacksize;
  threadInfo^.exited:=false;
  threadInfo^.mainThread:=false;

{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Starting new thread...');
{$endif}
  subThread:=CreateNewProc([
                           {$IFDEF MORPHOS}
                           NP_CodeType,CODETYPE_PPC,
                           NP_PPCStackSize, stacksize,
                           {$ELSE}
                           NP_StackSize, stacksize,
                           {$ENDIF}
                           NP_Entry,PtrUInt(@ThreadFunc),
                           NP_Name,PtrUInt(PChar('FPC Subthread')),
                           TAG_DONE]);
  if subThread = nil then
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Failed to start the subthread!');
{$endif}
      exit;
    end;
  replyPort:=CreateMsgPort;

  ThreadID:=TThreadID(subThread);
  threadInfo^.threadID:=ThreadID;
  threadInfo^.replyPort:=replyPort;
  new(threadInfo^.replyMsg);

  // the thread should be started here, and waiting 
  // for our start message, so send it
  FillChar(threadMsg,sizeof(threadMsg),0);
  with threadMsg do
    begin
      with tm_MsgNode do
        begin
          mn_Node.ln_Type:=NT_MESSAGE;
          mn_Length:=SizeOf(TThreadMsg);
          mn_ReplyPort:=replyPort;
        end;
      tm_ThreadInfo:=threadInfo;
    end;
  AddToThreadList(AThreadList,threadInfo);

{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Sending start message to subthread ID:'+hexStr(subThread));
{$endif}
  PutMsg(@subThread^.pr_MsgPort,PMessage(@threadMsg));

  { wait for a reply, so we know the thread has initialized properly }
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Waiting for reply...');
{$endif}
  WaitPort(replyPort);
  GetMsg(replyPort);

  ABeginThread:=ThreadId;
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Thread created successfully, ID:'+hexStr(subThread));
{$endif}
end;


procedure AEndThread(ExitCode : DWord);
begin
  DoneThread;
end;


function ASuspendThread (threadHandle : TThreadID) : dword;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: unsupported operation: SuspendThread called for ID:'+hexStr(Pointer(threadHandle)));
{$endif}
  // cannot be properly supported on Amiga
  result:=dword(-1);
end;


function AResumeThread (threadHandle : TThreadID) : dword;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: unsupported operation: ResumeThread called for ID:'+hexStr(Pointer(threadHandle)));
{$endif}
  // cannot be properly supported on Amiga
  result:=dword(-1);
end;


procedure AThreadSwitch;  {give time to other threads}
begin
  { On Unix, this calls sched_yield();
    Harry 'Piru' Sintonen recommended to emulate this on Amiga systems with
    exec/Forbid-exec/Permit pair which is pretty fast to execute and will
    trigger a rescheduling.
    Another idea by Frank Mariak was to use exec/SetTaskPri() with the same
    priority }
  Forbid();
  Permit();
end;


function AKillThread (threadHandle : TThreadID) : dword;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: unsupported operation: KillThread called for ID:'+hexStr(Pointer(threadHandle)));
{$endif}
  // cannot be properly supported on Amiga
  AKillThread:=dword(-1);
end;


function ACloseThread (threadHandle : TThreadID) : dword;
begin
{$WARNING The return value here seems to be undocumented}
  RemoveFromThreadList(AThreadList, threadHandle);
  result:=0;
end;


function AWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
var
  LResultP: Pointer;
  p: PThreadInfo;
begin
{.$WARNING Support for timeout argument is not implemented}
{ But since CThreads uses pthread_join, which has also no timeout,
  I don't think this is a big issue. (KB) }
  AWaitForThreadTerminate:=0;
  Forbid();
  p:=GetThreadInfo(AThreadList,threadHandle);
  if (p <> nil) then
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Waiting for thread to exit, ID:'+hexStr(Pointer(threadHandle)));
{$endif}
      { WaitPort will break the Forbid() state... }
      WaitPort(p^.replyPort);
      GetMsg(p^.replyPort);
      AWaitForThreadTerminate:=DWord(p^.exitCode);
    end
  else
{$ifdef DEBUG_MT}
    SysDebugLn('FPC AThreads: Error, attempt to wait for invalid thread ID to exit, ID:'+hexStr(Pointer(threadHandle)))
{$endif}
  ;
  Permit();
end;


function AThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean; {-15..+15, 0=normal}
begin
  {$Warning ThreadSetPriority needs to be implemented}
  result:=false;
end;


function AThreadGetPriority (threadHandle : TThreadID): Integer;
begin
  {$Warning ThreadGetPriority needs to be implemented}
  result:=0;
end;


function AGetCurrentThreadId : TThreadID;
begin
  AGetCurrentThreadId := TThreadID(FindTask(nil));
end;


Type  PINTRTLEvent = ^TINTRTLEvent;
      TINTRTLEvent = record
        isset: boolean;
      end;

Function intRTLEventCreate: PRTLEvent;

var p:pintrtlevent;

begin
  new(p);
  result:=PRTLEVENT(p);
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);

var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  dispose(p);
end;

procedure intRTLEventSetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  p^.isset:=true;
end;


procedure intRTLEventResetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  p^.isset:=false;
end;


procedure intRTLEventWaitFor(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  p^.isset:=false;
end;

procedure intRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);
var
  p : pintrtlevent;
begin
  p:=pintrtlevent(aevent);
end;


procedure AInitCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: InitCriticalSection $'+hexStr(@CS));
{$ENDIF}
  PSignalSemaPhore(CS):=AllocVec(sizeof(TSignalSemaphore),MEMF_CLEAR);
  InitSemaphore(PSignalSemaphore(CS));
end;


procedure AEnterCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: EnterCriticalSection $'+hexStr(@CS));
{$ENDIF}
  ObtainSemaphore(PSignalSemaphore(CS));
end;


function ATryEnterCriticalSection(var CS):longint;
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: TryEnterCriticalSection $'+hexStr(@CS));
{$ENDIF}
  result:=DWord(AttemptSemaphore(PSignalSemaphore(CS)));
  if result<>0 then
    result:=1;
end;


procedure ALeaveCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: LeaveCriticalSection $'+hexStr(@CS));
{$ENDIF}
  ReleaseSemaphore(PSignalSemaphore(CS));
end;


procedure ADoneCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: DoneCriticalSection $'+hexStr(@CS));
{$ENDIF}
  { unlock as long as unlocking works to unlock it if it is recursive
    some Delphi code might call this function with a locked mutex }
  with PSignalSemaphore(CS)^ do
    while ss_NestCount > 0 do
      ReleaseSemaphore(PSignalSemaphore(CS));
  FreeVec(Pointer(CS));
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


function ASemaphoreInit: Pointer;
begin
  result:=nil;
end;

procedure ASemaphoreDestroy(const FSem: Pointer);
begin
end;

procedure ASemaphoreWait(const FSem: Pointer);
begin
end;

procedure ASemaphorePost(const FSem: Pointer);
begin
end;


function AInitThreads : Boolean;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Entering InitThreads...');
{$endif}
  result:=true;

  ThreadID := TThreadID(FindTask(nil));
{$ifdef DEBUG_MT}
{$endif DEBUG_MT}
  // We assume that if you set the thread manager, the application is multithreading.
  InitAThreading;
end;

function ADoneThreads : Boolean;
begin
  result:=true;
end;


procedure SetAThreadManager;
begin
  with AThreadManager do begin
    InitManager            :=@AInitThreads;
    DoneManager            :=@ADoneThreads;
    BeginThread            :=@ABeginThread;
    EndThread              :=@AEndThread;
    SuspendThread          :=@ASuspendThread;
    ResumeThread           :=@AResumeThread;
    KillThread             :=@AKillThread;
    ThreadSwitch           :=@AThreadSwitch;
    CloseThread            :=@ACloseThread;
    WaitForThreadTerminate :=@AWaitForThreadTerminate;
    ThreadSetPriority      :=@AThreadSetPriority;
    ThreadGetPriority      :=@AThreadGetPriority;
    GetCurrentThreadId     :=@AGetCurrentThreadId;
    InitCriticalSection    :=@AInitCriticalSection;
    DoneCriticalSection    :=@ADoneCriticalSection;
    EnterCriticalSection   :=@AEnterCriticalSection;
    TryEnterCriticalSection:=@ATryEnterCriticalSection;
    LeaveCriticalSection   :=@ALeaveCriticalSection;
    InitThreadVar          :=@AInitThreadVar;
    RelocateThreadVar      :=@ARelocateThreadVar;
    AllocateThreadVars     :=@AAllocateThreadVars;
    ReleaseThreadVars      :=@AReleaseThreadVars;
    BasicEventCreate       :=@intBasicEventCreate;
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasicEventWaitFor;
    rtlEventCreate         :=@intrtlEventCreate;
    rtlEventDestroy        :=@intrtlEventDestroy;
    rtlEventSetEvent       :=@intrtlEventSetEvent;
    rtlEventResetEvent     :=@intrtlEventResetEvent;
    rtleventWaitForTimeout :=@intrtleventWaitForTimeout;
    rtleventWaitFor        :=@intrtleventWaitFor;
    // semaphores
    SemaphoreInit          :=@ASemaphoreInit;
    SemaphoreDestroy       :=@ASemaphoreDestroy;
    SemaphoreWait          :=@ASemaphoreWait;
    SemaphorePost          :=@ASemaphorePost;
  end;
  SetThreadManager(AThreadManager);
end;

Procedure InitSystemThreads; external name '_FPC_InitSystemThreads';


{ This should only be called from the finalization }
procedure WaitForAllThreads;
begin
  { If we are the main thread exiting, we have to wait for our subprocesses to
    exit. Because AmigaOS won't clean up for us. Also, after exiting the main
    thread the OS unloads all the code segments with code potentially still
    running in the background... So even waiting here forever is better than
    exiting with active threads, which will most likely just kill the OS
    immediately. (KB) }
  ObtainSemaphore(@AThreadListSemaphore);

{$IFDEF DEBUG_MT}
  if AThreadListLen > 1 then
    begin
      SysDebugLn('FPC AThreads: We have registered subthreads, checking their status...');
      if CountRunningThreads(AThreadList) > 1 then
        SysDebugLn('FPC AThreads: We have running subthreads, waiting for them to exit...');
    end;
{$ENDIF}

  while CountRunningThreads(AThreadList) > 1 do
    begin
      ReleaseSemaphore(@AThreadListSemaphore);
      DOSDelay(1);
      { Reobtain the semaphore... }
      ObtainSemaphore(@AThreadListSemaphore);
    end;

{$IFDEF DEBUG_MT}
  if AThreadListLen > 1 then
    SysDebugLn('FPC AThreads: All threads exited but some lacking cleanup - resources will be leaked!')
  else
    SysDebugLn('FPC AThreads: All threads exited normally.');
{$ENDIF}
  ReleaseSemaphore(@AThreadListSemaphore);
end;

initialization
  initsystemthreads;
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: Unit Initialization');
{$ENDIF}
  if ThreadingAlreadyUsed then
    begin
      writeln('Threading has been used before athreads was initialized.');
      writeln('Make athreads one of the first units in your uses clause!');
      runerror(211);
    end;
  AThreadList:=nil;
  AThreadListLen:=0;
  InitSemaphore(@AThreadListSemaphore);
  SetAThreadManager;

finalization
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: Unit Finalization');
{$ENDIF}
  WaitForAllThreads;
end.
