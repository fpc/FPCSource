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

{$WARNING These should be in the system unit }
{ some BeginThread flags we support }
const
  CREATE_SUSPENDED = 1;
  STACK_SIZE_PARAM_IS_A_RESERVATION = 2;

procedure SetAThreadBaseName(s: String);

implementation

uses
  sysutils, exec, amigados, utility;

const
  threadvarblocksize : dword = 0;

var
  SubThreadBaseName: String = 'FPC Subthread';

{.$define DEBUG_MT}
type
  TThreadOperation = ( toNone, toStart, toResume, toExit );

type
  PThreadMsg = ^TThreadMsg;

  PThreadInfo = ^TThreadInfo;
  TThreadInfo = record
    threadVars: Pointer;     { have threadvars ptr as first field, so no offset is needed to access it (faster) }
    threadVarsSize: DWord;   { size of the allocated threadvars block }
    nextThread: PThreadInfo; { threadinfos are a linked list, using this field }
    threadID: TThreadID;     { thread ID, as returned by CreateNewProc() }
    stackLen: PtrUInt;       { stack size the thread was construced with }
    exitCode: Pointer;       { exitcode after the process has exited     }
    f: TThreadFunc;          { ThreadFunc function pointer }
    p: Pointer;              { ThreadFunc argument }
    flags: dword;            { Flags this thread were created with }
    num: longint;            { This was the "num"th thread to created }
    mainthread: boolean;     { true if this is our main thread }
    exited: boolean;         { true if the thread has exited, and can be cleaned up }
    suspended: boolean;      { true if the thread was started suspended, and not resumed yet }
    replyPort: PMsgPort;     { Amiga exec.library IPC message reply port }
    replyMsg: PMessage;      { exit message for the thread }
    name: String;            { Thread's name }
  end;

  TThreadMsg = record
    tm_MsgNode   : TMessage;
    tm_ThreadInfo: PThreadInfo;
    tm_Operation : TThreadOperation;
  end;

var
  AThreadManager: TThreadManager;
  AThreadList: PThreadInfo;
  AThreadListLen: LongInt;
  AThreadNum: LongInt;
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
  inc(AThreadNum);
  ti^.num:=AThreadNum;
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

{ Helper function for IPC }
procedure SendMessageToThread(var threadMsg: TThreadMsg; p: PThreadInfo; const op: TThreadOperation; waitReply: boolean);
begin
  FillChar(threadMsg,sizeof(threadMsg),0);
  with threadMsg do
    begin
      with tm_MsgNode do
        begin
          mn_Node.ln_Type:=NT_MESSAGE;
          mn_Length:=SizeOf(TThreadMsg);
          if waitReply then
            mn_ReplyPort:=p^.replyPort
          else
            mn_ReplyPort:=nil;
        end;
      tm_ThreadInfo:=p;
      tm_Operation:=op;
    end;
  PutMsg(@PProcess(p^.threadID)^.pr_MsgPort,@threadMsg);

  if waitReply then
    begin
      WaitPort(p^.replyPort);
      GetMsg(p^.replyPort);
    end;
end;

procedure SetAThreadBaseName(s: String);
begin
  ObtainSemaphore(@AThreadListSemaphore);
  SubThreadBaseName:=s;
  ReleaseSemaphore(@AThreadListSemaphore);
end;

function GetAThreadBaseName: String;
begin
  ObtainSemaphore(@AThreadListSemaphore);
  GetAThreadBaseName:=SubThreadBaseName;
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
  p: PThreadInfo;
begin
{$IFDEF DEBUG_MT}
  {SysDebugLn('FPC AThreads: RelocateThreadvar');}
{$ENDIF}
  p:=PThreadInfo(PProcess(FindTask(nil))^.pr_Task.tc_UserData);
  if (p <> nil) and (p^.threadVars <> nil) then
    result:=p^.threadVars + Offset
  else
    result:=nil;
end;


procedure AAllocateThreadVars;
var
  p: PThreadInfo;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Allocating threadvars');
{$endif}
  { we've to allocate the memory from system  }
  { because the FPC heap management uses      }
  { exceptions which use threadvars but       }
  { these aren't allocated yet ...            }
  { allocate room on the heap for the thread vars }
  p:=PThreadInfo(PProcess(FindTask(nil))^.pr_Task.tc_UserData);
  if p <> nil then
    begin
{$ifdef AMIGA}
      ObtainSemaphore(ASYS_heapSemaphore);
{$endif}
      p^.threadVars:=AllocPooled(ASYS_heapPool,threadvarblocksize);
      if p^.threadVars = nil then
        SysDebugLn('FPC AThreads: Failed to allocate threadvar memory!')
      else
        begin
          p^.threadVarsSize:=threadvarblocksize;
          FillChar(p^.threadVars^,threadvarblocksize,0);
        end;
{$ifdef AMIGA}
      ReleaseSemaphore(ASYS_heapSemaphore);
{$endif}
    end
  else
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: AllocateThreadVars: tc_UserData of this process was nil!')
{$endif}
    end;
end;


procedure AReleaseThreadVars;
var
  p: PThreadInfo;
begin
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Releasing threadvars');
{$endif}
  p:=PThreadInfo(PProcess(FindTask(nil))^.pr_Task.tc_UserData);
  if (p <> nil) and (p^.threadVars <> nil) then
    begin
{$ifdef AMIGA}
      ObtainSemaphore(ASYS_heapSemaphore);
{$endif}
      FreePooled(ASYS_heapPool,p^.threadVars,p^.threadVarsSize);
{$ifdef AMIGA}
      ReleaseSemaphore(ASYS_heapSemaphore);
{$endif}
    end
  else
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: ReleaseThreadVars: tc_UserData or threadVars area of this process was nil!')
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
      FillChar(threadInfo^,sizeof(TThreadInfo),0);
      p^.pr_Task.tc_UserData:=threadInfo;
      threadInfo^.replyPort:=@p^.pr_MsgPort;
      threadInfo^.mainThread:=true;
      threadInfo^.threadID:=TThreadID(p);
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
  resumeMsg: PThreadMsg;
  exitSuspend: boolean; // true if we have to exit instead of suspend
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
  { first debug line can't be before threadvar allocation }
  SysDebugLn('FPC AThreads: Entering subthread function, ID:'+hexStr(thisThread));
{$endif}

  if threadInfo^.name <> '' then
    begin
{$ifdef DEBUG_MT}
      { this line can't be before threadvar allocation }
      SysDebugLn('FPC AThreads: Renaming thread ID:'+hexStr(thisThread)+' to '+threadInfo^.name);
{$endif}
      thisThread^.pr_Task.tc_Node.ln_Name:=PChar(@threadInfo^.name[1]);
    end;

  { Reply the message, so the calling thread could continue }
  { note that threadMsg was allocated on the caller's task, so }
  { it will be invalid below this point }
  ReplyMsg(PMessage(threadMsg));

  { if creating a suspended thread, wait for the wakeup message to arrive }
  { then check if we actually have to resume, or exit }
  exitSuspend:=false;
  if threadInfo^.suspended then
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Suspending subthread on entry, ID:'+hexStr(thisThread));
{$endif}
      WaitPort(@thisThread^.pr_MsgPort);
      resumeMsg:=PThreadMsg(GetMsg(@thisThread^.pr_MsgPort));
      exitSuspend:=resumeMsg^.tm_Operation <> toResume;
      threadInfo^.suspended:=false;
      ReplyMsg(PMessage(resumeMsg));
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Resuming subthread on entry, ID:'+hexStr(thisThread)+', resumed only to exit: '+inttostr(ord(exitSuspend)));
{$endif}
    end;

  { Finally, call the user code }
  if not exitSuspend then
    begin
      InitThread(threadInfo^.stackLen);
      threadInfo^.exitCode:=Pointer(threadInfo^.f(threadInfo^.p));
      DoneThread;
    end;

{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Exiting Subthread function, ID:'+hexStr(thisThread));
{$endif}
  Forbid();
  threadInfo^.exited:=true;

  { Send our exit message... }
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
  FillChar(threadInfo^,sizeof(TThreadInfo),0);
  threadInfo^.f:=ThreadFunction;
  threadInfo^.p:=p;
  if (creationFlags and STACK_SIZE_PARAM_IS_A_RESERVATION) > 0 then
    threadInfo^.stackLen:=stacksize
  else
    threadInfo^.stackLen:=System.StackLength; { inherit parent's stack size }
  threadInfo^.suspended:=(creationFlags and CREATE_SUSPENDED) > 0;

{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Starting new thread... Stack size: '+inttostr(threadInfo^.stackLen));
{$endif}
  subThread:=CreateNewProc([
                           {$IFDEF MORPHOS}
                           NP_CodeType,CODETYPE_PPC,
                           NP_PPCStackSize, threadInfo^.stacklen,
                           {$ELSE}
                           NP_StackSize, threadInfo^.stacklen,
                           {$ENDIF}
                           NP_Entry,PtrUInt(@ThreadFunc),
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
{$ifdef DEBUG_MT}
  SysDebugLn('FPC AThreads: Sending start message to subthread and waiting for reply ID:'+hexStr(subThread));
{$endif}
  AddToThreadList(AThreadList,threadInfo);
  { AddToThreadList assigned us a number, so use it to name the thread }
  threadInfo^.name:=GetAThreadBaseName+' #'+inttostr(threadInfo^.num);
  SendMessageToThread(threadMsg,threadInfo,toStart,true);

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
var
  m: TThreadMsg;
  p: PThreadInfo;
begin
  AResumeThread:=0;
  Forbid();
  p:=GetThreadInfo(AThreadList,threadHandle);
  if (p <> nil) and p^.suspended then
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Waiting for thread to resume, ID:'+hexStr(Pointer(threadHandle)));
{$endif}
      { WaitPort in SendMessageToThread will break the Forbid() state... }
      SendMessageToThread(m,p,toResume,true);
      AResumeThread:=0;
    end
  else
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Error, attempt to resume a non-suspended thread, or invalid thread ID:'+hexStr(Pointer(threadHandle)));
{$endif}
      AResumeThread:=dword(-1);
    end;
  Permit();
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
  p: PThreadInfo;
  m: TThreadMsg;
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
      { WaitPort in SendMessageToThread will break the Forbid() state... }
      if p^.suspended then
        SendMessageToThread(m,p,toExit,true);

      { WaitPort will break the Forbid() state... }
      WaitPort(p^.replyPort);
      GetMsg(p^.replyPort);
      AWaitForThreadTerminate:=DWord(p^.exitCode);
    end
  else
    begin
{$ifdef DEBUG_MT}
      SysDebugLn('FPC AThreads: Error, attempt to wait for invalid thread ID to exit, ID:'+hexStr(Pointer(threadHandle)));
{$endif}
      AWaitForThreadTerminate:=dword(-1); { Return non-zero code on error. }
    end;
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
  SysDebugLn('FPC AThreads: InitCriticalSection '+hexStr(@CS));
{$ENDIF}
  InitSemaphore(PSignalSemaphore(@CS));
end;


procedure AEnterCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: EnterCriticalSection '+hexStr(@CS));
{$ENDIF}
  ObtainSemaphore(PSignalSemaphore(@CS));
end;


function ATryEnterCriticalSection(var CS):longint;
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: TryEnterCriticalSection '+hexStr(@CS));
{$ENDIF}
  result:=DWord(AttemptSemaphore(PSignalSemaphore(@CS)));
  if result<>0 then
    result:=1;
end;


procedure ALeaveCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: LeaveCriticalSection '+hexStr(@CS));
{$ENDIF}
  ReleaseSemaphore(PSignalSemaphore(@CS));
end;


procedure ADoneCriticalSection(var CS);
begin
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: DoneCriticalSection '+hexStr(@CS));
{$ENDIF}
  { unlock as long as unlocking works to unlock it if it is recursive
    some Delphi code might call this function with a locked mutex }
  with TSignalSemaphore(CS) do
    while ss_NestCount > 0 do
      ReleaseSemaphore(PSignalSemaphore(@CS));
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
  AThreadNum:=-1; { Mainthread will be 0. }
  InitSemaphore(@AThreadListSemaphore);
  SetAThreadManager;

finalization
{$IFDEF DEBUG_MT}
  SysDebugLn('FPC AThreads: Unit Finalization');
{$ENDIF}
  WaitForAllThreads;
end.
