{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    BeOS (bethreads) threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit bethreads;
interface
{$S-}

Procedure SetBeThreadManager;

implementation

Uses
  systhrds,
  BaseUnix,
  unix,
  unixtype,
  sysutils;

{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include OS specific parts. }

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}
    const
      threadvarblocksize : dword = 0;

    var
      TLSKey : pthread_key_t;

    procedure BeInitThreadvar(var offset : dword;size : dword);
      begin
        offset:=threadvarblocksize;
        inc(threadvarblocksize,size);
      end;

    function BeRelocateThreadvar(offset : dword) : pointer;
      begin
        BeRelocateThreadvar:=pthread_getspecific(tlskey)+Offset;
      end;


    procedure BeAllocateThreadVars;
      var
        dataindex : pointer;
      begin
        { we've to allocate the memory from system  }
        { because the FPC heap management uses      }
        { exceptions which use threadvars but       }
        { these aren't allocated yet ...            }
        { allocate room on the heap for the thread vars }
        DataIndex:=Pointer(Fpmmap(nil,threadvarblocksize,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0));
        FillChar(DataIndex^,threadvarblocksize,0);
        pthread_setspecific(tlskey,dataindex);
      end;


    procedure BeReleaseThreadVars;
      begin
        {$ifdef ver1_0}
        Fpmunmap(longint(pthread_getspecific(tlskey)),threadvarblocksize);
        {$else}
        Fpmunmap(pointer(pthread_getspecific(tlskey)),threadvarblocksize);
        {$endif}
      end;

{ Include OS independent Threadvar initialization }

{$endif HASTHREADVAR}


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

    procedure DoneThread;
      begin
        { Release Threadvars }
{$ifdef HASTHREADVAR}
        CReleaseThreadVars;
{$endif HASTHREADVAR}
      end;


    function ThreadMain(param : pointer) : pointer;cdecl;
      var
        ti : tthreadinfo;
{$ifdef DEBUG_MT}
        // in here, don't use write/writeln before having called
        // InitThread! I wonder if anyone ever debugged these routines,
        // because they will have crashed if DEBUG_MT was enabled!
        // this took me the good part of an hour to figure out
        // why it was crashing all the time!
        // this is kind of a workaround, we simply write(2) to fd 0
        s: string[100]; // not an ansistring
{$endif DEBUG_MT}
      begin
{$ifdef DEBUG_MT}
        s := 'New thread started, initing threadvars'#10;
        fpwrite(0,s[1],length(s));
{$endif DEBUG_MT}
{$ifdef HASTHREADVAR}
        { Allocate local thread vars, this must be the first thing,
          because the exception management and io depends on threadvars }
        CAllocateThreadVars;
{$endif HASTHREADVAR}
        { Copy parameter to local data }
{$ifdef DEBUG_MT}
        s := 'New thread started, initialising ...'#10;
        fpwrite(0,s[1],length(s));
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
        DoneThread;
        pthread_detach(pthread_t(pthread_self()));
      end;


    function BeBeginThread(sa : Pointer;stacksize : dword;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword; var ThreadId : THandle) : DWord;
      var
        ti : pthreadinfo;
        thread_attr : pthread_attr_t;
      begin
{$ifdef DEBUG_MT}
        writeln('Creating new thread');
{$endif DEBUG_MT}
        { Initialize multithreading if not done }
        if not IsMultiThread then
         begin
{$ifdef HASTHREADVAR}
          { We're still running in single thread mode, setup the TLS }
           pthread_key_create(@TLSKey,nil);
           InitThreadVars(@CRelocateThreadvar);
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
        pthread_attr_init(@thread_attr);
        pthread_attr_setinheritsched(@thread_attr, PTHREAD_EXPLICIT_SCHED);

        // will fail under linux -- apparently unimplemented
        pthread_attr_setscope(@thread_attr, PTHREAD_SCOPE_PROCESS);

        // don't create detached, we need to be able to join (waitfor) on
        // the newly created thread!
        //pthread_attr_setdetachstate(@thread_attr, PTHREAD_CREATE_DETACHED);
        if pthread_create(@threadid, @thread_attr, @ThreadMain,ti) <> 0 then begin
          threadid := 0;
        end;
        BeBeginThread:=threadid;
{$ifdef DEBUG_MT}
        writeln('BeginThread returning ',BeBeginThread);
{$endif DEBUG_MT}
      end;


    procedure BeEndThread(ExitCode : DWord);
      begin
        DoneThread;
        pthread_detach(pthread_t(pthread_self()));
        pthread_exit(pointer(ExitCode));
      end;


{$warning threadhandle can be larger than a dword}
    function  BeSuspendThread (threadHandle : dword) : dword;
    begin
      {$Warning SuspendThread needs to be implemented}
    end;

{$warning threadhandle can be larger than a dword}
    function  BeResumeThread  (threadHandle : dword) : dword;
    begin
      {$Warning ResumeThread needs to be implemented}
    end;

    procedure CThreadSwitch;  {give time to other threads}
    begin
      {extern int pthread_yield (void) __THROW;}
      {$Warning ThreadSwitch needs to be implemented}
    end;

{$warning threadhandle can be larger than a dword}
    function  BeKillThread (threadHandle : dword) : dword;
    begin
      pthread_detach(pthread_t(threadHandle));
      CKillThread := pthread_cancel(pthread_t(threadHandle));
    end;

{$warning threadhandle can be larger than a dword}
    function  BeWaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;  {0=no timeout}
    var
      LResultP: Pointer;
      LResult: DWord;
    begin
      LResult := 0;
      LResultP := @LResult;
      pthread_join(pthread_t(threadHandle), @LResultP);
      CWaitForThreadTerminate := LResult;
    end;

{$warning threadhandle can be larger than a dword}
    function  BeThreadSetPriority (threadHandle : dword; Prio: longint): boolean; {-15..+15, 0=normal}
    begin
      {$Warning ThreadSetPriority needs to be implemented}
    end;


{$warning threadhandle can be larger than a dword}
    function  BeThreadGetPriority (threadHandle : dword): Integer;
    begin
      {$Warning ThreadGetPriority needs to be implemented}
    end;

{$warning threadhandle can be larger than a dword}
    function  BeGetCurrentThreadId : dword;
    begin
      CGetCurrentThreadId:=dword(pthread_self());
    end;

    procedure BeSetThreadDebugNameA(threadHandle: TThreadID; const ThreadName: AnsiString);
    begin
      {$Warning SetThreadDebugName needs to be implemented}
    end;

    procedure BeSetThreadDebugNameU(threadHandle: TThreadID; const ThreadName: UnicodeString);
    begin
      {$Warning SetThreadDebugName needs to be implemented}
    end;

{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

    procedure BeInitCriticalSection(var CS);

    var
      MAttr : pthread_mutexattr_t;
      res: longint;
    begin
      res:=pthread_mutexattr_init(@MAttr);
      if res=0 then
        begin
          res:=pthread_mutexattr_settype(@MAttr,longint(_PTHREAD_MUTEX_RECURSIVE));
          if res=0 then
            res := pthread_mutex_init(@CS,@MAttr)
          else
            { No recursive mutex support :/ }
            res := pthread_mutex_init(@CS,NIL);
        end
      else 
        res:= pthread_mutex_init(@CS,NIL);
      pthread_mutexattr_destroy(@MAttr);
      if res <> 0 then
        runerror(6);
    end;                           

    procedure BeEnterCriticalSection(var CS);
      begin
         if pthread_mutex_lock(@CS) <> 0 then
           runerror(6);
      end;

    procedure BeLeaveCriticalSection(var CS);
      begin
         if pthread_mutex_unlock(@CS) <> 0 then
           runerror(6)
      end;

    procedure BeDoneCriticalSection(var CS);
      begin
         if pthread_mutex_destroy(@CS) <> 0 then
           runerror(6);
      end;


{*****************************************************************************
                           Heap Mutex Protection
*****************************************************************************}

    var
      HeapMutex : pthread_mutex_t;

    procedure BeThreadHeapMutexInit;
      begin
         pthread_mutex_init(@heapmutex,nil);
      end;

    procedure BeThreadHeapMutexDone;
      begin
         pthread_mutex_destroy(@heapmutex);
      end;

    procedure BeThreadHeapMutexLock;
      begin
         pthread_mutex_lock(@heapmutex);
      end;

    procedure BeThreadHeapMutexUnlock;
      begin
         pthread_mutex_unlock(@heapmutex);
      end;

    const
      BeThreadMemoryMutexManager : TMemoryMutexManager = (
        MutexInit : @BeThreadHeapMutexInit;
        MutexDone : @BeThreadHeapMutexDone;
        MutexLock : @BeThreadHeapMutexLock;
        MutexUnlock : @BeThreadHeapMutexUnlock;
      );

    procedure InitHeapMutexes;
      begin
        SetMemoryMutexManager(BeThreadMemoryMutexManager);
      end;

Function BeInitThreads : Boolean;

begin
{$ifdef DEBUG_MT}
  Writeln('Entering InitThreads.');
{$endif}  
{$ifndef dynpthreads}
  Result:=True;
{$else}
  Result:=LoadPthreads;
{$endif}
  ThreadID := SizeUInt (pthread_self);
{$ifdef DEBUG_MT}
  Writeln('InitThreads : ',Result);
{$endif DEBUG_MT}
end;

Function BeDoneThreads : Boolean;

begin
{$ifndef dynpthreads}
  Result:=True;
{$else}
  Result:=UnloadPthreads;
{$endif}
end;

type
     TPthreadMutex = pthread_mutex_t;
     Tbasiceventstate=record
         FSem: Pointer;
         FManualReset: Boolean;
         FEventSection: TPthreadMutex;
	end;
     plocaleventstate = ^tbasiceventstate;  
//     peventstate=pointer;

Const 
	wrSignaled = 0;
	wrTimeout  = 1;
	wrAbandoned= 2;
	wrError	   = 3;

function IntBasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;

var
  MAttr : pthread_mutexattr_t;
  res   : cint;


begin
  new(plocaleventstate(result));
  plocaleventstate(result)^.FManualReset:=AManualReset;
  plocaleventstate(result)^.FSem:=New(PSemaphore);  //sem_t.
//  plocaleventstate(result)^.feventsection:=nil;
  res:=pthread_mutexattr_init(@MAttr);
  if res=0 then
    begin
      res:=pthread_mutexattr_settype(@MAttr,longint(_PTHREAD_MUTEX_RECURSIVE));
      if Res=0 then
        Res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,@MAttr)
      else
        res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,nil);
    end
  else
    res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,nil);
  pthread_mutexattr_destroy(@MAttr);
  if res <> 0 then
    runerror(6);
  if sem_init(psem_t(plocaleventstate(result)^.FSem),ord(False),Ord(InitialState)) <> 0 then
    runerror(6);
end;

procedure Intbasiceventdestroy(state:peventstate);

begin
  sem_destroy(psem_t(  plocaleventstate(state)^.FSem));
end;

procedure IntbasiceventResetEvent(state:peventstate);

begin
  While sem_trywait(psem_t( plocaleventstate(state)^.FSem))=0 do
    ;
end;

procedure IntbasiceventSetEvent(state:peventstate);

Var
  Value : Longint;

begin
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  Try
    sem_getvalue(plocaleventstate(state)^.FSem,@value);
    if Value=0 then
      sem_post(psem_t( plocaleventstate(state)^.FSem));
  finally
    pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
  end;
end;

function IntbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;

begin
  If TimeOut<>Cardinal($FFFFFFFF) then
    result:=wrError
  else
    begin
      sem_wait(psem_t(plocaleventstate(state)^.FSem));
      result:=wrSignaled;
      if plocaleventstate(state)^.FManualReset then
        begin
          pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
          Try
              intbasiceventresetevent(State);
              sem_post(psem_t( plocaleventstate(state)^.FSem));
            Finally
          pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
        end;
      end;
    end;
end;

Var
  BeThreadManager : TThreadManager;

Procedure SetBeThreadManager;

begin
  With BeThreadManager do
    begin
    InitManager            :=@BeInitThreads;
    DoneManager            :=@BeDoneThreads;
    BeginThread            :=@BeBeginThread;
    EndThread              :=@BeEndThread;
    SuspendThread          :=@BeSuspendThread;
    ResumeThread           :=@BeResumeThread;
    KillThread             :=@BeKillThread;
    ThreadSwitch           :=@BeThreadSwitch;
    WaitForThreadTerminate :=@BeWaitForThreadTerminate;
    ThreadSetPriority      :=@BeThreadSetPriority;
    ThreadGetPriority      :=@BeThreadGetPriority;
    GetCurrentThreadId     :=@BeGetCurrentThreadId;
    SetThreadDebugNameA    :=@BeSetThreadDebugNameA;
    SetThreadDebugNameU    :=@BeSetThreadDebugNameU;
    InitCriticalSection    :=@BeInitCriticalSection;
    DoneCriticalSection    :=@BeDoneCriticalSection;
    EnterCriticalSection   :=@BeEnterCriticalSection;
    LeaveCriticalSection   :=@BeLeaveCriticalSection;
{$ifdef hasthreadvar}
    InitThreadVar          :=@BeInitThreadVar;
    RelocateThreadVar      :=@BeRelocateThreadVar;
    AllocateThreadVars     :=@BeAllocateThreadVars;
    ReleaseThreadVars      :=@BeReleaseThreadVars;
{$endif}
    BasicEventCreate       :=@intBasicEventCreate;       
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    end;
  SetThreadManager(BeThreadManager);
  InitHeapMutexes;
end;

initialization
  SetBeThreadManager;
end.
