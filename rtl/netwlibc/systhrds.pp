{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    netware (pthreads) threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

unit systhrds;
interface
{$S-}

//Procedure SetCThreadManager;

{ Posix compliant definition }

uses Libc;

type
  PRTLCriticalSection = Ppthread_mutex_t;
  TRTLCriticalSection = pthread_mutex_t;


{$i threadh.inc}

implementation


{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{$i thread.inc}

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}
    const
      threadvarblocksize : dword = 0;

    var
      TLSKey : pthread_key_t;
      ThVarAllocResourceTag : rtag_t;

    procedure SysInitThreadvar(var offset : dword;size : dword);
      begin
        offset:=threadvarblocksize;
        inc(threadvarblocksize,size);
      end;

    function SysRelocateThreadvar(offset : dword) : pointer;
      begin
        SysRelocateThreadvar:=pthread_getspecific(tlskey)+Offset;
      end;


    procedure SysAllocateThreadVars;
      var
        dataindex : pointer;
      begin
        { we've to allocate the memory from system  }
        { because the FPC heap management uses      }
        { exceptions which use threadvars but       }
        { these aren't allocated yet ...            }
        { allocate room on the heap for the thread vars }
        DataIndex:=_Alloc(threadvarblocksize,ThVarAllocResourceTag);
        //DataIndex:=Pointer(Fpmmap(nil,threadvarblocksize,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0));
        FillChar(DataIndex^,threadvarblocksize,0);
        pthread_setspecific(tlskey,dataindex);
      end;


    procedure SysReleaseThreadVars;
      begin
        _Free (pthread_getspecific(tlskey));
      end;

{ Include OS independent Threadvar initialization }
{$i threadvr.inc}


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
        SysReleaseThreadVars;
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
        SysAllocateThreadVars;
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
	pthread_detach(pointer(pthread_self));
      end;


    function SysBeginThread(sa : Pointer;stacksize : dword;
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
           InitThreadVars(@SysRelocateThreadvar);
{$endif HASTHREADVAR}
           IsMultiThread:=true;
         end;
        { the only way to pass data to the newly created thread
          in a MT safe way, is to use the heap }
        getmem(ti,sizeof(pthreadinfo));
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
        SysBeginThread:=threadid;
{$ifdef DEBUG_MT}
        writeln('BeginThread returning ',SysBeginThread);
{$endif DEBUG_MT}
      end;


    procedure SysEndThread(ExitCode : DWord);
      begin
        DoneThread;
        pthread_detach(pointer(pthread_self));
        pthread_exit(pointer(ExitCode));
      end;


    function  SysSuspendThread (threadHandle : dword) : dword;
    begin
      {$Warning SuspendThread needs to be implemented}
    end;

    function  SysResumeThread  (threadHandle : dword) : dword;
    begin
      {$Warning ResumeThread needs to be implemented}
    end;

    procedure SysThreadSwitch;  {give time to other threads}
    begin
      {extern int pthread_yield (void) __THROW;}
      {$Warning ThreadSwitch needs to be implemented}
    end;

    function  SysKillThread (threadHandle : dword) : dword;
    begin
      pthread_detach(pointer(threadHandle));
      SysKillThread := pthread_cancel(Pointer(threadHandle));
    end;

    function  SysWaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;  {0=no timeout}
    var
      LResultP: Pointer;
      LResult: DWord;
    begin
      LResult := 0;
      LResultP := @LResult;
      pthread_join(Pointer(threadHandle), @LResultP);
      SysWaitForThreadTerminate := LResult;
    end;

    function  SysThreadSetPriority (threadHandle : dword; Prio: longint): boolean; {-15..+15, 0=normal}
    begin
      {$Warning ThreadSetPriority needs to be implemented}
    end;


    function  SysThreadGetPriority (threadHandle : dword): Integer;
    begin
      {$Warning ThreadGetPriority needs to be implemented}
    end;

    function  SysGetCurrentThreadId : dword;
    begin
      SysGetCurrentThreadId:=dword(pthread_self);
    end;


{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

    procedure SysInitCriticalSection(var CS);

    Var
      P : PRTLCriticalSection;

      begin
         P:=PRTLCriticalSection(@CS);
         FillChar (p^,sizeof(p^),0);
         pthread_mutex_init(P,NIL);
      end;

    procedure SysEnterCriticalSection(var CS);
      begin
         pthread_mutex_lock(PRTLCriticalSection(@CS));
      end;

    procedure SysLeaveCriticalSection(var CS);
      begin
         pthread_mutex_unlock(PRTLCriticalSection(@CS));
      end;

    procedure SysDoneCriticalSection(var CS);
      begin
         pthread_mutex_destroy(PRTLCriticalSection(@CS));
      end;


{*****************************************************************************
                           Heap Mutex Protection
*****************************************************************************}

    var
      HeapMutex : pthread_mutex_t;

    procedure PThreadHeapMutexInit;
      begin
         pthread_mutex_init(@heapmutex,nil);
      end;

    procedure PThreadHeapMutexDone;
      begin
         pthread_mutex_destroy(@heapmutex);
      end;

    procedure PThreadHeapMutexLock;
      begin
         pthread_mutex_lock(@heapmutex);
      end;

    procedure PThreadHeapMutexUnlock;
      begin
         pthread_mutex_unlock(@heapmutex);
      end;

    const
      PThreadMemoryMutexManager : TMemoryMutexManager = (
        MutexInit : @PThreadHeapMutexInit;
        MutexDone : @PThreadHeapMutexDone;
        MutexLock : @PThreadHeapMutexLock;
        MutexUnlock : @PThreadHeapMutexUnlock;
      );

    procedure InitHeapMutexes;
      begin
        SetMemoryMutexManager(PThreadMemoryMutexManager);
      end;

type
     TPthreadMutex = ppthread_mutex_t;
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
  MAttr : pthread_mutex_attr_t;
  res   : cint;


begin
  //new(plocaleventstate(result));
  getmem (result,sizeof(plocaleventstate));
  plocaleventstate(result)^.FManualReset:=AManualReset;
  plocaleventstate(result)^.FSem:=New(PSemaphore);  //sem_t.
//  plocaleventstate(result)^.feventsection:=nil;
  res:=pthread_mutexattr_init(@MAttr);
  if Res=0 then
    try
      Res:=pthread_mutexattr_settype(@MAttr,longint(PTHREAD_MUTEX_RECURSIVE));
      if Res=0 then
        Res:=pthread_mutex_init(@plocaleventstate(result)^.feventsection,@MAttr);
    finally
      pthread_mutexattr_destroy(@MAttr);
    end;
  sem_init(psem_t(plocaleventstate(result)^.FSem),ord(False),Ord(InitialState));
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
  NWThreadManager : TThreadManager;

Procedure SetNWThreadManager;

begin
  With NWThreadManager do
  begin
    InitManager            :=nil;
    DoneManager            :=nil;
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
{$ifdef hasthreadvar}
    InitThreadVar          :=@SysInitThreadVar;
    RelocateThreadVar      :=@SysRelocateThreadVar;
    AllocateThreadVars     :=@SysAllocateThreadVars;
    ReleaseThreadVars      :=@SysReleaseThreadVars;
{$endif}
    BasicEventCreate       :=@intBasicEventCreate;
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    end;
  SetThreadManager(NWThreadManager);
  InitHeapMutexes;
end;

initialization
  {$ifdef HASTHREADVAR}
  ThVarAllocResourceTag := AllocateResourceTag(getnlmhandle,'Threadvar Memory',AllocSignature);
  {$endif}
  SetNWThreadManager;
end.
{
  $Log$
  Revision 1.1  2004-09-05 20:58:47  armin
  * first rtl version for netwlibc

}

