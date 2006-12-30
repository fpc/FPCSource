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
{$mode objfpc}
{$ifdef linux}
{$define dynpthreads} // Useless on BSD, since they are in libc
{$endif}


{ sem_init is best, since it does not consume any file descriptors.    }
{ sem_open is second best, since it consumes only one file descriptor  }
{ per semaphore.                                                       }
{ If neither is available, pipe is used as fallback, which consumes 2  }
{ file descriptors per semaphore.                                      }

{ Darwin doesn't support nameless semaphores in at least }
{ Mac OS X 10.4.8/Darwin 8.8                             }
{$ifndef darwin}
{$define has_sem_init}
{$define has_sem_getvalue}
{$else }
{$ifdef darwin}
{$define has_sem_open}
{$endif}
{$endif}

unit cthreads;
interface
{$S-}

{$ifndef dynpthreads}   // If you have problems compiling this on FreeBSD 5.x
 {$linklib c}           // try adding -Xf
 {$ifndef Darwin}
   {$linklib pthread}
 {$endif darwin}
{$endif}

Procedure SetCThreadManager;

implementation

Uses
  BaseUnix,
  unix,
  unixtype
{$ifdef dynpthreads}
  ,dl
{$endif}
  ;

{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include OS specific parts. }
{$i pthread.inc}

Type  PINTRTLEvent = ^TINTRTLEvent;
      TINTRTLEvent = record
        condvar: pthread_cond_t;
        mutex: pthread_mutex_t;
        IsSet: boolean;
       end;

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

    const
      threadvarblocksize : dword = 0;


    var
      TLSKey : pthread_key_t;

    procedure CInitThreadvar(var offset : dword;size : dword);
      begin
        {$ifdef cpusparc}
        threadvarblocksize:=align(threadvarblocksize,16);
        {$endif cpusparc}

        {$ifdef cpupowerpc}
        threadvarblocksize:=align(threadvarblocksize,8);
        {$endif cpupowerc}

        {$ifdef cpui386}
        threadvarblocksize:=align(threadvarblocksize,8);
        {$endif cpui386}

        {$ifdef cpuarm}
        threadvarblocksize:=align(threadvarblocksize,4);
        {$endif cpuarm}

        {$ifdef cpum68k}
        threadvarblocksize:=align(threadvarblocksize,2);
        {$endif cpum68k}

        {$ifdef cpux86_64}
        threadvarblocksize:=align(threadvarblocksize,16);
        {$endif cpux86_64}

        {$ifdef cpupowerpc64}
        threadvarblocksize:=align(threadvarblocksize,16);
        {$endif cpupowerpc64}

        offset:=threadvarblocksize;

        inc(threadvarblocksize,size);
      end;



    procedure CAllocateThreadVars;
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

    function CRelocateThreadvar(offset : dword) : pointer;

    var
      P : Pointer;

      begin
        P:=pthread_getspecific(tlskey);
        if (P=Nil) then
          begin
          CAllocateThreadvars;
          // If this also goes wrong: bye bye threadvars...
          P:=pthread_getspecific(tlskey);
          end;
        CRelocateThreadvar:=P+Offset;
      end;

    procedure CReleaseThreadVars;
      begin
        Fpmunmap(pointer(pthread_getspecific(tlskey)),threadvarblocksize);
      end;

{ Include OS independent Threadvar initialization }



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
        CReleaseThreadVars;
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
        { Allocate local thread vars, this must be the first thing,
          because the exception management and io depends on threadvars }
        CAllocateThreadVars;
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
        pthread_exit(nil);
      end;


  function CBeginThread(sa : Pointer;stacksize : PtrUInt;
                       ThreadFunction : tthreadfunc;p : pointer;
                       creationFlags : dword; var ThreadId : TThreadId) : TThreadID;
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
          if (InterLockedExchange(longint(IsMultiThread),ord(true)) = 0) then
            begin
              { We're still running in single thread mode, setup the TLS }
              pthread_key_create(@TLSKey,nil);
              InitThreadVars(@CRelocateThreadvar);
            end
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
      if pthread_create(ppthread_t(@threadid), @thread_attr, @ThreadMain,ti) <> 0 then
        begin
          dispose(ti);
          threadid := TThreadID(0);
        end;
      CBeginThread:=threadid;
{$ifdef DEBUG_MT}
      writeln('BeginThread returning ',ptrint(CBeginThread));
{$endif DEBUG_MT}
    end;


  procedure CEndThread(ExitCode : DWord);
    begin
      DoneThread;
      pthread_detach(pthread_t(pthread_self()));
      pthread_exit(pointer(ptrint(ExitCode)));
    end;



  function  CSuspendThread (threadHandle : TThreadID) : dword;
    begin
      result := pthread_kill(threadHandle,SIGSTOP);
    end;


  function  CResumeThread  (threadHandle : TThreadID) : dword;
    begin
      result := pthread_kill(threadHandle,SIGCONT);
    end;


  procedure CThreadSwitch;  {give time to other threads}
    begin
      {extern int pthread_yield (void) __THROW;}
      {$Warning ThreadSwitch needs to be implemented}
    end;


  function  CKillThread (threadHandle : TThreadID) : dword;
    begin
      pthread_detach(pthread_t(threadHandle));
      CKillThread := pthread_cancel(pthread_t(threadHandle));
    end;


  function  CWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
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
    function  CThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean; {-15..+15, 0=normal}
    begin
      {$Warning ThreadSetPriority needs to be implemented}
    end;


{$warning threadhandle can be larger than a dword}
  function  CThreadGetPriority (threadHandle : TThreadID): Integer;
    begin
      {$Warning ThreadGetPriority needs to be implemented}
    end;


  function  CGetCurrentThreadId : TThreadID;
    begin
      CGetCurrentThreadId := TThreadID (pthread_self());
    end;


{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

    procedure CInitCriticalSection(var CS);

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

    procedure CEnterCriticalSection(var CS);
      begin
         if pthread_mutex_lock(@CS) <> 0 then
           runerror(6);
      end;

    procedure CLeaveCriticalSection(var CS);
      begin
         if pthread_mutex_unlock(@CS) <> 0 then
           runerror(6)
      end;

    procedure CDoneCriticalSection(var CS);
      begin
         { unlock as long as unlocking works to unlock it if it is recursive
           some Delphi code might call this function with a locked mutex      }
         while pthread_mutex_unlock(@CS)=0 do
           ;

         if pthread_mutex_destroy(@CS) <> 0 then
           runerror(6);
      end;


{*****************************************************************************
                           Semaphore routines
*****************************************************************************}
  

procedure cSemaphoreWait(const FSem: Pointer);
var
  res: cint;
  err: cint;
{$if not defined(has_sem_init) and not defined(has_sem_open)}
  b: byte;
{$endif}
begin
{$if defined(has_sem_init) or defined(has_sem_open)}
  repeat
    res:=sem_wait(PSemaphore(FSem));
    err:=fpgeterrno;
  until (res<>-1) or (err<>ESysEINTR);
{$else}
  repeat
    res:=fpread(PFilDes(FSem)^[0], b, 1);
    err:=fpgeterrno;
  until (res<>-1) or ((err<>ESysEINTR) and (err<>ESysEAgain));
{$endif}
end;

procedure cSemaphorePost(const FSem: Pointer);
{$if defined(has_sem_init) or defined(has_sem_open)}
begin
  sem_post(PSemaphore(FSem));
end;
{$else}
var
  writeres: cint;
  err: cint;
  b : byte;
begin
  b:=0;
  repeat
    writeres:=fpwrite(PFilDes(FSem)^[1], b, 1);
    err:=fpgeterrno;
  until (writeres<>-1) or ((err<>ESysEINTR) and (err<>ESysEAgain));
end;
{$endif}


{$if defined(has_sem_open) and not defined(has_sem_init)}
function cIntSemaphoreOpen(const name: pchar; initvalue: boolean): Pointer;
var
  err: cint;
begin
  repeat
    cIntSemaphoreOpen := sem_open(name,O_CREAT,0,ord(initvalue));
    err:=fpgeterrno;
  until (ptrint(cIntSemaphoreOpen) <> SEM_FAILED) or (err <> ESysEINTR);
  if (ptrint(cIntSemaphoreOpen) <> SEM_FAILED) then
    { immediately unlink so the semaphore will be destroyed when the }
    { the process exits                                              }
    sem_unlink(name)
  else
    cIntSemaphoreOpen:=NIL;
end;
{$endif}


function cIntSemaphoreInit(initvalue: boolean): Pointer;
{$if defined(has_sem_open) and not defined(has_sem_init)}
var
  tid: string[31];
  semname: string[63];
  err: cint;
{$endif}
begin
{$ifdef has_sem_init}
  cIntSemaphoreInit := GetMem(SizeOf(TSemaphore));
  if sem_init(PSemaphore(cIntSemaphoreInit), 0, ord(initvalue)) <> 0 then
    begin
      FreeMem(cIntSemaphoreInit);
      cIntSemaphoreInit:=NIL;
    end;
{$else}
{$ifdef has_sem_open}
  { avoid a potential temporary nameclash with another process/thread }
  str(fpGetPid,semname);
  str(ptruint(pthread_self),tid);
  semname:='/FPC'+semname+'T'+tid+#0;
  cIntSemaphoreInit:=cIntSemaphoreOpen(@semname[1],initvalue);
{$else}
  cIntSemaphoreInit := GetMem(SizeOf(TFilDes));
  if (fppipe(PFilDes(cIntSemaphoreInit)^) <> 0) then
    begin
      FreeMem(cIntSemaphoreInit);
      cIntSemaphoreInit:=nil;
    end
  else if initvalue then
    cSemaphorePost(cIntSemaphoreInit);
{$endif}
{$endif}
end;


function cSemaphoreInit: Pointer;
begin
  cSemaphoreInit:=cIntSemaphoreInit(false);
end;


procedure cSemaphoreDestroy(const FSem: Pointer);
begin
{$ifdef has_sem_init}
  sem_destroy(PSemaphore(FSem));
  FreeMem(FSem);
{$else}
{$ifdef has_sem_open}
  sem_close(PSemaphore(FSem));
{$else has_sem_init}
  fpclose(PFilDes(FSem)^[0]);
  fpclose(PFilDes(FSem)^[1]);
  FreeMem(FSem);
{$endif}
{$endif}
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
     TPthreadMutex = pthread_mutex_t;
     Tbasiceventstate=record
         FSem: Pointer;
         FEventSection: TPthreadMutex;
         FManualReset: Boolean;
        end;
     plocaleventstate = ^tbasiceventstate;
//     peventstate=pointer;

Const
        wrSignaled = 0;
        wrTimeout  = 1;
        wrAbandoned= 2;
        wrError    = 3;

function IntBasicEventCreate(EventAttributes : Pointer; AManualReset,InitialState : Boolean;const Name : ansistring):pEventState;

var
  MAttr : pthread_mutexattr_t;
  res   : cint;
begin
  new(plocaleventstate(result));
  plocaleventstate(result)^.FManualReset:=AManualReset;
{$ifdef has_sem_init}
  plocaleventstate(result)^.FSem:=cIntSemaphoreInit(true);
  if plocaleventstate(result)^.FSem=nil then
    begin
      FreeMem(result);
      runerror(6);
    end;
{$else}
{$ifdef has_sem_open}
  plocaleventstate(result)^.FSem:=cIntSemaphoreOpen(PChar(Name),InitialState);
  if (plocaleventstate(result)^.FSem = NIL) then
    begin
      FreeMem(result);
      runerror(6);
    end;
{$else}
  plocaleventstate(result)^.FSem:=cSemaphoreInit;
  if (plocaleventstate(result)^.FSem = NIL) then
    begin
      FreeMem(result);
      runerror(6);
    end;
  if InitialState then
    cSemaphorePost(plocaleventstate(result)^.FSem);
{$endif}
{$endif}
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
    begin
      cSemaphoreDestroy(plocaleventstate(result)^.FSem);
      FreeMem(result);
      runerror(6);
    end;
end;

procedure Intbasiceventdestroy(state:peventstate);

begin
  cSemaphoreDestroy(plocaleventstate(state)^.FSem);
  FreeMem(state);
end;

procedure IntbasiceventResetEvent(state:peventstate);

{$if defined(has_sem_init) or defined(has_sem_open)}
var
  res: cint;
  err: cint;
begin
  repeat
    res:=sem_trywait(psem_t(plocaleventstate(state)^.FSem));
    err:=fpgeterrno;
  until (res<>0) and ((res<>-1) or (err<>ESysEINTR));
{$else has_sem_init or has_sem_open}
var
  fds: TFDSet;
  tv : timeval;
begin
  tv.tv_sec:=0;
  tv.tv_usec:=0;
  fpFD_ZERO(fds);
  fpFD_SET(PFilDes(plocaleventstate(state)^.FSem)^[0],fds);
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  Try
    while fpselect(PFilDes(plocaleventstate(state)^.FSem)^[0],@fds,nil,nil,@tv) > 0 do
      cSemaphoreWait(plocaleventstate(state)^.FSem);
  finally
    pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
  end;
{$endif has_sem_init or has_sem_open}
end;

procedure IntbasiceventSetEvent(state:peventstate);

Var
{$if defined(has_sem_init) or defined(has_sem_open)}
  Value : Longint;
  res : cint;
  err : cint;
{$else}
  fds: TFDSet;
  tv : timeval;
{$endif}
begin
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  Try
{$if defined(has_sem_init) or defined(has_sem_open)}
    if (sem_getvalue(plocaleventstate(state)^.FSem,@value) <> -1) then
      begin
        if Value=0 then
          cSemaphorePost(plocaleventstate(state)^.FSem);
      end
    else if (fpgeterrno = ESysENOSYS) then
      { not yet implemented on Mac OS X 10.4.8 }
      begin
        repeat
          res:=sem_trywait(psem_t(plocaleventstate(state)^.FSem));
          err:=fpgeterrno;
        until ((res<>-1) or (err<>ESysEINTR));
        { now we've either decreased the semaphore by 1 (if it was  }
        { not zero), or we've done nothing (if it was already zero) }
        { -> increase by 1 and we have the same result as           }
        { increasing by 1 only if it was 0                          }
        cSemaphorePost(plocaleventstate(state)^.FSem);
      end
    else
      runerror(6);
{$else has_sem_init or has_sem_open}
    tv.tv_sec:=0;
    tv.tv_usec:=0;
    fpFD_ZERO(fds);
    fpFD_SET(PFilDes(plocaleventstate(state)^.FSem)^[0],fds);
    if fpselect(PFilDes(plocaleventstate(state)^.FSem)^[0],@fds,nil,nil,@tv)=0 then
      cSemaphorePost(plocaleventstate(state)^.FSem);
{$endif has_sem_init or has_sem_open}
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
      cSemaphoreWait(plocaleventstate(state)^.FSem);
      result:=wrSignaled;
      if plocaleventstate(state)^.FManualReset then
        begin
          pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
          Try
            intbasiceventresetevent(State);
            cSemaphorePost(plocaleventstate(state)^.FSem);
          Finally
            pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
        end;
      end;
    end;
end;

function intRTLEventCreate: PRTLEvent;

var p:pintrtlevent;

begin
  new(p);
  pthread_cond_init(@p^.condvar, nil);
  pthread_mutex_init(@p^.mutex, nil);
  p^.IsSet:= false;
  result:=PRTLEVENT(p);
end;

procedure intRTLEventDestroy(AEvent: PRTLEvent);

var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_cond_destroy(@p^.condvar);
  pthread_mutex_destroy(@p^.mutex);
  dispose(p);
end;

procedure intRTLEventSetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  p^.IsSet:= true;
  pthread_cond_signal(@p^.condvar);
  pthread_mutex_unlock(@p^.mutex);
end;


procedure intRTLEventResetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  p^.IsSet:= false;
  pthread_mutex_unlock(@p^.mutex);
end;


procedure intRTLEventStartWait(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
end;

procedure intRTLEventWaitFor(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  while not p^.IsSet do pthread_cond_wait(@p^.condvar, @p^.mutex);
  p^.IsSet:=false;
  pthread_mutex_unlock(@p^.mutex);
end;

procedure intRTLEventWaitForTimeout(AEvent: PRTLEvent;timeout : longint);
  var
    p : pintrtlevent;
    errres : cint;
    timespec : ttimespec;
    tnow : timeval;

  begin
    p:=pintrtlevent(aevent);
    fpgettimeofday(@tnow,nil);
    timespec.tv_sec:=tnow.tv_sec+(timeout div 1000);
    timespec.tv_nsec:=(timeout mod 1000)*1000000 + tnow.tv_usec*1000;
    if timespec.tv_nsec >= 1000000000 then
    begin
      inc(timespec.tv_sec);
      dec(timespec.tv_nsec, 1000000000);
    end;
    errres:=0;
    while (not p^.IsSet) and
          (errres <> ESysETIMEDOUT) do
      begin
        errres:=pthread_cond_timedwait(@p^.condvar, @p^.mutex, @timespec);
      end;
    p^.IsSet:= false;
    pthread_mutex_unlock(@p^.mutex);
  end;


type
  threadmethod = procedure of object;


Function CInitThreads : Boolean;

begin
{$ifdef DEBUG_MT}
  Writeln('Entering InitThreads.');
{$endif}
{$ifndef dynpthreads}
  Result:=True;
{$else}
  Result:=LoadPthreads;
{$endif}
  ThreadID := TThreadID (pthread_self);
{$ifdef DEBUG_MT}
  Writeln('InitThreads : ',Result);
{$endif DEBUG_MT}
end;

Function CDoneThreads : Boolean;

begin
{$ifndef dynpthreads}
  Result:=True;
{$else}
  Result:=UnloadPthreads;
{$endif}
end;


Var
  CThreadManager : TThreadManager;

Procedure SetCThreadManager;

begin
  With CThreadManager do begin
    InitManager            :=@CInitThreads;
    DoneManager            :=@CDoneThreads;
    BeginThread            :=@CBeginThread;
    EndThread              :=@CEndThread;
    SuspendThread          :=@CSuspendThread;
    ResumeThread           :=@CResumeThread;
    KillThread             :=@CKillThread;
    ThreadSwitch           :=@CThreadSwitch;
    WaitForThreadTerminate :=@CWaitForThreadTerminate;
    ThreadSetPriority      :=@CThreadSetPriority;
    ThreadGetPriority      :=@CThreadGetPriority;
    GetCurrentThreadId     :=@CGetCurrentThreadId;
    InitCriticalSection    :=@CInitCriticalSection;
    DoneCriticalSection    :=@CDoneCriticalSection;
    EnterCriticalSection   :=@CEnterCriticalSection;
    LeaveCriticalSection   :=@CLeaveCriticalSection;
    InitThreadVar          :=@CInitThreadVar;
    RelocateThreadVar      :=@CRelocateThreadVar;
    AllocateThreadVars     :=@CAllocateThreadVars;
    ReleaseThreadVars      :=@CReleaseThreadVars;
    BasicEventCreate       :=@intBasicEventCreate;
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    rtlEventCreate         :=@intrtlEventCreate;
    rtlEventDestroy        :=@intrtlEventDestroy;
    rtlEventSetEvent       :=@intrtlEventSetEvent;
    rtlEventResetEvent     :=@intrtlEventResetEvent;
    rtlEventStartWait      :=@intrtlEventStartWait;
    rtleventWaitForTimeout :=@intrtleventWaitForTimeout;
    rtleventWaitFor        :=@intrtleventWaitFor;
    // semaphores
    SemaphoreInit          :=@cSemaphoreInit;
    SemaphoreDestroy       :=@cSemaphoreDestroy;
    SemaphoreWait          :=@cSemaphoreWait;
    SemaphorePost          :=@cSemaphorePost;
  end;
  SetThreadManager(CThreadManager);
  InitHeapMutexes;
end;


initialization
  if ThreadingAlreadyUsed then
    begin
      writeln('Threading has been used before cthreads was initialized.');
      writeln('Make cthreads one of the first units in your uses clause.');
      runerror(211);
    end;
  SetCThreadManager;
finalization
end.
