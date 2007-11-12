{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    pthreads threading support implementation

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

{$ifdef linux}
{$define has_sem_timedwait}
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
  unixtype,
  initc
{$ifdef dynpthreads}
  ,dl
{$endif}
  ;

{*****************************************************************************
                             System unit import
*****************************************************************************}

procedure fpc_threaderror; [external name 'FPC_THREADERROR'];

{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include OS specific parts. }
{$i pthread.inc}

Type  PINTRTLEvent = ^TINTRTLEvent;
      TINTRTLEvent = record
        condvar: pthread_cond_t;
        mutex: pthread_mutex_t;
        isset: boolean;
       end;

      TTryWaitResult = (tw_error, tw_semwasunlocked, tw_semwaslocked);

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

    function ThreadMain(param : pointer) : pointer;cdecl;
      var
        ti : tthreadinfo;
        nset: tsigset;
{$if defined(linux) and not defined(FPC_USE_LIBC)}
        nlibcset: tlibc_sigset;
{$endif linux/no FPC_USE_LIBC}
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
        { unblock all signals we are interested in (may be blocked by }
        { default in new threads on some OSes, see #9073)             }
        fpsigemptyset(nset);
        fpsigaddset(nset,SIGSEGV);
        fpsigaddset(nset,SIGBUS);
        fpsigaddset(nset,SIGFPE);
        fpsigaddset(nset,SIGILL);
{$if defined(linux) and not defined(FPC_USE_LIBC)}
        { sigset_t has a different size for linux/kernel and linux/libc }
        fillchar(nlibcset,sizeof(nlibcset),0);
        if (sizeof(nlibcset)>sizeof(nset)) then
          move(nset,nlibcset,sizeof(nset))
        else
          move(nset,nlibcset,sizeof(nlibcset));
        pthread_sigmask(SIG_UNBLOCK,@nlibcset,nil);
{$else linux}
        pthread_sigmask(SIG_UNBLOCK,@nset,nil);
{$endif linux}
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
        pthread_exit(ThreadMain);
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
    {  pthread_kill(SIGSTOP) cannot be used, because posix-compliant
       implementations then freeze the entire process instead of only
       the target thread. Suspending a particular thread is not
       supported by posix nor by most *nix implementations, presumably
       because of concerns mentioned in E.4 at
       http://pauillac.inria.fr/~xleroy/linuxthreads/faq.html#E and in
       http://java.sun.com/j2se/1.4.2/docs/guide/misc/threadPrimitiveDeprecation.html
    }
//      result := pthread_kill(threadHandle,SIGSTOP);
    end;


  function  CResumeThread  (threadHandle : TThreadID) : dword;
    begin
//      result := pthread_kill(threadHandle,SIGCONT);
    end;


  procedure sched_yield; cdecl; external 'c' name 'sched_yield';

  procedure CThreadSwitch;  {give time to other threads}
    begin
      { At least on Mac OS X, the pthread_yield_np calls through to this. }
      { Further, sched_yield is in POSIX and supported on FreeBSD 4+,     }
      { Linux, Mac OS X and Solaris, while the thread-specific yield      }
      { routines are called differently everywhere and non-standard.      }
      sched_yield;
    end;


  function  CKillThread (threadHandle : TThreadID) : dword;
    begin
      pthread_detach(pthread_t(threadHandle));
      CKillThread := pthread_cancel(pthread_t(threadHandle));
    end;


  function  CWaitForThreadTerminate (threadHandle : TThreadID; TimeoutMs : longint) : dword;  {0=no timeout}
    var
      LResultP: Pointer;
    begin
      pthread_join(pthread_t(threadHandle), @LResultP);
      CWaitForThreadTerminate := dword(LResultP);
    end;

    function  CThreadSetPriority (threadHandle : TThreadID; Prio: longint): boolean; {-15..+15, 0=normal}
    begin
      {$Warning ThreadSetPriority needs to be implemented}
    end;


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
        fpc_threaderror;
    end;

    procedure CEnterCriticalSection(var CS);
      begin
         if pthread_mutex_lock(@CS) <> 0 then
           fpc_threaderror
      end;

    procedure CLeaveCriticalSection(var CS);
      begin
         if pthread_mutex_unlock(@CS) <> 0 then
           fpc_threaderror
      end;

    procedure CDoneCriticalSection(var CS);
      begin
         { unlock as long as unlocking works to unlock it if it is recursive
           some Delphi code might call this function with a locked mutex      }
         while pthread_mutex_unlock(@CS)=0 do
           ;

         if pthread_mutex_destroy(@CS) <> 0 then
           fpc_threaderror;
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
    err:=fpgetCerrno;
  until (res<>-1) or (err<>ESysEINTR);
{$else}
  repeat
    res:=fpread(PFilDes(FSem)^[0], b, 1);
    err:=fpgeterrno;
  until (res<>-1) or ((err<>ESysEINTR) and (err<>ESysEAgain));
{$endif}
end;

{$if defined(has_sem_timedwait)}

function cSemaphoreTimedWait(const FSem: Pointer; const Timeout: ttimespec): cint;
var
  res: cint;
  err: cint;
begin
  repeat
    res:=sem_timedwait(PSemaphore(FSem), @Timeout);
    if res=0 then exit(0);
    err:=fpgetCerrno;
  until err<>ESysEINTR;
  result:=err;
end;

{$endif}

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


function cSemaphoreTryWait(const FSem: pointer): TTryWaitResult;
var
  res: cint;
  err: cint;
{$if defined(has_sem_init) or defined(has_sem_open)}
begin
  repeat
    res:=sem_trywait(FSem);
    err:=fpgetCerrno;
  until (res<>-1) or (err<>ESysEINTR);
  if (res=0) then
    result:=tw_semwasunlocked
  else if (err=ESysEAgain) then
    result:=tw_semwaslocked
  else
    result:=tw_error;
{$else has_sem_init or has_sem_open}
var
  fds: TFDSet;
  tv : timeval;
begin
  tv.tv_sec:=0;
  tv.tv_usec:=0;
  fpFD_ZERO(fds);
  fpFD_SET(PFilDes(FSem)^[0],fds);
  repeat
    res:=fpselect(PFilDes(FSem)^[0]+1,@fds,nil,nil,@tv);
    err:=fpgeterrno;
  until (res>=0) or ((res=-1) and (err<>ESysEIntr));
  if (res>0) then
    begin
      cSemaphoreWait(FSem);
      result:=tw_semwasunlocked
    end
  else if (res=0) then
    result:=tw_semwaslocked
  else
    result:=tw_error;
{$endif has_sem_init or has_sem_open}
end;




{$if defined(has_sem_open) and not defined(has_sem_init)}
function cIntSemaphoreOpen(const name: pchar; initvalue: boolean): Pointer;
var
  err: cint;
begin
  repeat
    cIntSemaphoreOpen := sem_open(name,O_CREAT,0,ord(initvalue));
    err:=fpgetCerrno;
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


type
     TPthreadMutex = pthread_mutex_t;
     Tbasiceventstate=record
         FSem: Pointer;
         FEventSection: TPthreadMutex;
         FWaiters: longint;
         FManualReset,
         FDestroying: Boolean;
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
  plocaleventstate(result)^.FWaiters:=0;
  plocaleventstate(result)^.FDestroying:=False;
{$ifdef has_sem_init}
  plocaleventstate(result)^.FSem:=cIntSemaphoreInit(initialstate);
  if plocaleventstate(result)^.FSem=nil then
    begin
      FreeMem(result);
      fpc_threaderror;
    end;
{$else}
{$ifdef has_sem_open}
  plocaleventstate(result)^.FSem:=cIntSemaphoreOpen(PChar(Name),InitialState);
  if (plocaleventstate(result)^.FSem = NIL) then
    begin
      FreeMem(result);
      fpc_threaderror;
    end;
{$else}
  plocaleventstate(result)^.FSem:=cSemaphoreInit;
  if (plocaleventstate(result)^.FSem = NIL) then
    begin
      FreeMem(result);
      fpc_threaderror;
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
      fpc_threaderror;
    end;
end;

procedure Intbasiceventdestroy(state:peventstate);
var
  i: longint;
begin
  { safely mark that we are destroying this event }
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  plocaleventstate(state)^.FDestroying:=true;
  { wake up everyone who is waiting }
  for i := 1 to plocaleventstate(state)^.FWaiters do
    cSemaphorePost(plocaleventstate(state)^.FSem);
  pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
  { now wait until they've finished their business }
  while (plocaleventstate(state)^.FWaiters <> 0) do
    cThreadSwitch;

  { and clean up }
  cSemaphoreDestroy(plocaleventstate(state)^.FSem);
  dispose(plocaleventstate(state));
end;


procedure IntbasiceventResetEvent(state:peventstate);

begin
{$if not defined(has_sem_init) and not defined(has_sem_open)}
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  try
{$endif}
    while (cSemaphoreTryWait(plocaleventstate(state)^.FSem) = tw_semwasunlocked) do
      ;
{$if not defined(has_sem_init) and not defined(has_sem_open)}
  finally
    pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
  end;
{$endif}
end;

procedure IntbasiceventSetEvent(state:peventstate);

Var
  res : cint;
  err : cint;
{$if defined(has_sem_init) or defined(has_sem_open)}
  Value : Longint;
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
    else if (fpgetCerrno = ESysENOSYS) then
      { not yet implemented on Mac OS X 10.4.8 }
      begin
        repeat
          res:=sem_trywait(psem_t(plocaleventstate(state)^.FSem));
          err:=fpgetCerrno;
        until ((res<>-1) or (err<>ESysEINTR));
        { now we've either decreased the semaphore by 1 (if it was  }
        { not zero), or we've done nothing (if it was already zero) }
        { -> increase by 1 and we have the same result as           }
        { increasing by 1 only if it was 0                          }
        cSemaphorePost(plocaleventstate(state)^.FSem);
      end
    else
      fpc_threaderror;
{$else has_sem_init or has_sem_open}
    tv.tv_sec:=0;
    tv.tv_usec:=0;
    fpFD_ZERO(fds);
    fpFD_SET(PFilDes(plocaleventstate(state)^.FSem)^[0],fds);
    repeat
      res:=fpselect(PFilDes(plocaleventstate(state)^.FSem)^[0]+1,@fds,nil,nil,@tv);
      err:=fpgeterrno;
    until (res>=0) or ((res=-1) and (err<>ESysEIntr));
    if (res=0) then
      cSemaphorePost(plocaleventstate(state)^.FSem);
{$endif has_sem_init or has_sem_open}
  finally
    pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
  end;
end;


function IntbasiceventWaitFor(Timeout : Cardinal;state:peventstate) : longint;
var
  i, loopcnt: cardinal;
  timespec, timetemp, timeleft: ttimespec;
  nanores, nanoerr: cint;
  twres: TTryWaitResult;
  lastloop: boolean;
begin
  { safely check whether we are being destroyed, if so immediately return. }
  { otherwise (under the same mutex) increase the number of waiters        }
  pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
  if (plocaleventstate(state)^.FDestroying) then
    begin
      pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
      result := wrAbandoned;
      exit;
    end;
  inc(plocaleventstate(state)^.FWaiters);
  pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);

  if TimeOut=Cardinal($FFFFFFFF) then
    begin
      { if no timeout, just wait until we are woken up }
      cSemaphoreWait(plocaleventstate(state)^.FSem);
      if not(plocaleventstate(state)^.FDestroying) then
        result:=wrSignaled
      else
        result:=wrAbandoned;
    end
  else
    begin
{$ifdef has_sem_timedwait}
      fpgettimeofday(@timespec,nil);
      inc(timespec.tv_nsec, (timeout mod 1000) * 1000000);
      inc(timespec.tv_sec, timeout div 1000);
      if timespec.tv_nsec > 1000000000 then
      begin
        dec(timespec.tv_nsec, 1000000000);
        inc(timespec.tv_sec);
      end;
      nanores := cSemaphoreTimedWait(plocaleventstate(state)^.FSem, timespec);
      if nanores = 0 then
        result := wrSignaled
      else if nanores = ESysETIMEDOUT then
        result := wrTimeout
      else
        result := wrError;
{$else}
      timespec.tv_sec:=0;
      { 50 miliseconds or less -> wait once for this duration }
      if (timeout <= 50) then
        loopcnt:=1
      { otherwise wake up every 50 msecs to check    }
      { (we'll wait a little longer in total because }
      {  we don't take into account the overhead)    }
      else
        begin
          loopcnt := timeout div 50;
          timespec.tv_nsec:=50*1000000;
        end;
      result := wrTimeOut;
      nanores := 0;

      for i := 1 to loopcnt do
        begin
          { in the last iteration, wait for the amount of time left }
          if (i = loopcnt) then
            timespec.tv_nsec:=(timeout mod 50) * 1000000;
          timetemp:=timespec;
          lastloop:=false;
          { every time our sleep is interrupted for whatever reason, }
          { also check whether the semaphore has been posted in the  }
          { mean time                                                }
          repeat
          {$if not defined(has_sem_init) and not defined(has_sem_open)}
            pthread_mutex_lock(@plocaleventstate(state)^.feventsection);
            try
          {$endif}
              twres := cSemaphoreTryWait(plocaleventstate(state)^.FSem);
          {$if not defined(has_sem_init) and not defined(has_sem_open)}
            finally
              pthread_mutex_unlock(@plocaleventstate(state)^.feventsection);
            end;
          {$endif}
            case twres of
              tw_error:
                begin
                  result := wrError;
                  break;
                end;
              tw_semwasunlocked:
                begin
                  result := wrSignaled;
                  break;
                end;
            end;
            if (lastloop) then
              break;
            nanores:=fpnanosleep(@timetemp,@timeleft);
            nanoerr:=fpgeterrno;
            timetemp:=timeleft;
            lastloop:=(i=loopcnt);
          { loop until 1) we slept complete interval (except if last for-loop }
          { in which case we try to lock once more); 2) an error occurred;    }
          { 3) we're being destroyed                                          }
          until ((nanores=0) and not lastloop) or ((nanores<>0) and (nanoerr<>ESysEINTR)) or plocaleventstate(state)^.FDestroying;
          { adjust result being destroyed or error (in this order, since   }
          { if we're being destroyed the "error" could be ESysEINTR, which }
          { is not a real error                                            }
          if plocaleventstate(state)^.FDestroying then
            result := wrAbandoned
          else if (nanores <> 0) then
            result := wrError;
          { break out of greater loop when we got the lock, when an error }
          { occurred, or when we are being destroyed                      }
          if (result<>wrTimeOut) then
            break;
        end;
{$endif}
    end;
  
  if (result=wrSignaled) then
    begin
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
  { don't put this above the previous if-block, because otherwise   }
  { we can get errors in case an object is destroyed between the    }
  { end of the wait/sleep loop and the signalling above.            }
  { The pthread_mutex_unlock above takes care of the memory barrier }
  interlockeddecrement(plocaleventstate(state)^.FWaiters);
end;

function intRTLEventCreate: PRTLEvent;

var p:pintrtlevent;

begin
  new(p);
  pthread_cond_init(@p^.condvar, nil);
  pthread_mutex_init(@p^.mutex, nil);
  p^.isset:=false;
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
  p^.isset:=true;
  pthread_cond_signal(@p^.condvar);
  pthread_mutex_unlock(@p^.mutex);
end;


procedure intRTLEventResetEvent(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  p^.isset:=false;
  pthread_mutex_unlock(@p^.mutex);
end;


procedure intRTLEventWaitFor(AEvent: PRTLEvent);
var p:pintrtlevent;

begin
  p:=pintrtlevent(aevent);
  pthread_mutex_lock(@p^.mutex);
  while not p^.isset do pthread_cond_wait(@p^.condvar, @p^.mutex);
  p^.isset:=false;
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
    pthread_mutex_lock(@p^.mutex);
    while (not p^.isset) and
          (errres <> ESysETIMEDOUT) do
      begin
        errres:=pthread_cond_timedwait(@p^.condvar, @p^.mutex, @timespec);
      end;
    p^.isset:=false;
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
    rtleventWaitForTimeout :=@intrtleventWaitForTimeout;
    rtleventWaitFor        :=@intrtleventWaitFor;
    // semaphores
    SemaphoreInit          :=@cSemaphoreInit;
    SemaphoreDestroy       :=@cSemaphoreDestroy;
    SemaphoreWait          :=@cSemaphoreWait;
    SemaphorePost          :=@cSemaphorePost;
  end;
  SetThreadManager(CThreadManager);
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
