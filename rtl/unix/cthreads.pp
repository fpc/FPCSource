{
    $Id$
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

unit cthreads;
interface
{$S-}

{$ifndef dynpthreads}	// If you have problems compiling this on FreeBSD 5.x
 {$linklib c}		// try adding -Xf
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
  sysutils
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
       end;

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}
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

        offset:=threadvarblocksize;

        inc(threadvarblocksize,size);
      end;

    function CRelocateThreadvar(offset : dword) : pointer;
      begin
        CRelocateThreadvar:=pthread_getspecific(tlskey)+Offset;
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


    procedure CReleaseThreadVars;
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


    function CBeginThread(sa : Pointer;stacksize : dword;
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
        CBeginThread:=threadid;
{$ifdef DEBUG_MT}
        writeln('BeginThread returning ',CBeginThread);
{$endif DEBUG_MT}
      end;


    procedure CEndThread(ExitCode : DWord);
      begin
        DoneThread;
        pthread_detach(pthread_t(pthread_self()));
        pthread_exit(pointer(ptrint(ExitCode)));
      end;


{$warning threadhandle can be larger than a dword}
    function  CSuspendThread (threadHandle : dword) : dword;
    begin
      {$Warning SuspendThread needs to be implemented}
    end;

{$warning threadhandle can be larger than a dword}
    function  CResumeThread  (threadHandle : dword) : dword;
    begin
      {$Warning ResumeThread needs to be implemented}
    end;

    procedure CThreadSwitch;  {give time to other threads}
    begin
      {extern int pthread_yield (void) __THROW;}
      {$Warning ThreadSwitch needs to be implemented}
    end;

{$warning threadhandle can be larger than a dword}
    function  CKillThread (threadHandle : dword) : dword;
    begin
      pthread_detach(pthread_t(threadHandle));
      CKillThread := pthread_cancel(pthread_t(threadHandle));
    end;

{$warning threadhandle can be larger than a dword}
    function  CWaitForThreadTerminate (threadHandle : dword; TimeoutMs : longint) : dword;  {0=no timeout}
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
    function  CThreadSetPriority (threadHandle : dword; Prio: longint): boolean; {-15..+15, 0=normal}
    begin
      {$Warning ThreadSetPriority needs to be implemented}
    end;


{$warning threadhandle can be larger than a dword}
    function  CThreadGetPriority (threadHandle : dword): Integer;
    begin
      {$Warning ThreadGetPriority needs to be implemented}
    end;

{$warning threadhandle can be larger than a dword}
    function  CGetCurrentThreadId : dword;
    begin
      CGetCurrentThreadId:=dword(pthread_self());
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
         if pthread_mutex_destroy(@CS) <> 0 then
           runerror(6);
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

function intRTLEventCreate: PRTLEvent;

var p:pintrtlevent;

begin
  new(p);
  pthread_cond_init(@p^.condvar, nil);
  pthread_mutex_init(@p^.mutex, nil);
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
  pthread_cond_signal(@p^.condvar);
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
  pthread_cond_wait(@p^.condvar, @p^.mutex);
  pthread_mutex_unlock(@p^.mutex);
end;

type tthreadmethod = procedure of object;



var
  { event that happens when gui thread is done executing the method}
  ExecuteEvent: PRtlEvent;
  { guard for synchronization variables }
  SynchronizeCritSect: TRtlCriticalSection;
  { method to execute }
  SynchronizeMethod: TThreadMethod;
  { caught exception in gui thread, to be raised in calling thread }
  SynchronizeException: Exception;

procedure CheckSynchronize;
  { assumes being called from GUI thread }
begin
  if SynchronizeMethod = nil then
    exit;
  try
    SynchronizeMethod;
  except
    SynchronizeException := Exception(AcquireExceptionObject);
  end;
  RtlEventSetEvent(ExecuteEvent);
end;

procedure intRTLEventsync(thrdmethd: tmethod;synchronizemethodproc:TProcedure);

var LocalSyncException : Exception;

begin
  EnterCriticalSection(SynchronizeCritSect);
  SynchronizeMethod := tthreadmethod(thrdmethd);
  SynchronizeException := nil;
  RtlEventStartWait(ExecuteEvent);
  SynchronizeMethodProc;
  // wait infinitely
  RtlEventWaitFor(ExecuteEvent);
  SynchronizeMethod := nil;
  LocalSyncException  := SynchronizeException;
  LeaveCriticalSection(SynchronizeCritSect);
  if LocalSyncException <> nil then
    raise LocalSyncException;
end;

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
  ThreadID := SizeUInt (pthread_self);
{$ifdef DEBUG_MT}
  Writeln('InitThreads : ',Result);
{$endif DEBUG_MT}
 {$ifndef ver1_0}
    InitCriticalSection(SynchronizeCritSect);
    ExecuteEvent := RtlEventCreate;
    SynchronizeMethod := nil;
  {$endif}
end;

Function CDoneThreads : Boolean;

begin
  {$ifndef ver1_0}
    DoneCriticalSection(SynchronizeCritSect);  
    RtlEventDestroy(ExecuteEvent);
  {$endif}
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
  With CThreadManager do
    begin
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
{$ifdef hasthreadvar}
    InitThreadVar          :=@CInitThreadVar;
    RelocateThreadVar      :=@CRelocateThreadVar;
    AllocateThreadVars     :=@CAllocateThreadVars;
    ReleaseThreadVars      :=@CReleaseThreadVars;
{$endif}
    BasicEventCreate       :=@intBasicEventCreate;       
    BasicEventDestroy      :=@intBasicEventDestroy;
    BasicEventResetEvent   :=@intBasicEventResetEvent;
    BasicEventSetEvent     :=@intBasicEventSetEvent;
    BasiceventWaitFor      :=@intBasiceventWaitFor;
    rtlEventCreate         :=@intrtlEventCreate;       
    rtlEventDestroy        :=@intrtlEventDestroy;
    rtlEventSetEvent       :=@intrtlEventSetEvent;
    rtlEventStartWait      :=@intrtlEventStartWait;
    rtleventWaitFor        :=@intrtleventWaitFor;
    rtleventsync           :=trtleventsynchandler(@intrtleventsync);
    rtlchksyncunix	   :=@checksynchronize;
    end;
  SetThreadManager(CThreadManager);
  InitHeapMutexes;
end;


initialization
  SetCThreadManager;
finalization
end.
{
  $Log$
  Revision 1.20  2005-02-06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

  Revision 1.19  2004/12/28 14:20:03  marco
   * tthread patch from neli

  Revision 1.18  2004/12/27 15:28:40  marco
   * checksynchronize now in interface win32 uses the default impl.
       unix uses systhrds, rest empty implementation.

  Revision 1.17  2004/12/23 20:20:30  michael
  + Fixed tmt1 test bug

  Revision 1.16  2004/12/23 15:08:59  marco
   * 2nd synchronize attempt. cthreads<->systhrds difference was not ok, but
     only showed on make install should be fixed now.

  Revision 1.15  2004/12/22 21:29:24  marco
   * rtlevent kraam. Checked (compile): Linux, FreeBSD, Darwin, Windows
  	Check work: ask Neli.

  Revision 1.14  2004/12/12 14:30:27  peter
    * x86_64 updates

  Revision 1.13  2004/10/14 17:39:33  florian
    + added system.align
    + threadvars are now aligned

  Revision 1.12  2004/09/09 20:29:06  jonas
    * fixed definition of pthread_mutex_t for non-linux targets (and for
      linux as well, actually).
    * base libpthread definitions are now in ptypes.inc, included in unixtype
      They sometimes have an extra underscore in front of their name, in
      case they were also exported by the packages/base/pthreads unit, so
      they can keep their original name there
    * cthreadds unit now imports systuils, because it uses exceptions (it
      already did so before as well)
    * fixed many linux definitions of libpthread functions in pthrlinux.inc
      (integer -> cint etc)
    + added culonglong type to ctype.inc

  Revision 1.11  2004/05/23 15:30:42  marco
   * basicevent, still untested.

  Revision 1.10  2004/03/03 22:00:28  peter
    * $ifdef debug code

  Revision 1.9  2004/02/22 16:48:39  florian
    * several 64 bit issues fixed

  Revision 1.8  2004/02/15 16:33:32  marco
   * linklibs fixed for new pthread mechanism on FreeBSD

  Revision 1.7  2004/01/20 23:13:53  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.6  2004/01/07 17:40:56  jonas
    * Darwin does not have a lib_r, libc itself is already reentrant

  Revision 1.5  2003/12/16 09:43:04  daniel
    * Use of 0 instead of nil fixed

  Revision 1.4  2003/11/29 17:34:14  michael
  + Removed dummy variable from SetCthreadManager

  Revision 1.3  2003/11/27 20:24:53  michael
  + Compiles on BSD too now

  Revision 1.2  2003/11/27 20:16:59  michael
  + Make works with 1.0.10 too

  Revision 1.1  2003/11/26 20:10:59  michael
  + New threadmanager implementation

  Revision 1.20  2003/11/19 10:54:32  marco
   * some simple restructures

  Revision 1.19  2003/11/18 22:36:12  marco
   * Last patch was ok, problem was somewhere else. Moved *BSD part of pthreads to freebsd/pthreads.inc

  Revision 1.18  2003/11/18 22:35:09  marco
   * Last patch was ok, problem was somewhere else. Moved *BSD part of pthreads to freebsd/pthreads.inc

  Revision 1.17  2003/11/17 10:05:51  marco
   * threads for FreeBSD. Not working tho

  Revision 1.16  2003/11/17 08:27:50  marco
   * pthreads based ttread from Johannes Berg

  Revision 1.15  2003/10/01 21:00:09  peter
    * GetCurrentThreadHandle renamed to GetCurrentThreadId

  Revision 1.14  2003/10/01 20:53:08  peter
    * GetCurrentThreadId implemented

  Revision 1.13  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.12  2003/09/16 13:17:03  marco
   * Wat cleanup, ouwe syscalls nu via baseunix e.d.

  Revision 1.11  2003/09/16 13:00:02  marco
   * small BSD gotcha removed (typing mmap params)

  Revision 1.10  2003/09/15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.9  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.8  2003/03/27 17:14:27  armin
  * more platform independent thread routines, needs to be implemented for unix

  Revision 1.7  2003/01/05 19:11:32  marco
   * small changes originating from introduction of Baseunix to FreeBSD

  Revision 1.6  2002/11/11 21:41:06  marco
   * syscall.inc -> syscallo.inc

  Revision 1.5  2002/10/31 13:45:21  carl
    * threadvar.inc -> threadvr.inc

  Revision 1.4  2002/10/26 18:27:52  marco
   * First series POSIX calls commits. Including getcwd.

  Revision 1.3  2002/10/18 18:05:06  marco
   * $I pthread.inc instead of pthreads.inc

  Revision 1.2  2002/10/18 12:19:59  marco
   * Fixes to get the generic *BSD RTL compiling again + fixes for thread
     support. Still problems left in fexpand. (inoutres?) Therefore fixed
     sysposix not yet commited

  Revision 1.1  2002/10/16 06:22:56  michael
  Threads renamed from threads to systhrds

  Revision 1.1  2002/10/14 19:39:17  peter
    * threads unit added for thread support

}

