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
unit systhrds;
interface

{$S-}

{$ifndef BSD}
 {$linklib c}
 {$linklib pthread}
{$else}
 // Link reentrant libc with pthreads
 {$linklib c_r}
{$endif}

  type
     PRTLCriticalSection = ^TRTLCriticalSection;
     TRTLCriticalSection = record
          m_spinlock : longint;
          m_count : longint;
          m_owner : pointer {pthread_t};
          m_kind : longint;
          m_waiting : record
            head,tail : pointer;
          end; {_pthread_queue}
       end;

{ Include generic thread interface }
{$i threadh.inc}


implementation

{*****************************************************************************
                             Generic overloaded 
*****************************************************************************}

{ Include generic overloaded routines }
{$i thread.inc}


{$ifndef BSD}
{$i pthread.inc}
{$else}
{$i ptypes.inc}

CONST PTHREAD_EXPLICIT_SCHED       = 0;
      PTHREAD_CREATE_DETACHED      = 1;
      PTHREAD_SCOPE_PROCESS	   = 0;

 TYPE
    pthread_t       = pointer;
    ppthread_t      = ^pthread_t;
    pthread_key_t   = cint;
    ppthread_key_t  = ^pthread_key_t;
    pthread_mutex_t = pointer;
    ppthread_mutex_t= ^pthread_mutex_t;
    pthread_attr_t  = pointer; // opague
    ppthread_attr_t = ^pthread_attr_t; // opague
    __destr_func_t  = procedure (p :pointer);cdecl;
    __startroutine_t= function (p :pointer):pointer;cdecl;
    pthread_mutex_attr_t  = pointer;
    ppthread_mutex_attr_t = ^pthread_mutex_t;

function  pthread_getspecific	     (t : pthread_key_t):pointer; cdecl; external;
function  pthread_setspecific	     (t : pthread_key_t;p:pointer):cint; cdecl; external;
function  pthread_key_create 	     (p : ppthread_key_t;f: __destr_func_t):cint; cdecl;external;
function  pthread_attr_init           (p : ppthread_key_t):cint; cdecl; external;
function  pthread_attr_setinheritsched(p : ppthread_attr_t;i:cint):cint; cdecl; external;
function  pthread_attr_setscope	     (p : ppthread_attr_t;i:cint):cint;cdecl;external;
function  pthread_attr_setdetachstate (p : ppthread_attr_t;i:cint):cint;cdecl;external;
function  pthread_create ( p: ppthread_t;attr : ppthread_attr_t;f:__startroutine_t;arg:pointer):cint;cdecl;external;
procedure pthread_exit  ( p: pointer); cdecl;external;
function  pthread_mutex_init (p:ppthread_mutex_t;o:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_mutex_destroy (p:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_mutex_lock    (p:ppthread_mutex_attr_t):cint; cdecl;external;
function  pthread_mutex_unlock  (p:ppthread_mutex_attr_t):cint; cdecl;external;

{$endif}

{*****************************************************************************
                       System dependent memory allocation
*****************************************************************************}

{$ifndef BSD}

const
  syscall_nr_mmap                        = 90;
  syscall_nr_munmap                      = 91;

  { Constansts for MMAP }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$20;

type
  SysCallRegs=record
    reg1,reg2,reg3,reg4,reg5,reg6 : longint;
  end;

var
  Errno : longint;

{ Include syscall itself }
{$i syscall.inc}

Function Sys_mmap(adr,len,prot,flags,fdes,off:longint):longint;
type
  tmmapargs=packed record
    address : longint;
    size    : longint;
    prot    : longint;
    flags   : longint;
    fd      : longint;
    offset  : longint;
  end;
var
  t : syscallregs;
  mmapargs : tmmapargs;
begin
  mmapargs.address:=adr;
  mmapargs.size:=len;
  mmapargs.prot:=prot;
  mmapargs.flags:=flags;
  mmapargs.fd:=fdes;
  mmapargs.offset:=off;
  t.reg2:=longint(@mmapargs);
  Sys_mmap:=syscall(syscall_nr_mmap,t);
end;

Function Sys_munmap(adr,len:longint):longint;
var
  t : syscallregs;
begin
  t.reg2:=adr;
  t.reg3:=len;
  Sys_munmap:=syscall(syscall_nr_munmap,t);
end;

{$else}
CONST
  { Constansts for MMAP. These are still private for *BSD }
  MAP_PRIVATE   =2;
  MAP_ANONYMOUS =$1000;

  // include some non posix internal types.
  {$i bsdtypes.inc}
  // *BSD POSIX. Include headers to syscalls.
  {$I bsdsysch.inc}

{$endif}

{*****************************************************************************
                             Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}
    const
      threadvarblocksize : dword = 0;

    var
      TLSKey : pthread_key_t;

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
        DataIndex:=Pointer(Sys_mmap(0,threadvarblocksize,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0));
        FillChar(DataIndex^,threadvarblocksize,0);
        pthread_setspecific(tlskey,dataindex);
      end;


    procedure SysReleaseThreadVars;
      begin
        Sys_munmap(longint(pthread_getspecific(tlskey)),threadvarblocksize);
      end;

{ Include OS independent Threadvar initialization }
{$i threadvar.inc}


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
      begin
{$ifdef HASTHREADVAR}
        { Allocate local thread vars, this must be the first thing,
          because the exception management and io depends on threadvars }
        SysAllocateThreadVars;
{$endif HASTHREADVAR}
        { Copy parameter to local data }
{$ifdef DEBUG_MT}
        writeln('New thread started, initialising ...');
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
      end;


    function BeginThread(sa : Pointer;stacksize : dword;
                         ThreadFunction : tthreadfunc;p : pointer;
                         creationFlags : dword; var ThreadId : DWord) : DWord;
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
        pthread_attr_setscope(@thread_attr, PTHREAD_SCOPE_PROCESS);
        pthread_attr_setdetachstate(@thread_attr, PTHREAD_CREATE_DETACHED);
        pthread_create(@threadid, @thread_attr, @ThreadMain,ti);
        BeginThread:=threadid;
      end;
      
      
    procedure EndThread(ExitCode : DWord);
      begin
        DoneThread;
        pthread_exit(pointer(ExitCode));
      end;


{*****************************************************************************
                          Delphi/Win32 compatibility
*****************************************************************************}

    procedure InitCriticalSection(var CS:TRTLCriticalSection);
      begin
         cs.m_spinlock:=0;
         cs.m_count:=0;
         cs.m_owner:=0;
         cs.m_kind:=1;
         cs.m_waiting.head:=0;
         cs.m_waiting.tail:=0;
         pthread_mutex_init(@CS,NIL);
      end;

    procedure EnterCriticalSection(var CS:TRTLCriticalSection);
      begin
         pthread_mutex_lock(@CS);
      end;

    procedure LeaveCriticalSection(var CS:TRTLCriticalSection);
      begin
         pthread_mutex_unlock(@CS);
      end;

    procedure DoneCriticalSection(var CS:TRTLCriticalSection);
      begin
         pthread_mutex_destroy(@CS);
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


initialization
  InitHeapMutexes;
end.
{
  $Log$
  Revision 1.4  2002-10-26 18:27:52  marco
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
  